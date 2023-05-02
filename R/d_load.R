library(readxl)
library(janitor)
library(tidyverse)


# Find Sharepoint Directory:
source("R/sharepointfilepath.R")


# Process R&D Extraction Data:
raw_load_data <- read_excel(file.path(paste0(sharepath, "/1 - Update data/A- Metro data - load and variants.xlsx")),
  sheet = "load",
  col_types = c(
    "text", "skip", "skip",
    "date", "skip", "numeric", "numeric",
    "skip", "skip", "skip", "numeric",
    "skip", "skip", "skip", "skip", "skip",
    "skip", "skip", "skip", "skip", "skip"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  mutate(sample_start_date = as.Date(sample_start_date))

seq_date <- function(x) seq(min(x, na.rm = T), max(x, na.rm = T), by = "day")
all_dates <- seq_date(raw_load_data$sample_start_date)

load_data_bysample <-
  raw_load_data %>%
  mutate(flow_l_day = metro_flow_rate_mgd_on_sample_start_date * 3785411.8) %>%
  select(-metro_flow_rate_mgd_on_sample_start_date) %>%
  rename(N1_gene_l = copies_l_3, N2_gene_l = copies_l_4) %>%
  pivot_longer(
    cols = c("N1_gene_l", "N2_gene_l"), names_to = "gene_num",
    values_to = "n_l"
  ) %>%
  # calculate n per day, per person, per person in millions
  mutate(copies_day = n_l * flow_l_day) %>%
  mutate(copies_day_person = copies_day / 1950000) %>%
  mutate(copies_day_person_M = copies_day_person / 1e6) %>%
  # average across runs for a sample average
  group_by(sample_name, sample_start_date, flow_l_day) %>%
  summarize(across(c(copies_day_person_M), ~ mean(., na.rm = T))) %>%
  ungroup()

load_data <-
  load_data_bysample %>%
  # average by date:
  group_by(sample_start_date) %>%
  add_tally(name = "n_samples") %>%
  # average across multiple samples:
  summarize(
    copies_day_person_M_mn = mean(copies_day_person_M, na.rm = T),
    copies_day_person_M_se = sd(copies_day_person_M, na.rm = T) / sqrt(n_samples)
  ) %>%
  ungroup() %>%
  unique() %>%
  rename(date = sample_start_date) %>%
  # (fill in for missing dates)
  complete(date = seq.Date(min(date, na.rm = T), max(date, na.rm = T), by = "days")) %>%
  # add 7-day running average:
  mutate(copies_day_person_7day = zoo::rollapply(copies_day_person_M_mn, 7, align = "right", mean, na.rm = T, partial = F, fill = "extend")) %>%
  arrange(date) %>%
  mutate(
    hover_text_load = paste0(
      format(date, "%b %d, %Y"), "<br>",
      "<b>", round(copies_day_person_M_mn, 2), "M</b> copies per person per day"
    ),
    hover_text_load_7day = paste0(
      "7-day avg. load on ", format(date, "%b %d "), ":<br>",
      "<b>", round(copies_day_person_7day, 1), "M</b> copies per person per day"
    )
  ) %>%
  filter(!is.na(date))

head(load_data)
tail(load_data)

# Process UMGC Extraction Data:
raw_load_data_umgc <- read_excel(file.path(paste0(sharepath, "/BLU EMP SEN - SARS-CoV-2/UMGC_BLU_EMP_SEN_Data_Report.xlsx")),
                                 sheet = "Load" ,
                                 col_types = c(
                                   "text", "date", "text", "numeric", "numeric", "text"
                                 )) %>%
  janitor::clean_names() %>% 
  mutate( site = substr(sample_id, 1, 3)) %>% 
  filter( ( site %in% c('MET')))

influent_flow <- read.csv(paste0(sharepath, "/1 - Update data/influent_flow_data.csv")) %>%
  janitor::clean_names() %>% 
  rename( sample_start_date = date) %>% 
  mutate(sample_start_date = as.Date(sample_start_date, format = "%m/%d/%Y"))

load_data_formatted_umgc  <- raw_load_data_umgc %>%
  mutate(collection_date = as.Date(collection_date)) %>%
  mutate(sample_start_date = as.Date(collection_date) - 1) %>%
  select(-collection_date, -site) %>% 
  mutate ( wastewater_volume_ml = 3) %>% 
  mutate( elution_volume_ul = 20)  %>% 
  merge(influent_flow, by = c("sample_start_date"))

seq_date <- function(x) seq(min(x, na.rm = T), max(x, na.rm = T), by = "day")
all_dates <- seq_date(raw_load_data$sample_start_date)

load_data_bysample_umgc <-
  load_data_formatted_umgc %>%
  mutate(flow_l_day = metro_influent_flow * 3785411.8) %>%
  select(-metro_influent_flow) %>%
  mutate( copies_per_l = copies_per_u_l / wastewater_volume_ml * elution_volume_ul * 1000 )  %>%
  mutate( copies_per_l_N1 = ifelse( target == "N1", copies_per_l, NA))  %>%
  mutate( copies_per_l_N2 = ifelse( target == "N2", copies_per_l, NA))  %>%
  # calculate n per day, per person, per person in millions
  mutate(copies_day = copies_per_l * flow_l_day) %>%
  mutate(copies_day_person = copies_day / 1950000) %>%
  mutate(copies_day_person_M = copies_day_person / 1e6) %>%
  # average across runs for a sample average
  group_by(sample_id, sample_start_date, flow_l_day) %>%
  summarize(
    across(c(copies_day, copies_day_person_M, copies_per_l, copies_per_l_N1, copies_per_l_N2), ~ mean(., na.rm = T))) %>%
  ungroup()

load_data_umgc <-
  load_data_bysample_umgc %>%
  # average by date:
  group_by(sample_start_date) %>%
  add_tally(name = "n_samples") %>%
  # average across multiple samples:
  summarize(
    copies_day_mn = mean(copies_day, na.rm = T),
    copies_day_person_M_mn = mean(copies_day_person_M, na.rm = T),
    copies_day_person_M_se = sd(copies_day_person_M, na.rm = T) / sqrt(n_samples),
  ) %>%
  ungroup() %>%
  unique() %>%
  rename(date = sample_start_date) %>%
  # (fill in for missing dates)
  complete(date = seq.Date(min(date, na.rm = T), max(date, na.rm = T), by = "days")) %>% 
  # add 7-day running average:
  arrange(date) %>%
  mutate(copies_day_person_7day = zoo::rollapply(copies_day_person_M_mn, 7, align = "right", mean, na.rm = T, partial = F, fill = "extend")) %>%
  mutate(
    hover_text_load = paste0(
      format(date, "%b %d, %Y"), "<br>",
      "<b>", round(copies_day_person_M_mn, 2), "M</b> copies per person per day"
    ),
    hover_text_load_7day = paste0("7-day avg. load on ", format(date, "%b %d "), ":<br>",
                                  "<b>", round(copies_day_person_7day, 1), "M</b> copies per person per day"
    )
  ) %>%
  filter(!is.na(date))


umgc_switch_date = as.Date("2023-04-24")

load_data_umgc_official <- load_data_umgc %>%
                            filter (date >= umgc_switch_date) %>%
                            select( -copies_day_mn)

load_data_combined <- load_data %>%
                      filter( date < umgc_switch_date) %>%
                      rbind(load_data_umgc_official)


write.csv(load_data_combined, "data/clean_load_data.csv", row.names = F)
write.csv(load_data_combined, "metc-wastewater-covid-monitor/data/clean_load_data.csv", row.names = F)
