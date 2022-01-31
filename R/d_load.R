library(readxl)
library(janitor)
library(tidyverse)

raw_load_data <- read_excel("data/raw-load-data-2.xlsx", 
                              sheet = "time trend", col_types = c("skip", 
                                                                  "text", "date", "skip", "skip", "numeric", 
                                                                  "skip", "numeric", "skip", "skip", 
                                                                  "skip", "skip", "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", "skip", 
                                                                  "numeric", "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", "skip", 
                                                                  "skip", "skip", "skip", 
                                                                  "skip"), skip = 1) %>%
  janitor::clean_names() %>%
  mutate(sample_start_date = as.Date(sample_start_date))



seq_date <- function(x) seq(min(x, na.rm = T), max(x, na.rm = T), by = "day")
all_dates <- seq_date(raw_load_data$sample_start_date)

load_data <-
  raw_load_data %>%
  mutate(flow_l_day = metro_flow_rate_mgd_on_sample_start_date * 3785411.8) %>%
  select(-metro_flow_rate_mgd_on_sample_start_date) %>%
  rename(N1_gene_l = t1_copies_u_l, N2_gene_l = t2_copies_u_l) %>%
  # convert microliter to liter: 
  mutate(N1_gene_l = 1e6 * (N1_gene_l/20), N2_gene_l = 1e6 * (N2_gene_l/20)) %>%
  pivot_longer(
    cols = c("N1_gene_l", "N2_gene_l"), names_to = "gene_num",
    values_to = "n_l"
  ) %>%
  # calculate n per day, per person, per person in millions
  mutate(copies_day = n_l * flow_l_day) %>%
  mutate(copies_day_person = copies_day / 195000) %>%
  mutate(copies_day_person_M = copies_day_person / 1e7) %>%
  # average across runs for a sample average
  group_by(sample_name, sample_start_date, flow_l_day) %>%
  summarize(across(c(copies_day_person_M), ~ mean(., na.rm = T))) %>%
  ungroup() %>%
  # average by date:
  group_by(sample_start_date) %>%
  add_tally(name = "n_samples") %>%
  # average across multiple samples: 
  summarize(
    copies_day_person_M_mn = mean(copies_day_person_M, na.rm = T),
    copies_day_person_M_se = sd(copies_day_person_M, na.rm = T) / (n_samples)
  ) %>%
  ungroup() %>%
  unique() %>%
  rename(date = sample_start_date) %>%
  # (fill in for missing dates)
  complete(date = seq.Date(min(date, na.rm = T), max(date, na.rm = T), by = "days")) %>%
  # add 7-day running average: 
  mutate(copies_day_person_7day = zoo::rollapply(copies_day_person_M_mn, 7, align = "right", mean, na.rm = T, partial = F, fill = 'extend')) %>%
  arrange(date) %>%
  mutate(hover_text_load = paste0(
    format(date, "%b %d, %Y"), "<br>",
    "<b>", round(copies_day_person_M_mn, 2), "M</b> copies per person per day"
  ),
  hover_text_load_7day = paste0(
    "7-day avg. load on ", format(date, "%b %d "), ":<br>",
    "<b>", round(copies_day_person_7day, 1), "M</b> copies per person per day")
  ) %>%
  filter(!is.na(date))

head(load_data)
tail(load_data)


write.csv(load_data, "data/clean_load_data.csv", row.names = F)
write.csv(load_data, "metc-wastewater-covid-monitor/data/clean_load_data.csv", row.names = F)
