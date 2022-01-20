library(readxl)
library(janitor)
library(tidyverse)

# read in raw -----
header1 <- read.csv("data/raw-variant-data.csv") %>% names()
header2 <- read.csv("data/raw-variant-data.csv", skip = 1) %>% names()
header <- paste0(header1, header2)

header1 <- read_excel("data/raw-variant-data.xlsx") %>% 
  janitor::clean_names() %>% 
  names()
header2 <- read_excel("data/raw-variant-data.xlsx", skip = 1) %>% 
  janitor::clean_names() %>% 
  names()
header <- paste0(header1, header2)

raw_variant_data <-
  read_excel(
    "data/raw-variant-data.xlsx",
    skip = 1
  ) %>%
  set_names(header) %>%
  # get rid of trailing numbers in header: 
  rename_all(~gsub("_[[:digit:]]$|_[[:digit:]][[:digit:]]$", "", .)) %>% 
  rename_all(~gsub("[[:digit:]]sample|[[:digit:]][[:digit:]]sample", "sample", .)) %>% 
  rename_all(~gsub("allele_[[:digit:]]|allele_[[:digit:]][[:digit:]]", "allele", .)) %>% 
  rename_all(~gsub("[[:digit:]]frequency|[[:digit:]][[:digit:]]frequency", "frequency", .)) %>% 
  rename(date = n501y_sample_start_date) %>%
  select(-contains("sample_start_date"))

variant_samples <- raw_variant_data %>%
  select(date, contains("sample")) %>%
  pivot_longer(contains("sample"),
               names_to = "mutation", values_to = "sample_id"
  ) %>%
  mutate(mutation = str_remove(mutation, "_sample"))


variant_frequencies <- raw_variant_data %>%
  select(date, contains("frequency")) %>%
  pivot_longer(contains("frequency"),
               names_to = "mutation", values_to = "frequency"
  ) %>%
  mutate(mutation = str_remove(mutation, "_frequency_of_mutant_allele"))


variant_data <- full_join(variant_samples, variant_frequencies)%>%
  filter(!is.na(date))


# reshape-----
variant_data_new <-
  variant_data %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  # average by date
  group_by(date, mutation) %>%
  summarize(frequency = mean(frequency, na.rm = T)) %>%
  ungroup() %>%
  mutate(variant = case_when(
    mutation == "n501y" ~ "Alpha, Beta & Gamma",
    mutation == "l452r" ~ "Delta",
    mutation == "k417n" & date >= "2021-11-18" ~ "Omicron",
    TRUE ~ "Other"
  )) %>%
  mutate(mutation = toupper(mutation)) %>%
  # rolling 7 day average, by variant type
  complete(nesting(mutation, variant),
    date = seq.Date(min(date, na.rm = T), max(date, na.rm = T), by = "days")
  ) %>%
  group_by(mutation, variant) %>%
  # interpolate missing values up to 3 days:
  mutate(frequency_gapfill = zoo::na.approx(frequency, maxgap = 2, na.rm = F)) %>%
  # now getting a rolling average with a 7-day window:
  mutate(frequency_7day = zoo::rollapply(frequency_gapfill, 7, align = "center", mean, na.rm = T, partial = T, fill = NA)) %>%
  ungroup() %>%
  arrange(date, mutation, variant) %>%
  filter(!variant == "Other") %>%
  mutate(hover_text_variant = paste0(
    format(date, "%b %d, %Y"), "<br>",
    "<b>", variant, "</b> ", round(frequency * 100, digits = 2), "%"
  )) %>%
  mutate(across(where(is.numeric), round, digits = 6)) 

write.csv(variant_data_new, "data/clean_variant_data.csv", row.names = F)
write.csv(variant_data_new, "metc-wastewater-covid-monitor/data/clean_variant_data.csv", row.names = F)
