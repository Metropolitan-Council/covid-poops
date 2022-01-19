library(readxl)
library(janitor)
library(tidyverse)

# read in raw -----
header1 <- read.csv("data/raw-variant-data.csv") %>% names()
header2 <- read.csv("data/raw-variant-data.csv", skip = 1) %>% names()
header <- paste0(header1, header2)

raw_variant_data <- read.csv(
  "data/raw-variant-data.csv",
  skip = 1
) %>%
  set_names(header) %>%
  janitor::clean_names() %>%
  select(-l452r_sample_start_date_1, -k417n_sample_start_date_2) %>%
  rename(
    l452r_sample_id = l452r_1sample,
    k417n_sample_id = k417n_1sample_1,
    n501y_sample_id = n501y_1sample_id
  ) %>%
  rename(
    n501y_frequency = n501y_2frequency_of_mutant_allele,
    l452r_frequency = l452r_2frequency_of_mutant_allele_1,
    k417n_frequency = k417n_2frequency_of_mutant_allele_2
  ) %>%
  rename(date = n501y_sample_start_date)

variant_samples <- raw_variant_data %>%
  select(date, contains("sample_id")) %>%
  pivot_longer(contains("sample_id"),
               names_to = "mutation", values_to = "sample_id"
  ) %>%
  mutate(mutation = str_remove(mutation, "_sample_id"))


variant_frequencies <- raw_variant_data %>%
  select(date, contains("frequency")) %>%
  pivot_longer(contains("frequency"),
               names_to = "mutation", values_to = "frequency"
  ) %>%
  mutate(mutation = str_remove(mutation, "_frequency"))


variant_data <- full_join(variant_samples, variant_frequencies)


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
    "<b>", variant, "</b> ", round(frequency_7day * 100, digits = 2), "%"
  )) %>% 
  mutate(across(where(is.numeric), round, digits = 6))

write.csv(variant_data_new, "data/clean_variant_data.csv", row.names = F)
write.csv(variant_data_new, "metc-wastewater-covid-monitor/data/clean_variant_data.csv", row.names = F)
