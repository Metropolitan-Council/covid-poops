# d_covid_cases

library(readxl)
library(janitor)
library(tidyverse)

# Find Sharepoint Directory:
source("R/sharepointfilepath.R")

raw_case_data <- read_csv(paste0(sharepath, "/BLU EMP SEN - SARS-CoV-2/raw_zip_code_data.csv")) %>%
  janitor::clean_names() %>%
  rename(date = 1, zip = 2) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

zip_code_multipliers <- read_excel(file.path("data/reference_tables.xlsx"),
  sheet = paste0("Zipcode Multipliers"),
  col_types = c("numeric", "text", "numeric")
) %>%
  janitor::clean_names() %>%
  rename(zip = 1, percent_coverage = 3) %>%
  group_by(zip) %>%
  summarize(percent_coverage = max(percent_coverage, na.rm = TRUE))

case_area_pop <- 1951318

case_data <- raw_case_data %>%
  right_join(zip_code_multipliers, by = "zip") %>%
  mutate(covid_cases_new = case_count * percent_coverage) %>%
  group_by(date) %>%
  summarize(covid_cases_new = sum(covid_cases_new, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(covid_cases_per100K = 100000 * covid_cases_new / (case_area_pop)) %>%
  # now getting a rolling average with a 7-day window:
  mutate(covid_cases_7day = zoo::rollapplyr(covid_cases_per100K, 7, mean, fill = c("extend", "extend", "extend"))) %>%
  mutate(hover_text_case = paste0(
    format(date, "%b %d, %Y"), "<br>",
    "<b>", round(covid_cases_7day, 2), "</b> cases per 100,000 people"
  )) %>%
  select(date, covid_cases_new, covid_cases_per100K, covid_cases_7day, hover_text_case) %>%
  filter(!is.na(date))

write.csv(case_data, "metc-wastewater-covid-monitor/data/case_data.csv", row.names = F)
write.csv(case_data, "data/case_data.csv", row.names = F)


write.csv(case_data, file.path(paste0(sharepath, "/1 - Update data/metro-case-data.csv")), row.names = F)
