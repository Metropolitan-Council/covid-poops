# d_covid_cases

library(readxl)
library(janitor)
library(tidyverse)

raw_case_data <- read.csv("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv?_ga=2.86006619.233414847.1642517751-2016304881.1642174657")

case_data <- raw_case_data %>%
  janitor::clean_names() %>%
  filter(county_name %in% c("Hennepin County ", "Ramsey County ", "Scott County ", "Dakota County ", "Washington County ", "Anoka County ", "Carver County ") & state == "MN") %>%
  select(-county_fips, -state, -state_fips) %>%
  pivot_longer(cols = contains("x"), names_to = "date", values_to = "covid_cases_total") %>%
  mutate(date = str_remove(date, "x")) %>%
  mutate(date = as.Date(date, format = "%Y_%m_%d")) %>%
  group_by(date) %>%
  summarize(covid_cases_total = sum(covid_cases_total)) %>%
  mutate(covid_cases_new = covid_cases_total - lag(covid_cases_total, 1)) %>%
  mutate(covid_cases_per100K = 100000 * covid_cases_new / (3163104)) %>%
  # mutate(frequency_gapfill = zoo::na.approx(frequency, maxgap = 2, na.rm = F)) %>%
  # now getting a rolling average with a 7-day window:
  mutate(covid_cases_7day = zoo::rollapply(covid_cases_per100K, 7, align = "right", mean, na.rm = T, partial = T, fill = NA)) %>%
  mutate(hover_text_case = paste0(
    format(date, "%b %d, %Y"), "<br>",
    "<b>", round(covid_cases_7day, 2), "</b> cases per 100,000 people"
  ))

write.csv(case_data, "metc-wastewater-covid-monitor/data/case_data.csv", row.names = F)
