# globals

# toolbox:
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(councilR)
library(DT)

options(
  launch.browser = TRUE,
  scipen = 99999,
  datatable.print.rownames = FALSE
)
# data:
combined_data <- read.csv("combined_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(weekof = lubridate::floor_date(date, unit = "week", week_start = 7)) %>%
  select(weekof, covid_cases_7day, copies_day_person_M_mn) %>%
  group_by(weekof) %>%
  summarize(across(c(covid_cases_7day, copies_day_person_M_mn), ~ mean(., na.rm = T)))

load_data <- read.csv("clean_load_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(is.numeric, round, digits = 4))

variant_data <- read.csv("clean_variant_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(is.numeric, round, digits = 2))

case_data <- read.csv("case_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(is.numeric, round, digits = 4))


font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

whiteSmoke <- "#F5F5F5"

pal <- c(colors$cdGreen, colors$esBlue, colors$metrostatsDaPurp)
pal <- setNames(pal, c("Omicron", "Delta", "Alpha, Beta & Gamma"))



ann_list <- list(
  text = paste(
    "<br><br>",
    "<i>", "Data last updated",
    max(c(
      load_data$date,
      variant_data$date,
      case_data$date
    ),na.rm = T),
    "</i>"
  ),
  font = list(
    size = 11,
    family = font_family_list,
    color = councilR::colors$suppBlack
  ),
  x = 1,
  y = -0.13,
  showarrow = F,
  xref = "paper", yref = "paper",
  xanchor = "right", yanchor = "auto",
  xshift = 0, yshift = -25
)

hov_lab_list <- list(
  font = list(
    size = 20,
    family = font_family_list,
    color = councilR::colors$suppWhite
  ),
  # bgcolor = "white",
  stroke = list(
    councilR::colors$suppGray,
    councilR::colors$suppGray,
    councilR::colors$suppGray,
    councilR::colors$suppGray
  ),
  padding = list(l = 5, r = 5, b = 5, t = 5)
)
