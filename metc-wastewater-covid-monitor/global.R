# globals

# toolbox:
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(councilR)
library(DT)
library(gh)

options(
  launch.browser = TRUE,
  scipen = 99999,
  datatable.print.rownames = FALSE
)
# data:
combined_data <- read.csv("data/combined_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(weekof = lubridate::floor_date(date, unit = "week", week_start = 7)) %>%
  select(weekof, covid_cases_7day, copies_day_person_M_mn) %>%
  group_by(weekof) %>%
  summarize(across(c(covid_cases_7day, copies_day_person_M_mn), ~ mean(., na.rm = T)))
#
# ggplot(data = combined_data,
#        mapping = aes(
#          x = covid_cases_7day,
#            y = copies_day_person_M_mn)) +
# stat_smooth(n = 95,
#             method = "lm", aes(outfit = fit<<-..y..))
# fit
#
#
# ggplotly(cases_vs_load_plot, layerData = 2, originalData = FALSE) %>% plotly_data()


# combined_data <- combined_data %>%
#   mutate(predicted_copies = fit,
#          hover_text_predict = paste0("Week starting ", weekof)) %>%
#                                      # , "<br>",
# "<b>", round(predicted_copies), "</b>",
# " predicted copies/day/person")) %>%
# arrange(predicted_copies)

case_data <- read.csv("data/case_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 4))

load_data <- read.csv("data/clean_load_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  left_join(case_data) %>%
  mutate(across(where(is.numeric), round, digits = 4))

variant_data <- read.csv("data/clean_variant_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 2))




font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

whiteSmoke <- "#F5F5F5"

pal <- c("#84BB25", "#1D94B7", "#6D3571")
pal <- setNames(pal, c("Omicron", "Delta", "Alpha, Beta & Gamma"))



ann_list <- list(
  text = paste(
    "<br><br>",
    "<i>", "Data last updated",
    max(c(
      load_data$date,
      variant_data$date
      # case_data$date,
      # combined_data$date
    ), na.rm = T),
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
