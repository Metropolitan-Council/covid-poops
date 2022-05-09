# globals

# toolbox:
library(shiny)
# library(tidyverse)
library(dplyr)
library(lubridate)
library(plotly)
library(councilR)
library(DT)
library(gh)
library(sf)


# if you get error message
# `Error building image: unable to satisfy package: councilR (0.1.0.9001)`
# force install councilR from GitHub
# remotes::install_github("Metropolitan-Council/councilR", force = T)

options(
  launch.browser = TRUE,
  scipen = 99999,
  datatable.print.rownames = FALSE
)

case_data <- read.csv("data/case_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 4))

load_data <- read.csv("data/clean_load_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  left_join(case_data, by = "date") %>%
  mutate(across(where(is.numeric), round, digits = 4))

variant_data <- read.csv("data/clean_variant_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 2))

copies_by_variant <- read.csv("data/copies_by_variant.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 2))

sewershed <- readRDS("data/simple_wwtp_sewershed.rds") %>%
  filter(WWTP %in% c("Blue Lake", "Seneca", "Empire", "Metro"))

font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

whiteSmoke <- "#F5F5F5"

pal <- c("#84BB25", "#1D94B7", "#6D3571", "#D64776", "#666666")
pal <- setNames(pal, c(
  "Alpha, Beta & Gamma", "Delta",
  "Omicron BA.1", "Omicron BA.2", "Total load"
))

#Turn on when start detecting BA.2.12.1
#pal <- c("#84BB25", "#1D94B7", "#6D3571", "#D64776", "#92BED2", "#666666")
#pal <- setNames(pal, c(
 # "Alpha, Beta & Gamma", "Delta",
# "Omicron BA.1", "Omicron BA.2", "Omicron BA.2.12.1", "Total load"
#))

ann_list <- list(
  text = paste(
    "<br><br>",
    "<i>", "Latest sample date",
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
  y = -0.19,
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
