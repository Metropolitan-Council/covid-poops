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
library(shinyBS)
library(ggplot2)


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
  mutate(across(where(is.numeric), round, digits = 2)) %>%
  filter( !variant %in% c("Omicron BA.4 and BA.5"))

copies_by_variant <- read.csv("data/copies_by_variant.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 2)) %>%
  mutate(variant = factor(
    variant,
    levels = c(
      "Alpha, Beta & Gamma",
      "Delta",
      "Omicron BA.1",
      "Omicron BA.2 (Excluding BA.2.12.1)",
      "Omicron BA.2.12.1",
      "Omicron BA.4 and BA.5",
      "Omicron BA.4",
      "Omicron BA.5"
    )
  )) %>%
  filter(!(variant == "Omicron BA.4 and BA.5" &
             date > "2022-05-30")) %>%
  filter(date > "2021-04-01")

font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

whiteSmoke <- "#F5F5F5"

#pal <- c("#84BB25", "#1D94B7", "#6D3571", "#D64776", "#FBC740", "#A9A3FE", "#3D9F93", "#666666")
#pal <- setNames(pal, c(
#  "Alpha, Beta & Gamma", "Delta",
#  "Omicron BA.1", "Omicron BA.2 (Excluding BA.2.12.1)", "Omicron BA.2.12.1", "Omicron BA.4", "Omicron BA.5", "Total load"
#))

pal <- c("Total Viral Load" = "white",
         "Omicron BA.5" = "#000080",
         "Omicron BA.4" = "#3D9F93",
         "Omicron BA.4 and BA.5" = "#A9A3FE",
         "Omicron BA.2.12.1" = "#FBC740",
         "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776",
         "Omicron BA.1" = "#6D3571",
         "Delta" = "#1D94B7",
         "Alpha, Beta & Gamma" = "#84BB25"
)

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
  xshift = 0, yshift = -55
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
