
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(councilR) # remotes::install_github("Metropolitan-Council/councilR", force = T)
library(DT)
library(gh)


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

copies_by_variant <- read.csv("data/copies_by_variant.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 2))

omi_ratio_data <- read.csv("data/omi_ratio_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 3))

font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

whiteSmoke <- "#F5F5F5"

pal <- c("#84BB25", "#1D94B7", "#6D3571", "#D64776", "#FBC740", "#092D4E", "#666666")
pal <- setNames(pal, c("Alpha, Beta & Gamma", "Delta", "Omicron BA.1", "Omicron BA.2 (Excluding BA.2.12.1)", "Omicron BA.2.12.1", "Omicron BA.4 and BA.5", "Total load"))

ann_list <- list(
  text = paste(
    "<br><br>",
    "<i>", "Last sample date",
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
  y = -0.2,
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

variant_prev_title <- list(
  x = -0.05,
  y = 1.15,
  text = "Viral load in wastewater<br>M copies/person/day",
  xref = "paper",
  yref = "paper",
  showarrow = F,
  align = "left",
  font = list(
    size = 14,
    family = font_family_list,
    color = councilR::colors$suppBlack
  )
)
date_anno2 <- list(
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
  y = -0.35,
  showarrow = F,
  xref = "paper", yref = "paper",
  xanchor = "right", yanchor = "auto",
  xshift = 0, yshift = -25
)

# browser()
loadxvariantplot <-
  plot_ly(height = 500, width = 900) %>%
  # total load:
  add_trace(
    data = load_data,
    type = "scatter",
    mode = "lines",
    x = ~date,
    fill = "tozeroy",
    y = ~copies_day_person_7day,
    alpha = 0.25,
    line = list(width = 0.5, color = colors$suppGray),
    color = ~"Total load",
    colors = pal,
    name = "Total load",
    hoverinfo = "text",
    text = ~hover_text_load_7day
  ) %>%
  add_trace(
    data = copies_by_variant,
    type = "scatter",
    mode = "lines",
    x = ~date,
    fill = "tozeroy",
    y = ~copies_7day,
    split = ~variant,
    color = ~variant,
    alpha = 0.25,
    colors = pal,
    hoverinfo = "none"
  ) %>%
  add_trace(
    data = copies_by_variant,
    type = "scatter",
    mode = "markers",
    x = ~date,
    y = ~copies,
    split = ~variant,
    color = ~variant,
    alpha = 0.8,
    colors = pal,
    hoverinfo = "text",
    text = ~hover_text_variant,
    showlegend = F
  ) %>%
  layout(
    annotations = list(date_anno2, variant_prev_title),
    hovermode = "closest",
    hoverdistance = "10",
    hoverlabel = hov_lab_list,
    margin = list(
      l = 50,
      r = 100,
      b = 115,
      t = 50,
      pad = 10
    ),
    xaxis = list(
      title = list(
        text = "",
        standoff = 25,
        font = list(
          size = 14,
          family = font_family_list,
          color = councilR::colors$suppBlack
        )
      ),
      zerolinewidth = 2,
      zeroline = TRUE,
      showline = FALSE,
      showgrid = FALSE,
      tickfont = list(
        size = 12,
        family = font_family_list,
        color = councilR::colors$suppBlack
      )
    ),
    yaxis = list(
      title = list(
        # text = "<b>Viral load in wastewater,</b> M copies/person/day",
        standoff = 25,
        font = list(
          size = 14,
          family = font_family_list,
          color = councilR::colors$suppBlack
        )
      ),
      tickfont = list(
        size = 12,
        family = font_family_list,
        color = councilR::colors$suppBlack
      ),
      rangemode = "nonnegative",
      gridcolor = "gray90",
      zerolinecolor = "gray50",
      zerolinewidth = 2
    ),
    legend = list(
      orientation = "h",
      y = -0.2,
      xanchor = "left",
      font = list(
        size = 12,
        family = font_family_list,
        color = councilR::colors$suppBlack
      )
    )
  ) %>%
  config(displayModeBar = FALSE)
