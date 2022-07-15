

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

font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

whiteSmoke <- "#F5F5F5"

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

left_axis_text <- list(
  tickfont = list(color = colors$councilBlue,
                  size = 14,
                  family = font_family_list),
  overlaying = "y",
  side = "left",
  zerolinewidth = 0,
  zerolinecolor = colors$suppWhite,
  gridcolor = colors$suppWhite,
  rangemode = "nonnegative"
)

left_axis_title <- list(
  x = -0.05,
  y = 1.05,
  text = "Viral load in wastewater<br>M copies/person/day",
  xref = "paper",
  yref = "paper",
  showarrow = F,
  align = "left",
  font = list(
    size = 14,
    family = font_family_list,
    color = councilR::colors$councilBlue
  )
)

right_axis_title <- list(
  x = 1,
  y = 1.05,
  text = "COVID-19 cases<br>per 100K residents",
  xref = "paper",
  yref = "paper",
  showarrow = F,
  align = "right",
  font = list(
    size = 14,
    family = font_family_list,
    color = councilR::colors$suppBlack
  )
)

date_anno <- list(
  text = paste(
    "<br><br>",
    "<i>", "Latest sample date",
    max(c(
      load_data$date
      # variant_data$date
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
  y = -0.15,
  showarrow = F,
  xref = "paper", yref = "paper",
  xanchor = "right", yanchor = "auto",
  xshift = 0, yshift = -25
)

load_plot <-
  load_data %>%
  # left_join(case_data, by = "date") %>%
  plot_ly(type = "scatter", mode = "lines", height = 500, width = 900) %>%
  add_trace(
    mode = "markers",
    x = ~date,
    y = ~copies_day_person_M_mn,
    name = "Viral load, Metro Plant service area",
    size = 1,
    yaxis = "y2",
    marker = list(
      color = "rgba(0, 84, 164, .5)",
      size = 8,
      line = list(
        color = colors$councilBlue,
        width = 0.5
      )
    ),
    # fillcolor = ,
    # line = list(width = 2, color = colors$esBlue),
    hoverinfo = "text",
    text = ~hover_text_load
  ) %>%
  add_trace(
    mode = "lines",
    x = ~date,
    y = ~copies_day_person_7day,
    name = "7-day avg. viral load",
    size = 1,
    yaxis = "y2",
    # fill = "tozeroy",
    # fillcolor = "rgba(0, 154, 199, .5)",
    line = list(width = 2, color = colors$councilBlue),
    hoverinfo = "text",
    text = ~hover_text_load_7day
  ) %>%
  add_trace(
    x = ~slice(load_data, 1:(n()-7))$date,
    y = ~slice(load_data, 1:(n()-7))$covid_cases_7day,
    name = "7-day avg. cases per capita",
    fill = "tozeroy",
    fillcolor = "rgba(160, 160, 160, .3)",
    line = list(width = 0.5, color = colors$suppGray),
    hoverinfo = "text",
    text = ~slice(load_data, 1:(n()-7))$hover_text_case
  ) %>%
  add_trace(
    x = ~slice(load_data, (n() - 6):n())$date,
    y = ~slice(load_data, (n() - 6):n())$covid_cases_7day,
    name = "7-day avg. cases per capita, Incomplete",
    fill = "tozeroy",
    fillcolor = "rgba(160, 160, 160, .8)",
    line = list(width = 0.5, color = colors$suppGray),
    hoverinfo = "text",
    text = ~slice(load_data, (n() - 6):n())$hover_text_case
  ) %>%
  layout(
    annotations = list(left_axis_title, right_axis_title, date_anno),
    autosize = T,
    annotations = ann_list <- list(
      text = paste(
        "<br><br>",
        "<i>", "Latest sample date",
        max(c(
          load_data$date
          # variant_data$date
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
      y = -0.12,
      showarrow = F,
      xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "auto",
      xshift = 0, yshift = -60
    ),
    showlegend = TRUE,
    margin = list(l = 75, r = 75, b = 75, pad = 10),
    hovermode = "closest",
    hoverdistance = "10",
    hoverlabel = hov_lab_list,
    yaxis2 = left_axis_text,
    xaxis = list(
      title = list(
        range = ~c(min(date)-4, max(date) + 4),
        text = "",
        standoff = 0,
        font = list(
          size = 14,
          family = font_family_list,
          color = councilR::colors$suppBlack
        )
      ),
      zerolinewidth = 2,
      gridcolor = colors$suppWhite,
      zerolinecolor = colors$suppWhite,
      tickfont = list(
        size = 12,
        family = font_family_list,
        color = councilR::colors$suppBlack
      )
    ),
    yaxis = list(
      side = "right",
      title = list(
        standoff = 25,
        font = list(
          size = 16,
          family = font_family_list,
          color = councilR::colors$suppBlack
        )
      ),
      zerolinewidth = 1,
      tickfont = list(
        size = 14,
        family = font_family_list,
        color = councilR::colors$suppBlack
      ),
      gridcolor = colors$suppWhite,
      zerolinecolor = colors$suppWhite
    ),
    legend = list(
      orientation = "h",
      font = list(
        size = 12,
        family = font_family_list,
        color = councilR::colors$suppBlack
      )
    )
  ) %>%
  config(displayModeBar = F) 

htmlwidgets::saveWidget(load_plot, "fig/cases_vs_load.html")
