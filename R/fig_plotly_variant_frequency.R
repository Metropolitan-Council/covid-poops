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
  mutate(across(where(is.numeric), round, digits = 2)) %>%
  mutate(variant = factor(
    variant,
    levels = c(
      "Alpha, Beta & Gamma",
      "Delta",
      "Omicron BA.1",
      "Omicron BA.2.12.1",
      "Omicron BA.4 and BA.5",
      "Omicron BA.4",
      "Omicron BA.5 (Excluding BQ.1)",
      "Omicron BQ.1",
      "Omicron BA.2.75",
      "XBB",
      "Omicron BA.2 (Excluding BA.2.12.1)"
    )
  )) 

copies_by_variant <- read.csv("data/copies_by_variant.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 2))

omi_ratio_data <- read.csv("data/omi_ratio_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(where(is.numeric), round, digits = 3))

# "Arial Narrow" <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

whiteSmoke <- "#F5F5F5"

pal <- c("Total Viral Load" = "white",
         "XBB" = "#800000",
         "Omicron BA.2.75" = "#E4ADC4",
         "Omicron BQ.1" = "#006400",
         "Omicron BA.5 (Excluding BQ.1)" = "#000080",
         "Omicron BA.4" = "#3D9F93",
         "Omicron BA.4 and BA.5" = "#A9A3FE",
         "Omicron BA.2.12.1" = "#FBC740",
         "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776",
         "Omicron BA.1" = "#6D3571",
         "Delta" = "#1D94B7",
         "Alpha, Beta & Gamma" = "#84BB25"
)

# ann_list <- list(
#   text = paste(
#     "<br><br>",
#     "<i>", "Last sample date",
#     max(c(
#       load_data$date,
#       variant_data$date
#       # case_data$date,
#       # combined_data$date
#     ), na.rm = T),
#     "</i>"
#   ),
#   font = list(
#     size = 16,
#     family = "Arial Narrow",
#     color = councilR::colors$suppBlack
#   ),
#   x = 1,
#   y = -0.2,
#   showarrow = F,
#   xref = "paper", yref = "paper",
#   xanchor = "right", yanchor = "auto",
#   xshift = 0, yshift = -25
# )

hov_lab_list <- list(
  font = list(
    size = 20,
    family = "Arial Narrow",
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



variant_plot <- 
  variant_data %>%
  plot_ly(# set height and width
    width = 670, height = 425) %>%
  add_trace(
    type = "scatter",
    mode = "lines",
    x = ~date,
    fill = "tozeroy",
    y = ~frequency_7day,
    split = ~variant,
    color = ~variant,
    alpha = 0.25,
    colors = pal,
    hoverinfo = "none",
    showlegend = F
  ) %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    x = ~date,
    y = ~frequency,
    split = ~variant,
    color = ~variant,
    alpha = 0.8,
    colors = pal,
    hoverinfo = "text",
    text = ~hover_text_variant
  ) %>%
  layout(
    hoverlabel = list(
      font = list(
        size = 20,
        family = "Arial Narrow",
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
    ),
    margin = list(t = 50),
    legend = list(
      orientation = "h",
      yanchor = "top",
      xanchor = "left",
      title = list(text = ""),
      font = list(
        size = 16,
        family = "Arial Narrow",
        color = councilR::colors$suppBlack
      )),
    yaxis = list(
      title = list(
        # text = "<b>Frequency of marker genes (%)</b>",
        standoff = 25,
        font = list(
          size = 16,
          family = "Arial Narrow",
          color = councilR::colors$suppBlack
        )
      ),
      tickformat = "1%",
      tickfont = list(
        size = 16,
        family = "Arial Narrow",
        color = councilR::colors$suppBlack
      ),
      gridcolor = "gray90",
      zerolinecolor = "gray50",
      zerolinewidth = 2,
      range = c(0, 1.1)
    ),
    xaxis = list(
      title = list(text = ""),
      zeroline = FALSE,
      showline = FALSE,
      showgrid = FALSE,
      tickfont = list(
        size = 16,
        family = "Arial Narrow",
        color = councilR::colors$suppBlack
      )),
    annotations = list(list(
      x = -0.05,
      y = 1.15,
      text = "Variant frequency (%)<br>indicated by maker genes",
      xref = "paper",
      yref = "paper",
      showarrow = F,
      align = "left",
      font = list(
        size = 16,
        family = "Arial Narrow",
        color = councilR::colors$suppBlack
      )
    ))
  ) %>%
  config(displayModeBar = FALSE)

htmlwidgets::saveWidget(variant_plot, "fig/variant_frequency.html")
