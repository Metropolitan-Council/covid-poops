library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(councilR) # remotes::install_github("Metropolitan-Council/councilR", force = T)
library(DT)
library(gh)
library(readr)

load_data <- read_csv("data/clean_load_data.csv", show_col_types = F)

copies_by_variant <-
  read_csv("data/copies_by_variant.csv", show_col_types = F) %>%
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
      "Omicron BA.2 (Excluding BA.2.12.1)"
    )
  )) %>%
  filter(!(variant == "Omicron BA.4 and BA.5" &
             date > "2022-05-30")) %>%
  filter(date > "2021-04-01")

font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

whiteSmoke <- "#F5F5F5"

pal <- c("Total Viral Load" = "white",
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

ann_list <- list(
  text = paste(
    "<br><br>",
    "<i>", "Last sample date",
    max(c(
      load_data$date,
      copies_by_variant$date
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

variant_prev_title <- list(
  x = -0.05,
  y = 1.15,
  text = "Viral load in wastewater<br>copies_variant/person/day",
  xref = "paper",
  yref = "paper",
  showarrow = F,
  align = "left",
  font = list(
    size = 14,
    family = "Arial Narrow",
    color = councilR::colors$suppBlack
  )
)

date_anno2 <- list(
  text = paste(
    "<br><br>",
    "<i>", "Latest sample date",
    max(c(
      load_data$date,
      copies_by_variant$date
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


base_ggplot <-
  copies_by_variant %>%
  rename(`7-day-average` = hover_text_variant_7day) %>%
  mutate(variant = factor(variant, levels = rev(levels(variant)))) %>%
  ggplot(aes(x = date, y = copies_7day)) +
  geom_area(
    position = "stack",
    aes(
      x = date,
      y = copies_7day,
      color = variant,
      fill = variant,
      label = `7-day-average`
    ),
    alpha = 0.75,
    na.rm = T,
    lwd = 0.25
  )  +
  # solid black line - total covid load
  geom_line(
    data = load_data[load_data$date > min(copies_by_variant$date),] %>%
      mutate(variant = "Total COVID-19 Load") %>%
      rename(`7-day-average` = hover_text_load_7day),
    aes(
      x = date,
      y = copies_day_person_7day,
      color = variant,
      fill = variant,
      label = `7-day-average`
    ),
    lwd = 0.6
  ) +
  scale_fill_manual(values = pal,
                    drop = T) + 
  scale_color_manual(values = pal,
                     drop = T) +
  scale_y_continuous(name = "",
                     labels = scales::unit_format(unit = "M")) +
  scale_x_date(name = "Date",
               breaks = "2 months",
               date_labels = "%b '%y") +
  geom_hline(yintercept = 0, color = "gray30", size = 0.5) + 
  theme_void()+
  theme(panel.grid.major.x= element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(),
        axis.text.y = element_text())

variant_load_plot <- 
ggplotly(base_ggplot, tooltip = c("label"),
         width = 670, height = 425) %>%
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
    margin = list(l = 130, pad = 0),
    legend = list(
      orientation = "h",
      yanchor = "top",
      xanchor = "left",
      title = list(text = ""),
      font = list(
        size = 14,
        family = "Arial Narrow",
        color = councilR::colors$suppBlack
      )),
    yaxis = list(
      title = list(text = ""),
      tickfont = list(
        size = 16,
        family = "Arial Narrow",
        color = councilR::colors$suppBlack
      ),
      rangemode = "nonnegative",
      gridcolor = "gray90",
      zerolinecolor = "gray90",
      zerolinewidth = 0
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
    annotations = list(
      list(
        x = -0.22,
        y = 0.6,
        text = "Viral load<br>(copies<br>per person,<br>per day)",
        xref = "paper",
        yref = "paper",
        showarrow = F,
        align = "center",
        font = list(
          size = 16,
          family = "Arial Narrow",
          color = councilR::colors$suppBlack
        ))
    )
  )

htmlwidgets::saveWidget(variant_load_plot, "fig/variant_load_plot.html")
