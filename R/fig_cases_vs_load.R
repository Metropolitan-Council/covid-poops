# Create figures!

library(cowplot)
library(magick)
library(ggtext)
library(showtext)
library(tidyverse)

font_add("HelveticaNeueLTStd", "HelveticaNeueLTStd-Lt.otf")
font_add("Arial Narrow", "ARIALN.ttf")
font_add("Arial Narrow Italic",
  regular = "ARIALN.ttf",
  italic = "ARIALNI.ttf"
)
showtext_auto()

load_data <- read_csv("data/clean_load_data.csv", show_col_types = F)
case_data <- read_csv("metc-wastewater-covid-monitor/data/case_data.csv", show_col_types = F)

## LARGE -----
ylim.cases <- c(0, max(case_data$covid_cases_7day, na.rm = T))
ylim.load <- c(0, max(load_data$copies_day_person_M_mn, na.rm = T))

b <- diff(ylim.cases) / diff(ylim.load)
# Graph of load
load_plot <-
  load_data %>%
  left_join(case_data, by = "date") %>%
  filter(date >= "2021-06-01") %>%
  ggplot(aes(x = date, y = copies_day_person_M_mn)) +
  geom_ribbon(
    aes(ymin = 0, ymax = covid_cases_7day / b),
    fill = colors$suppGray,
    alpha = 0.3,
    na.rm = T,
    linetype = "blank"
  ) +
  geom_line(
    aes(y = copies_day_person_M_mn),
    color = colors$councilBlue,
    alpha = 0.8,
    lwd = 1.2,
    na.rm = T
  ) +
  #   geom_point(
  #   aes(y = copies_day_person_M_mn),
  #   color = colors$councilBlue,
  #   alpha = 0.8,
  #   lwd = 1.2,
  #   na.rm = T
  # ) +
  scale_y_continuous(
    "Viral load (copies per person, per day)",
    labels = unit_format(unit = "M"),
    sec.axis = sec_axis(
      ~ . * b,
      name = "COVID-19 cases per 100,000 residents (7-day avg.)",
      breaks = seq(from = 0, to = max(case_data$covid_cases_7day, na.rm = T), by = 25)
    )
  ) +
  labs(
    title = "<span style='color:#0054A4'>Viral load in wastewater</span>
    compared to <span style='color:#888888;'>metro-area COVID-19 cases</span>",
    x = "Date",
    caption = paste0(
      "\nCase data (gray area) are seven-day rolling averages for the 7-county area provided by MDH\nand downloaded from USAFacts. Viral load (blue line) are daily data.\nLast sample date ",
      max(load_data$date, na.rm = T),
      "."
    )
  ) +
  scale_x_date(date_breaks = "month", date_labels = "%b '%y") +
  councilR::council_theme(
    # size_header = 7,
    # size_axis_text = 5,
    # size_axis_title = 6,
    # size_caption = 3,
    use_showtext = T
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA,
      linetype = 0
    ),
    axis.title.y.right = element_text(size = 64, color = "#888888", vjust = 1),
    axis.title.y.left = element_text(size = 64, color = colors$councilBlue, vjust = 1),
    axis.text.x = element_text(size =48),
    axis.title.x = element_text(size = 64),
    axis.text.y.right = element_text(size = 48, color = "#888888", vjust = 0),
    axis.text.y.left = element_text(size = 48, color = colors$councilBlue, vjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_markdown(
      lineheight = 1.1,
      size = 72, hjust = 0.5
    ),
    plot.caption = element_text(
      size = 36,
      lineheight = 0.25,
      face = "italic",
      family = "Arial Narrow Italic"
    )
  )


load_plot

ggsave("fig/cases_vs_load_large.png",
  load_plot,
  scale = 1,
  height = 8.5, width = 11,
  units = "in", dpi = 300
)



## SMALL ----
load_data %>%
  left_join(case_data, by = "date") %>%
  filter(date >= "2021-06-01") %>%
  ggplot(aes(x = date, y = copies_day_person_M_mn)) +
  geom_ribbon(
    aes(ymin = 0, ymax = covid_cases_7day / b),
    fill = colors$suppGray,
    alpha = 0.3,
    na.rm = T,
    linetype = "blank"
  ) +
  geom_line(
    aes(y = copies_day_person_M_mn),
    color = colors$councilBlue,
    alpha = 0.8,
    lwd = 0.25,
    na.rm = T
  ) +
  scale_y_continuous(
    "Viral load (M copies per person, per day)",
    sec.axis = sec_axis(
      ~ . * b,
      name = "COVID-19 cases per 100,000 residents (7-day avg.)",
      breaks = seq(from = 0, to = max(case_data$covid_cases_7day, na.rm = T), by = 50)
    )
  ) +
  labs(
    title = "<span style='color:#0054A4;'>Viral load in wastewater</span>
    compared to <span style='color:#888888;'>metro-area COVID-19 cases</span>",
    x = "Date",
    caption = paste0(
      "\nCase data (gray area) are seven-day rolling averages for the 7-county area provided by MDH\nand downloaded from USAFacts. Viral load (blue line) are daily data.\nLast sample date ",
      max(load_data$date, na.rm = T),
      "."
    )
  ) +
  scale_x_date(date_breaks = "month", date_labels = "%b '%y") +
  council_theme(
    size_header = 7,
    size_axis_text = 5,
    size_axis_title = 6,
    size_caption = 3,
    use_showtext = T
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA,
      linetype = 0
    ),
    plot.title = element_markdown(
      lineheight = 1.1,
      size = 24, hjust = 0.5
    ),
    axis.title.y.left = element_text(size = 18, color = "#888888", vjust = 1),
    axis.title.y.right = element_text(size = 18, color = colors$councilBlue, vjust = 1),
    axis.text.y.left = element_text(size = 18, color = "#888888", vjust = 0),
    axis.text.y.right = element_text(size = 18, color = colors$councilBlue, vjust = 0),
    axis.text.x = element_text(size =18),
    axis.title.x = element_text(size = 18),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_text(
      size = 18,
      lineheight = 0.25,
      face = "italic",
      family = "Arial Narrow Italic"
    )
  )

ggsave("fig/cases_vs_load_small.png",
  height = 800, width = 1200,
  units = "px", dpi = 300
)
