# Create figures!
# Font sizes may not render correctly on Mac

# Load data packages ----
library(readr)
library(dplyr)
library(tidyr)

# Load plot packages ----
library(ggplot2)
library(ggtext)
library(stringr)
library(councilR)

# Load custom themes -----
source("R/covid-plot-theme.R")
# loads theme_council_covidplot_large, theme_council_covidplot_insta

# Load data -----
case_data <- read_csv("data/case_data.csv", show_col_types = F) %>%
  mutate(incomplete_flag = ifelse(date > max(date) - 7, "incomplete", "complete"))

cases_load <- read_csv("data/clean_load_data.csv", show_col_types = F) %>%
  left_join(case_data, by = "date")

cases_load_90 <-
  cases_load %>%
  filter(date > max(date, na.rm = T) - 90)


# Figure snippets ----

## Title, large plot ----
# 64pt font for large plots
my_title_large <- paste0(
  "<span style='color:#0054A4; font-size:64pt'>",
  "Total Viral load",
  "</span>",
  " in Metro Plant Wastewater influent"
)

# Subtitle, large plot ----
# 64pt font for large plots
my_subtitle_large <- paste0(
  "compared to metro-area ",
  "<span style='color:#607894;font-size:64pt'>",
  "COVID-19 cases",
  "</span>"
)

## Title, Instagram ---
# 48pt font for instagram-sized plots
my_title_insta <- paste0(
  "<span style='color:#0054A4; font-size:48pt'>",
  "Total viral load",
  "</span>",
  "<br>",
  # extra break, add this all on one line
  " in Metro Plant wastewater influent compared to metro-area"
)

## Subtitle, Instagram ----
my_subtitle_insta <- paste0(
  "<span style='color:#607894;font-size:48pt'>",
  "COVID-19 cases",
  "</span>"
)

## Caption, all plots ----
my_caption <- paste0(
  "This graph shows the amount of SARS-CoV-2 viral RNA entering the Metro Plant each day (light blue line) ",
  "and averaged over the preceding 7 days (bold blue line). ",
  "The gray area shows the number of new daily COVID-19 cases in the Metro Plant's service area, by sample collection date averaged over the preceding 7 days. ",
  "COVID-19 case data are from the Minnesota Department of Health. ",
  "The most recent case data (darker gray) are incomplete and subject to change. ",
  "Last wastewater sample date ",
  max(cases_load$date, na.rm = T),
  "."
)

## Axis labels, all plots -----
my_yaxis_left <- stringr::str_wrap("Viral load (copies per person, per day)", width = 10)
my_yaxis_right <- stringr::str_wrap("New Daily COVID-19 cases per 100,000 residents (7-day avg.)", width = 10)


## Scaling for secondary y-axis, all dates -----
ylim.cases <- c(0, max(cases_load$covid_cases_7day, na.rm = T))
ylim.load <- c(0, max(cases_load$copies_day_person_7day, na.rm = T))
b <- diff(ylim.cases) / diff(ylim.load)

## Scaling for secondary y-axis, 90 days -----
ylim.cases90 <- c(0, max(cases_load_90$covid_cases_7day, na.rm = T))
ylim.load90 <- c(0, max(cases_load_90$copies_day_person_7day, na.rm = T))
b90 <- diff(ylim.cases90) / diff(ylim.load90)

# Base plot function ----
load_plot_base <-
  function(data,
           title,
           subtitle,
           sec_axis_b,
           caption_width = 180,
           date_breaks = "month",
           date_labels = "%b\n'%y") {
    data %>%
      filter(date >= "2021-10-01") %>%
      ggplot(aes(x = date, y = copies_day_person_M_mn)) +
      geom_bar(
        aes(
          y = covid_cases_7day / sec_axis_b,
          color = incomplete_flag,
          fill = incomplete_flag
        ),
        stat = "identity",
        width = 1,
        na.rm = T
      ) +
      geom_line(
        aes(
          y = copies_day_person_M_mn
        ),
        alpha = 0.6,
        lwd = 0.25,
        na.rm = T,
        color = colors$councilBlue
      ) +
      geom_line(
        aes(
          y = copies_day_person_7day
        ),
        alpha = 0.8,
        lwd = 0.5,
        na.rm = T,
        color = colors$councilBlue
      ) +
      scale_fill_manual(values = c("#C1CCD9", "#8A91AB")) +
      scale_color_manual(values = c("#C1CCD9", "#8A91AB")) +
      guides(
        color = "none",
        fill = "none"
      ) +
      scale_y_continuous(
        name = my_yaxis_left,
        labels = scales::unit_format(unit = "M"),
        sec.axis = sec_axis(
          ~ . * sec_axis_b,
          name = my_yaxis_right
          # breaks = seq(
          #   from = 0,
          #   # round to nearest hundred
          #   to = round(max(data[,"covid_cases_7day"], na.rm = T), -2),
          #   by = by_cases
          # )
        )
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        caption = stringr::str_wrap(my_caption, width = caption_width)
      ) +
      scale_x_date(date_breaks = date_breaks, 
                   date_labels = date_labels)
  }


# All dates, large -----
cases_vs_load_large <-
  load_plot_base(
    data = cases_load,
    title = my_title_large,
    subtitle = my_subtitle_large,
    sec_axis_b = b,
    caption_width = 180,
    date_breaks = "2 months",
    date_labels = "%b\n'%y"
  ) +
  theme_council_covidplot_large(
    use_showtext = T,
    use_manual_font_sizes = TRUE
  ) +
  theme(
    plot.title = element_markdown(
      lineheight = 0.5,
      size = 48,
      hjust = 0.1
    ),
    plot.subtitle = element_markdown(
      lineheight = 0.5,
      size = 48,
      hjust = 0.9
    )
  )


ggsave("fig/cases_vs_load_large.png",
  cases_vs_load_large,
  scale = 1,
  height = 4, width = 8,
  units = "in", dpi = 300
)


# Instagram, All Dates ----
cases_vs_load_insta <-
  load_plot_base(
    data = cases_load,
    title = my_title_insta,
    subtitle = my_subtitle_insta,
    sec_axis_b = b,
    caption_width = 110,
    date_breaks = "3 months",
    date_labels = "%b\n'%y"
  ) +
  theme_council_covidplot_insta(
    use_showtext = T,
    use_manual_font_sizes = TRUE
  ) +
  theme(
    plot.title = element_markdown(
      lineheight = 0.5,
      size = 30,
      hjust = 0
    ),
    plot.subtitle = element_markdown(
      lineheight = 0.5,
      size = 30,
      hjust = 1
    )
  )

ggsave("fig/cases_vs_load_insta.png",
  cases_vs_load_insta,
  height = 1080, width = 1080,
  units = "px", dpi = 300
)

# Instagram, Last 90 Days ----
cases_vs_load_insta_90days <-
  load_plot_base(
    data = cases_load_90,
    title = my_title_insta,
    subtitle = my_subtitle_insta,
    sec_axis_b = b90,
    caption_width = 110,
    date_breaks = "2 weeks",
    date_labels = "%b\n'%y"
  ) +
  theme_council_covidplot_insta(
    use_showtext = T,
    use_manual_font_sizes = TRUE
  ) +
  theme(
    plot.title = element_markdown(
      lineheight = 0.5,
      size = 30,
      hjust = 0
    ),
    plot.subtitle = element_markdown(
      lineheight = 0.5,
      size = 30,
      hjust = 1
    )
  )

ggsave("fig/cases_vs_load_insta_90days.png",
  cases_vs_load_insta_90days,
  height = 1080, width = 1080,
  units = "px", dpi = 300
)

# Instagram, Omicron Era ----
cases_load_Omi <-
  cases_load %>%
  filter(date >= "2021-12-01")

cases_vs_load_insta_OmicronEra <-
  load_plot_base(
    data = cases_load_Omi,
    title = my_title_insta,
    subtitle = my_subtitle_insta,
    sec_axis_b = b90,
    caption_width = 110,
    date_breaks = "8 weeks",
    date_labels = "%b\n%d"
  ) +
  theme_council_covidplot_insta(
    use_showtext = T,
    use_manual_font_sizes = TRUE
  ) +

  ## custom y axis - will need to adjust manually over time ----
  scale_y_continuous(
    name = my_yaxis_left,
    labels = scales::unit_format(unit = "M"),
    sec.axis = sec_axis(
      ~ . * b90,
      name = my_yaxis_right,
      breaks = seq(
        from = 0,
        to = 100,
        by = 25
      )
    )
  ) +
  theme(
    plot.title = element_markdown(
      lineheight = 0.5,
      size = 30,
      hjust = 0
    ),
    plot.subtitle = element_markdown(
      lineheight = 0.5,
      size = 30,
      hjust = 1
    )
  )

ggsave("fig/cases_vs_load_insta_OmicronEra.png",
  cases_vs_load_insta_OmicronEra,
  height = 1080, width = 1080,
  units = "px", dpi = 300
)
