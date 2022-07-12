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
source('R/covid-plot-theme.R')
# loads theme_council_covidplot_large, theme_council_covidplot_insta

# Load data -----
cases_load <- read_csv("data/clean_load_data.csv", show_col_types = F) %>%
  left_join(read_csv("data/case_data.csv", show_col_types = F), by = "date") 

cases_load_90 <- 
  cases_load %>%
  filter(date > max(date, na.rm = T)-90)


# Figure snippets ----

## Title, large plot ----
# 64pt font for large plots
my_title_large <-  paste0(
  "<span style='color:#0054A4; font-size:64pt'>",
  "COVID-19 Viral load",
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
my_title_insta <-  paste0(
  "<span style='color:#0054A4; font-size:48pt'>",
  "COVID-19 Viral load",
  "</span>",
  "<br>",
  # extra break, add this all on one line
  " in Metro Plant Wastewater influent compared to metro-area"
)

## Subtitle, Instagram ----
my_subtitle_insta <- paste0(
  "<span style='color:#607894;font-size:48pt'>",
  "COVID-19 cases",
  "</span>"
)

## Caption, all plots ----
my_caption <- paste0(
  "Seven-day averagzes (gray area, thicker blue line) are for the preceding 7 days of data. ",
  "Narrower blue line represents daily wastewater load values. ",
  "Source: MDH and USA Facts (case data); Metropolitan Council and U of M Genomics Center (wastewater data). ",
  "Last sample date ",
  max(cases_load$date, na.rm = T),
  "."
)

## Axis labels, all plots -----
my_yaxis_left <- stringr::str_wrap("Viral load (M copies per person, per day)", width = 10)
my_yaxis_right <- stringr::str_wrap("COVID-19 cases per 100,000 residents (7-day avg.)", width = 10)


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
      geom_ribbon(
        aes(
          ymin = 0,
          ymax = covid_cases_7day / b,
          linetype = "COVID-19 cases, 7-day average",
          color = "COVID-19 cases, 7-day average"
        ),
        fill = "#607894",
        alpha = 0.3,
        na.rm = T
      ) +
      geom_line(
        aes(
          y = copies_day_person_7day,
          linetype = "Viral load, 7-day average",
          color = "Viral load, 7-day average"
        ),
        lwd = 0.4,
        na.rm = T
      ) +
      geom_line(
        aes(
          y = copies_day_person_M_mn,
          linetype = "Viral load, daily data",
          color = "Viral load, daily data"
        ),
        alpha = 0.8,
        lwd = 0.2,
        na.rm = T
      ) +
      scale_linetype_manual(values = c("blank",
                                       "solid",
                                       "solid")) +
      scale_color_manual(values = c("#607894",
                                    colors$councilBlue,
                                    colors$councilBlue)) +
      guides(color = "none",
             linetype = "none") +
      scale_y_continuous(
        name = my_yaxis_left,
        labels = scales::unit_format(unit = "M"),
        sec.axis = sec_axis(
          ~ . * sec_axis_b,
          name = my_yaxis_right,
          breaks = seq(
            from = 0,
            # round to nearest hundred
            to = round(max(data[,"covid_cases_7day"], na.rm = T), -2), 
            by = 50
          )
        )
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        caption = stringr::str_wrap(my_caption, width = caption_width)
      ) +
      scale_x_date(date_breaks = date_breaks, date_labels = date_labels)
  }
  

# All dates, large -----
cases_vs_load_large <- 
  load_plot_base(data = cases_load,
           title = my_title_large,
           subtitle = my_subtitle_large,
           sec_axis_b = b,
           caption_width = 180,
           date_breaks = "month",
           date_labels = "%b\n'%y") + 
  theme_council_covidplot_large(use_showtext = T,
                                use_manual_font_sizes = TRUE) +
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
  load_plot_base(data = cases_load,
                 title = my_title_insta,
                 subtitle = my_subtitle_insta,
                 sec_axis_b = b,
                 caption_width = 110,
                 date_breaks = "month",
                 date_labels = "%b\n'%y") + 
  theme_council_covidplot_insta(use_showtext = T,
                                use_manual_font_sizes = TRUE) +
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
  load_plot_base(data = cases_load_90,
                 title = my_title_insta,
                 subtitle = my_subtitle_insta,
                 sec_axis_b = b90,
                 caption_width = 110,
                 date_breaks = "2 weeks",
                 date_labels = "%b %d") + 
  theme_council_covidplot_insta(use_showtext = T,
                                use_manual_font_sizes = TRUE) +
  
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

ggsave("fig/cases_vs_load_insta_90days.png",
       cases_vs_load_insta_90days,
       height = 1080, width = 1080, 
       units = "px", dpi = 300
)
