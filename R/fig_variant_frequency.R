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

# Data -----
variant_data_date <- read_csv("data/clean_variant_data.csv", show_col_types = F) %>%
  # Get rid of BA4/5 data that will duplicate where we started measuring 4 & 5 separately:
  filter(!(variant == "Omicron BA.4 and BA.5" & date > "2022-05-30")) %>%
  # Choose just the relevant dates: 
  filter(date > "2021-04-01") %>%
  # Set factor levels for the variant: 
  mutate(variant = factor(
    variant,
    levels = c(
      "Other",
      "Alpha, Beta & Gamma",
      "Delta",
      "Omicron BA.1",
      "Omicron BA.2 (Excluding BA.2.12.1)",
      "Omicron BA.2.12.1",
      "Omicron BA.4 and BA.5",
      "Omicron BA.4",
      "Omicron BA.5"
    )
  ))

# Figure snippets ----
my_title <- "COVID-19 Variant Frequencies (%)"

my_subtitle <- "in Metro Plant wastewater influent. Daily data are shown as thin lines; seven-day averages are shown as bold lines."

my_caption <- paste0(
  "Variants are tracked by sequencing marker gene mutations on the SARS-CoV-2 viral genome. ",
  "Omicron BA.4 and BA.5 were not distinguished from one another using separate assays until 05-30-22. ",
  "These variants are shown together before this point, and separately afterwards. ",
  "Because assays for each variant are performed separately, variant frequencies do not always sum to 100%. ",
  "Last sample date ",
  max(variant_data_date$date, na.rm = T),
  "."
)

my_y_label <- stringr::str_wrap("Variant frequency (%)", width = 10)

# Color palette ----
variant_pal <- c("Alpha, Beta & Gamma" = "#84BB25", 
                 "Delta" = "#1D94B7", 
                 "Omicron BA.1" = "#6D3571", 
                 "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776", 
                 "Omicron BA.2.12.1" = "#FBC740",
                 "Omicron BA.4 and BA.5" = "#A9A3FE", 
                 "Omicron BA.4" = "#3D9F93", 
                 "Omicron BA.5" = "#000080")

# Base plot
base_plot <- function(data, caption_width = 180, subtitle_width = 100){
  ggplot(
    data = data,
    aes(
      x = date,
      y = frequency,
      color = variant
    )
  ) +
    geom_line(aes(
      x = date,
      y = frequency_7day,
      color = variant
    ),
    lwd = 0.4
    ) +
    geom_line(
      data = data[!is.na("frequency"), ],
      aes(x = date, y = frequency, color = variant),
      lwd = 0.15,
      na.rm = T,
      alpha = 0.5
    ) +
    scale_color_manual(values = variant_pal) +
    scale_y_continuous(
      name = my_y_label,
      labels = scales::percent,
      limits = c(0, 1.05)
    ) +
    scale_x_date(
      name = "Date",
      breaks = "3 months",
      date_labels = "%b '%y"
    ) +
    labs(
      title = my_title,
      subtitle = str_wrap(my_subtitle,
                          width = subtitle_width),
      caption = str_wrap(my_caption,
                         width = caption_width
      )
    )
}

## Large, all dates -----
variant_frequency_large <-
  base_plot(data = variant_data_date, caption_width = 180, subtitle_width = 80) + 
  theme_council_covidplot_large(
    use_showtext = T,
    use_manual_font_sizes = TRUE
  ) + 
  guides(
    fill = guide_legend(nrow = 2),
    color = guide_legend(nrow = 2)
  ) +
  theme(
    legend.justification = c(0.1, 0)
  )
  
 
ggsave(
  "fig/variant_frequency_large.png",
  variant_frequency_large,
  width = 8,
  height = 4,
  dpi = 300
)

## insta, all dates -----
variant_frequency_insta <- 
  base_plot(data = variant_data_date, caption_width = 110, subtitle_width = 60) + 
  theme_council_covidplot_insta(
    use_showtext = T,
    use_manual_font_sizes = TRUE
  ) + 
  guides(
    fill = guide_legend(nrow = 3),
    color = guide_legend(nrow = 3)
  ) + 
  theme(legend.justification = c(1, 0))

ggsave(
  "fig/variant_frequency_insta.png",
  variant_frequency_insta,
  height = 1080,
  width = 1080,
  dpi = 300,
  units = "px"
)

# insta, last 90 days
cutoff90 <- max(variant_data_date$date) -90
variant_data_date_90 = variant_data_date %>% 
  filter(!is.na(frequency_7day) & date >= cutoff90) %>%
  droplevels()

variant_frequency_insta_90days <- 
  base_plot(data = variant_data_date_90, caption_width = 110, subtitle_width = 60) + 
  theme_council_covidplot_insta(
    use_showtext = T,
    use_manual_font_sizes = TRUE
  ) + 
  scale_x_date(name = "Date",
               breaks = "2 weeks",
               date_labels = "%b %d")+
  guides(
    fill = guide_legend(nrow = 3),
    color = guide_legend(nrow = 3)
  ) + 
  theme(legend.justification = c(1, 0)) + 
  scale_color_manual(
    values = c(
      # turn off colors as they leave the stream ...
      # "Alpha, Beta & Gamma" = "#84BB25", 
               "Delta" = "#1D94B7", 
               "Omicron BA.1" = "#6D3571", 
               "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776", 
               "Omicron BA.2.12.1" = "#FBC740",
               "Omicron BA.4 and BA.5" = "#A9A3FE", 
               "Omicron BA.4" = "#3D9F93", 
               "Omicron BA.5" = "#000080")
  )
  
ggsave(
  "fig/variant_frequency_insta_90days.png",
  variant_frequency_insta_90days,
  height = 1080,
  width = 1080,
  dpi = 300,
  units = "px"
)