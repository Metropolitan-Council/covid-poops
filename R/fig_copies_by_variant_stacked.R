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
load_data <- read_csv("data/clean_load_data.csv", show_col_types = F)

copies_by_variant <-
  read_csv("data/copies_by_variant.csv", show_col_types = F) %>%
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
             date > "2022-05-31")) %>%
  filter(date > "2021-04-01") %>%
  mutate(variant = factor(variant, levels = rev(levels(variant))))

# Plot Snippets -----
my_title <-  "Viral load by variant"

my_subtitle <- "in Metro Plant wastewater influent, seven-day averages"

my_caption <-  paste0(
  "The viral load for each variant is estimated by multiplying the total viral load by the frequency of that variant. ",
  "Because measured frequencies do not always add to 100%, the sum of all variant loads may be slightly greater or less than the total viral load. ",
  "The loads of Omicron BA.4 and BA.5 are shown as the sum (BA.4+BA.5) before May 31, 2022, and separately afterwards. ",
  "Last wastewater sample date ",
  max(copies_by_variant$date, na.rm = T),
  "."
)

my_y_label <-
  str_wrap("Viral load (copies per person, per day)", width = 10)


# Color palettes -----
variant_fill_pal <- c(
  "Alpha, Beta & Gamma" = "#84BB25",
  "Delta" = "#1D94B7",
  "Omicron BA.1" = "#6D3571",
  "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776",
  "Omicron BA.2.12.1" = "#FBC740",
  "Omicron BA.4 and BA.5" = "#A9A3FE",
  "Omicron BA.4" = "#3D9F93",
  "Omicron BA.5" = "#000080",
  "Total COVID-19 Load" = "white"
)


variant_color_pal <-
  c(
    "Alpha, Beta & Gamma" = "#84BB25",
    "Delta" = "#1D94B7",
    "Omicron BA.1" = "#6D3571",
    "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776",
    "Omicron BA.2.12.1" = "#FBC740",
    "Omicron BA.4 and BA.5" = "#A9A3FE",
    "Omicron BA.4" = "#3D9F93",
    "Omicron BA.5" = "#000080",
    "Total COVID-19 Load" = "black"
  )



# Base plotting function ----
base_plot <-
  function(data, caption_width) {
    data %>%
      ggplot(aes(x = date, y = copies)) +
      geom_area(
        position = "stack",
        aes(
          x = date,
          y = copies_7day,
          color = variant,
          fill = variant
        ),
        alpha = 0.75,
        na.rm = T,
        lwd = 0.25
      ) +
      # solid black line - total covid load
      geom_line(
        data = load_data[load_data$date > min(data$date),] %>%
          mutate(variant = "Total COVID-19 Load"),
        aes(
          x = date,
          y = copies_day_person_7day,
          color = variant,
          # fill = variant
        ),
        lwd = 0.6
      ) +
      scale_fill_manual(values = variant_fill_pal,
                        drop = T) + 
      scale_color_manual(values = variant_color_pal,
                         drop = T) +
      scale_y_continuous(my_y_label,
                         labels = scales::unit_format(unit = "M")) +
      scale_x_date(name = "Date",
                   breaks = "2 months",
                   date_labels = "%b '%y") +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        caption = str_wrap(my_caption,
                           width = caption_width)
      )
  }


## Large, all dates
copies_by_variant_stacked_large <-
base_plot(data = copies_by_variant, caption_width = 180) + 
  theme_council_covidplot_large(use_showtext = T,
                                use_manual_font_sizes = T) +
  guides(fill = guide_legend(nrow = 2),
         color = guide_legend(nrow = 2)) +
  theme(legend.justification = c(0.9, 0))

ggsave(
  "fig/copies_by_variant_stacked_large.png",
  copies_by_variant_stacked_large,
  scale = 1,
  height = 4,
  width = 8,
  units = "in",
  dpi = 300
)

# Instagram, all Dates ----

copies_by_variant_stacked_insta <-
  base_plot(data = copies_by_variant, caption_width = 110) + 
  theme_council_covidplot_insta(use_showtext = T,
                                use_manual_font_sizes = T) +
  guides(fill = guide_legend(nrow = 3),
         color = guide_legend(nrow = 3)) +
  theme(legend.justification = c(0.85, 0),
        legend.text = element_text(size = 21))


ggsave(
  "fig/copies_by_variant_stacked_insta.png",
  copies_by_variant_stacked_insta,
  scale = 1,
  height = 1080,
  width = 1080,
  units = "px",
  dpi = 300
)




# Instagram, last 90 days -----
cutoff90 <- max(copies_by_variant$date) -90
copies_by_variant_90 <- 
  copies_by_variant %>% 
  filter(!is.na(copies_7day) & date >= cutoff90) %>%
  droplevels()


copies_variant_stacked_insta_90days <-
  base_plot(data = copies_by_variant_90, caption_width = 110) + 
  scale_x_date(name = "Date",
               breaks = "2 weeks",
               date_labels = "%b %d") +
  scale_color_manual(values = c(
    # "Alpha, Beta & Gamma" = "#84BB25",
    "Delta" = "#1D94B7",
    "Omicron BA.1" = "#6D3571",
    "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776",
    "Omicron BA.2.12.1" = "#FBC740",
    "Omicron BA.4 and BA.5" = "#A9A3FE",
    "Omicron BA.4" = "#3D9F93",
    "Omicron BA.5" = "#000080",
    "Total COVID-19 Load" = "black"
  )) + 
  scale_fill_manual(values = c(
    c(
      # "Alpha, Beta & Gamma" = "#84BB25",
      "Delta" = "#1D94B7",
      "Omicron BA.1" = "#6D3571",
      "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776",
      "Omicron BA.2.12.1" = "#FBC740",
      "Omicron BA.4 and BA.5" = "#A9A3FE",
      "Omicron BA.4" = "#3D9F93",
      "Omicron BA.5" = "#000080",
      "Total COVID-19 Load" = "white"
    )
  )) + 
    theme_council_covidplot_insta(use_showtext = T,
                                  use_manual_font_sizes = T) +
    guides(fill = guide_legend(nrow = 3),
           color = guide_legend(nrow = 3)) +
    theme(legend.justification = c(0.85, 0),
          legend.text = element_text(size = 21))


ggsave(
  "fig/copies_by_variant_stacked_insta_90days.png",
  copies_variant_stacked_insta_90days,
  height = 1080,
  width = 1080,
  units = "px",
  dpi = 300
)




# Instagram, Since Omicron -----
copies_by_variant_Omi <- 
  copies_by_variant %>% 
  filter(!is.na(copies_7day) & date >= "2021-12-01") %>%
  droplevels()


copies_variant_stacked_insta_OmicronEra <-
  base_plot(data = copies_by_variant_Omi, caption_width = 110) + 
  scale_x_date(name = "Date",
               breaks = "3 weeks",
               date_labels = "%b\n%d") +
  scale_color_manual(values = c(
    # "Alpha, Beta & Gamma" = "#84BB25",
    "Delta" = "#1D94B7",
    "Omicron BA.1" = "#6D3571",
    "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776",
    "Omicron BA.2.12.1" = "#FBC740",
    "Omicron BA.4 and BA.5" = "#A9A3FE",
    "Omicron BA.4" = "#3D9F93",
    "Omicron BA.5" = "#000080",
    "Total COVID-19 Load" = "black"
  )) + 
  scale_fill_manual(values = c(
    c(
      # "Alpha, Beta & Gamma" = "#84BB25",
      "Delta" = "#1D94B7",
      "Omicron BA.1" = "#6D3571",
      "Omicron BA.2 (Excluding BA.2.12.1)" = "#D64776",
      "Omicron BA.2.12.1" = "#FBC740",
      "Omicron BA.4 and BA.5" = "#A9A3FE",
      "Omicron BA.4" = "#3D9F93",
      "Omicron BA.5" = "#000080",
      "Total COVID-19 Load" = "white"
    )
  )) + 
  theme_council_covidplot_insta(use_showtext = T,
                                use_manual_font_sizes = T) +
  guides(fill = guide_legend(nrow = 3),
         color = guide_legend(nrow = 3)) +
  theme(legend.justification = c(0.85, 0),
        legend.text = element_text(size = 21))


ggsave(
  "fig/copies_by_variant_stacked_insta_OmicronEra.png",
  copies_variant_stacked_insta_OmicronEra,
  height = 1080,
  width = 1080,
  units = "px",
  dpi = 300
)
