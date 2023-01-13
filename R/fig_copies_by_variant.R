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
      "Other",
      "Alpha, Beta & Gamma",
      "Delta",
      "Omicron BA.1",
      "Omicron BA.2.12.1",
      "Omicron BA.4 and BA.5",
      "Omicron BA.4",
      "Omicron BA.5 (Excluding BQ.1)",
      "Omicron BQ.1",
      "Omicron BA.2 (Excluding BA.2.12.1)",
      "Omicron BA.2.75",
      "XBB"
    )
  )) %>%
  filter(!(variant == "Omicron BA.4 and BA.5" &
             date > "2022-05-30")) %>%
  filter(date > "2021-04-01")

# Plot Snippets -----
my_title <-  "Viral load by variant"

my_subtitle <- "in Metro Plant wastewater influent, seven-day averages"

my_caption <-  paste0(
  "Viral load of each variant is estimated by multiplying the total viral load by the prevalence of each variant. ",
  "Because frequencies do not always add to 100%, a total of all variants may be slightly greater or less than the total viral load. ",
  "Omicron BA.4 and BA.5 were not distinguished from one another using separate assays until 05-30-22. ",
  "These variants are shown together before this point, and separately afterwards. ",
  "Last sample date ",
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
  "Omicron BQ.1" = "#006400",
  "Omicron BA.5 (Excluding BQ.1)" = "#000080",
  "XBB" = "#800000",
  "Omicron BA.2.75" = "#E4ADC4",
  "Total COVID-19 Load" = "gray60"

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
    "Omicron BQ.1" = "#006400",
    "Omicron BA.5 (Excluding BQ.1)" = "#000080",
    "XBB" = "#800000",
    "Omicron BA.2.75" = "#E4ADC4",
    "Total COVID-19 Load" = "gray60"

  )



# Base plotting function ----

  
base_plot <-
  function(data, caption_width) {
    data %>%
      ggplot(aes(x = date, y = copies_variant)) +
      # solid black line - total covid load
      geom_area(
        data = load_data[load_data$date > min(data$date),] %>%
          mutate(variant = "Total COVID-19 Load"),
        aes(
          x = date,
          y = copies_day_person_7day,
          color = variant,
          fill = variant
        ),
        lty = "blank",
        alpha = 0.5
      ) +
      geom_area(
        position = "identity",
        aes(
          x = date,
          y = copies_7day,
          color = variant,
          fill = variant
        ),
        alpha = 0.5,
        na.rm = T,
        lwd = 0.4
        # lty = "blank"
      ) +
      # geom_point(
      #   aes(color = variant, fill = variant), size = 0.25) +
      scale_fill_manual(values = variant_fill_pal,
                        drop = T) + 
      scale_color_manual(values = variant_color_pal,
                         drop = T) +
      scale_y_continuous(my_y_label,
                         labels = scales::unit_format(unit = "M")) +
      scale_x_date(name = "Date",
                   breaks = "3 months",
                   date_labels = "%b '%y") +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        caption = str_wrap(my_caption,
                           width = caption_width)
      )
  }


## Large, all dates
copies_by_variant_large <-
  base_plot(data = copies_by_variant, caption_width = 180) + 
  theme_council_covidplot_large(use_showtext = T,
                                use_manual_font_sizes = T) +
  guides(fill = guide_legend(nrow = 2),
         color = guide_legend(nrow = 2)) +
  theme(legend.justification = c(0.9, 0),
        legend.text = element_text(size = 25))

ggsave(
  "fig/copies_by_variant_large.png",
  copies_by_variant_large,
  scale = 1,
  height = 4,
  width = 8,
  units = "in",
  dpi = 300
)

# Instagram, all Dates ----
copies_by_variant_insta <-
  base_plot(data = copies_by_variant, caption_width = 110) + 
  theme_council_covidplot_insta(use_showtext = T,
                                use_manual_font_sizes = T) +
  guides(fill = guide_legend(nrow = 3),
         color = guide_legend(nrow = 3)) +
  theme(legend.justification = c(0.85, 0),
        legend.text = element_text(size = 15))


ggsave(
  "fig/copies_by_variant_insta.png",
  copies_by_variant_insta,
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


copies_variant_insta_90days <-
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
    "Omicron BQ.1" = "#006400",
    "Omicron BA.5 (Excluding BQ.1)" = "#000080",
    "XBB" = "#800000",
    "Omicron BA.2.75" = "#E4ADC4",
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
      "Omicron BQ.1" = "#006400",
      "Omicron BA.5 (Excluding BQ.1)" = "#000080",
      "XBB" = "#800000",
      "Omicron BA.2.75" = "#E4ADC4",
      "Total COVID-19 Load" = "white"

    )
  )) + 
  theme_council_covidplot_insta(use_showtext = T,
                                use_manual_font_sizes = T) +
  guides(fill = guide_legend(nrow = 3),
         color = guide_legend(nrow = 3)) +
  theme(legend.justification = c(0.85, 0),
        legend.text = element_text(size = 14))


ggsave(
  "fig/copies_variant_insta_90days.png",
  copies_variant_insta_90days,
  height = 1080,
  width = 1080,
  units = "px",
  dpi = 300
)


