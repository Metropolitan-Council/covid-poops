library(cowplot)
library(magick)
library(ggtext)
library(showtext)
library(councilR)

font_add("HelveticaNeueLTStd", "HelveticaNeueLTStd-Lt.otf")
font_add("Arial Narrow", "Arial Narrow.ttf")
font_add("Arial Narrow Italic",
  regular = "Arial Narrow.ttf",
  italic = "Arial Narrow Italic.ttf"
)
showtext_auto()

variant_data_new <- read_csv("data/clean_variant_data.csv", show_col_types = F)


## LARGE -----

varplot <-
  variant_data_new %>%
  filter(!variant == "Other") %>%
  filter(date >= "2021-06-01") %>%
  ggplot(aes(x = date, y = frequency, color = variant, fill = variant)) +
  geom_line(aes(x = date, y = frequency_7day, color = variant), size = 0.8) +
  geom_area(position = "identity", aes(x = date, y = frequency_7day, group = variant, fill = variant, color = variant), alpha = 0.25, na.rm = T, lty = "blank") +
  geom_point() +
  scale_color_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571"),
    name = "Variant "
  ) +
  scale_fill_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571"),
    name = "Variant "
  ) +
  scale_y_continuous(name = "Variant frequency (%)", labels = scales::percent, limits = c(0, 1.05)) +
  scale_x_date(name = "Date", breaks = "1 months", date_labels = "%b '%y") +
  labs(
    title = "COVID-19 Variants in Metro Plant Wastewater",
    caption = paste0("\nShaded areas and lines are seven-day rolling averages. Points are daily data.\nLast sample date ", max(variant_data_new$date, na.rm = T), ".")
  ) +
  council_theme(use_showtext = T) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA,
      linetype = 0
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.caption = element_text(
      size = 11,
      face = "italic",
      margin = margin(t = -15, 0, 0, 0),
      family = "Arial Narrow Italic"
    ),
    legend.justification = c(0, 0),
    legend.text = element_text(size = 12),
  )

varplot

logo_file <- "metc-wastewater-covid-monitor/www/main-logo.png"

ggsave("fig/variants_static_graph_large.png",
  varplot,
  width = 11,
  height = 8.5,
  dpi = 300
)

## SMALL -----

variant_data_new %>%
  filter(
    !variant == "Other",
    date >= "2021-06-01"
  ) %>%
  ggplot(aes(x = date, y = frequency, color = variant, fill = variant)) +
  geom_line(aes(x = date, y = frequency_7day, color = variant),
    # size = 0.8,
    lwd = 0.3
  ) +
  geom_area(
    position = "identity", aes(
      x = date,
      y = frequency_7day,
      group = variant,
      fill = variant,
      color = variant
    ),
    alpha = 0.25, na.rm = T, lty = "blank"
  ) +
  geom_point(size = 0.1) +
  scale_color_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571"),
    name = "Variant "
  ) +
  scale_fill_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571"),
    name = "Variant "
  ) +
  scale_y_continuous(
    name = "Variant frequency (%)",
    labels = scales::percent, limits = c(0, 1.05)
  ) +
  scale_x_date(name = "Date", breaks = "1 months", date_labels = "%b '%y") +
  labs(
    title = "COVID-19 Variants in Metro Plant Wastewater",
    caption = paste0("\nShaded areas and lines are seven-day rolling averages. Points are daily data.\nLast sample date ", max(variant_data_new$date, na.rm = T), ".")
  ) +
  council_theme(
    size_header = 7,
    size_axis_text = 5,
    size_axis_title = 6,
    size_caption = 3,
    size_legend_title = 4,
    size_legend_text = 3,
    use_showtext = T
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA,
      linetype = 0
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(
      size = 3,
      # debug = T,
      face = "italic",
      family = "Arial Narrow Italic",
      margin = margin(t = -10, 0, 0, 0)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.margin = margin(),
    legend.key.height = unit(10, "pt"),
    legend.justification = c(0, 0),
    plot.title = element_text(hjust = 0.5)
  )


# logo_file <- "metc-wastewater-covid-monitor/www/main-logo.png"

ggsave("fig/variants_static_graph_small.png",
  height = 675,
  width = 1200,
  dpi = 300,
  units = "px"
)
