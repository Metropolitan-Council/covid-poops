library(cowplot)
library(magick)
library(ggtext)
library(showtext)
library(councilR)
# Font sizes may not render correctly on Mac


font_add("HelveticaNeueLTStd", "HelveticaNeueLTStd-Lt.otf")
font_add("HelveticaNeueLTStd", "HelveticaNeueLTStd-Lt.otf")
font_add("Arial Narrow", "ARIALN.ttf")
font_add("Arial Narrow Italic",
  regular = "ARIALN.ttf",
  italic = "ARIALNI.ttf"
)

showtext_auto()

variant_data_date <- read_csv("data/clean_variant_data.csv", show_col_types = F)
## LARGE -----

varplot <-
  variant_data_date %>%
  filter(!variant == "Other") %>%
  filter(date >= "2021-06-01") %>%
  ggplot(aes(x = date, y = frequency, color = variant, fill = variant)) +
  geom_line(aes(x = date, y = frequency_7day, color = variant), size = 0.8) +
  geom_area(position = "identity", aes(x = date, y = frequency_7day, group = variant, fill = variant, color = variant), alpha = 0.25, na.rm = T, lty = "blank") +
  geom_point() +
  scale_color_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776"),
    #values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776", '#92BED2'), # Turn this on when start detecting BA.2.12.1
    name = "Variant "
  ) +
  scale_fill_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776"),
    #values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776", '#92BED2'), # Turn this on when start detecting BA.2.12.1
    name = "Variant "
  ) +
  scale_y_continuous(name = "Variant frequency (%)", labels = scales::percent, limits = c(0, 1.05)) +
  scale_x_date(name = "Date", breaks = "1 months", date_labels = "%b '%y") +
  labs(
    title = "COVID-19 Variants in Metro Plant Wastewater",
    caption = paste0("\nShaded areas and lines are seven-day rolling averages. Points are daily data.\nLast sample date ", max(variant_data_date$date, na.rm = T), ".")
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
    plot.title = element_text(size = 72, hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.caption = element_text(
      size = 36,
      lineheight = 0.25,
      margin = margin(t = -15, 0, 0, 0),
      face = "italic",
      family = "Arial Narrow Italic"
    ),
    legend.justification = c(0, 0),
    legend.text = element_text(size = 48),
    legend.title = element_text(size = 48),
    axis.title.y = element_text(size = 64, vjust = 1),
    axis.text.x = element_text(size = 48),
    axis.title.x = element_text(size = 64),
    axis.text.y = element_text(size = 48, vjust = 0)
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

variant_data_date %>%
  filter(
    !variant == "Other",
    date >= "2021-06-01"
  ) %>%
  ggplot(aes(x = date, y = frequency, color = variant, fill = variant)) +
  geom_line(aes(x = date, y = frequency_7day, color = variant),
    # size = 0.8,
    lwd = 1
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
  geom_point(size = 1) +
  scale_color_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776"),
    #values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776", '#92BED2'), # Turn this on when start detecting BA.2.12.1
    name = "Variant "
  ) +
  scale_fill_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776"),
    #values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776", '#92BED2'), # Turn this on when start detecting BA.2.12.1
    name = "Variant "
  ) +
  scale_y_continuous(
    name = "Variant frequency (%)",
    labels = scales::percent, limits = c(0, 1.05)
  ) +
  scale_x_date(name = "Date", breaks = "1 months", date_labels = "%b '%y") +
  labs(
    title = "COVID-19 Variants in Metro Plant Wastewater",
    caption = paste0("\nShaded areas and lines are seven-day rolling averages. Points are daily data.\nLast sample date ", max(variant_data_date$date, na.rm = T), ".")
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
      size = 18,
      # debug = T,
      lineheight = 0.25,
      face = "italic",
      family = "Arial Narrow Italic",
      margin = margin(t = -1, 0, 0, 0)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.margin = margin(),
    legend.key.height = unit(10, "pt"),
    legend.justification = c(0, 0),
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(size = 18, vjust = 1),
    axis.text.y = element_text(size = 18, vjust = 0),
    axis.text.x = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 18),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank()
  )


# logo_file <- "metc-wastewater-covid-monitor/www/main-logo.png"

ggsave("fig/variants_static_graph_small.png",
  height = 800,
  width = 1200,
  dpi = 300,
  units = "px"
)
