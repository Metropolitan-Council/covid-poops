# Create figures!
# Font sizes may not render correctly on Mac

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

copies_by_variant <- read.csv("data/copies_by_variant.csv") %>%
  mutate(date = as.Date(date))


## LARGE -----
load_varplot <-
  copies_by_variant %>%
  filter(date >= "2021-01-01" & !variant == "Other") %>%
  mutate(variant = factor(variant, levels = c("Other", "Alpha, Beta & Gamma", "Delta", "Omicron BA.1", "Omicron BA.2 (Excluding BA.2.12.1)", "Omicron BA.2.12.1"))) %>% # Turn this on when start detecting BA.2.12.1
  ggplot(aes(x = date, y = copies)) +

  # gray background area - total
  geom_area(position = "identity", data = load_data, aes(x = date, y = copies_day_person_7day), color = "gray50", fill = "gray50", alpha = 0.5, lty = "blank") +
  geom_line(aes(x = date, y = copies_7day, color = variant), size = 0.8) +
  geom_area(position = "identity", aes(x = date, y = copies_7day, group = variant, fill = variant, color = variant), alpha = 0.5, na.rm = T, lty = "blank") +
  geom_point(aes(color = variant, fill = variant)) +
  scale_color_manual(
    values = c("gray50", "#84BB25", "#1D94B7", "#6D3571", "#D64776", "#FBC740"),
    # name = "Variant ",
    drop = F
  ) +
  scale_fill_manual(
    values = c("gray50", "#84BB25", "#1D94B7", "#6D3571", "#D64776", "#FBC740"),
    # name = "Variant ",
    drop = F
  ) +
  scale_y_continuous(
    "Viral load (copies per person, per day)",
    labels = scales::unit_format(unit = "M")
  ) +
  scale_x_date(name = "Date", breaks = "2 months", date_labels = "%b '%y") +
  labs(
    title = "COVID-19 Variants in Metro Plant Wastewater",
    caption = paste0("\n\n\nViral load of each variant is estimated by multiplying the total viral load (shown in gray) by the frequency of each variant. Frequencies do not always add to 100%.\nShaded areas and lines are seven-day rolling averages. Points are daily data. \nLast sample date ", max(variant_data_date$date, na.rm = T), ".")
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
    legend.position = "top",
    # legend.direction = "horizontal",
    plot.caption = element_text(
      size = 36,
      lineheight = 0.25,
      margin = margin(t = -15, 0, 0, 0),
      face = "italic",
      family = "Arial Narrow Italic"
    ),
    legend.justification = c(0, 0),
    legend.text = element_text(size = 48),
    # legend.title = element_text(size = 48),
    axis.title.y = element_text(size = 64, vjust = 1),
    axis.text.x = element_text(size = 40),
    axis.title.x = element_text(size = 64),
    axis.text.y = element_text(size = 48, vjust = 0),
    legend.title = element_blank() # turn off legend title
  )

load_varplot


ggsave("fig/copies_by_variant_large.png",
  load_varplot,
  scale = 1,
  height = 8.5, width = 11,
  units = "in", dpi = 300
)

## IG -----

load_varplot_ig <-
  copies_by_variant %>%
  filter(date >= "2021-01-01" & !variant == "Other") %>%
  mutate(variant = factor(variant, levels = c(
    "Other", "Alpha, Beta & Gamma",
    "Delta", "Omicron BA.1", "Omicron BA.2 (Excluding BA.2.12.1)", "Omicron BA.2.12.1"
  ))) %>%
  ggplot(aes(x = date, y = copies)) +

  # gray background area - total
  geom_area(
    position = "identity", data = load_data, aes(
      x = date,
      y = copies_day_person_7day
    ),
    color = "gray50", fill = "gray50", alpha = 0.5, lty = "blank"
  ) +
  geom_line(aes(x = date, y = copies_7day, color = variant), size = 0.8) +
  geom_area(
    position = "identity", aes(
      x = date,
      y = copies_7day, group = variant,
      fill = variant, color = variant
    ),
    alpha = 0.5, na.rm = T, lty = "blank"
  ) +
  geom_point(aes(color = variant, fill = variant), alpha = 0.5, lwd = 0.5) +
  scale_color_manual(
    values = c("gray50", "#84BB25", "#1D94B7", "#6D3571", "#D64776", "#FBC740"),
    # name = "Variant ",
    drop = F
  ) +
  scale_fill_manual(
    values = c("gray50", "#84BB25", "#1D94B7", "#6D3571", "#D64776", "#FBC740"),
    # name = "Variant ",
    drop = F
  ) +
  scale_y_continuous(
    "Viral load (copies per person, per day)",
    labels = scales::unit_format(unit = "M")
  ) +
  scale_x_date(name = "Date", date_labels = "%b '%y") +
  labs(
    title = "COVID-19 Variants in Metro Plant Wastewater",
    caption = stringr::str_wrap(paste0(
      "Viral load of each variant is estimated by multiplying the total viral load (shown in gray) by the frequency of each variant. Frequencies do not always add to 100%. Shaded areas and lines are seven-day rolling averages. Points are daily data. Last sample date ",
      max(variant_data_date$date, na.rm = T), "."
    ))
  ) +
  guides(color = guide_legend(nrow = 2)) +
  theme_council(
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
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 24, hjust = 0.5),
    legend.position = "bottom",
    # legend.direction = "horizontal",
    plot.caption = element_text(
      size = 18,
      lineheight = 0.25,
      face = "italic",
      family = "Arial Narrow Italic"
    ),
    legend.justification = c(0, 0),
    legend.text = element_text(size = 16),
    # legend.title = element_text(size = 48),

    axis.title.y = element_text(size = 18, color = "#888888", vjust = 1, angle = 90),
    axis.title.x = element_text(size = 18, color = "#888888", vjust = 1),
    axis.text.y = element_text(size = 18, color = "#888888", vjust = 0),
    axis.text.x = element_text(size = 18, color = "#888888", vjust = 0),
    legend.key.height = unit(x = 10, units = "points"),
    legend.key.width = unit(10, units = "points"),
    legend.title = element_blank() # turn off legend title
  )


ggsave("fig/copies_by_variant_insta.png",
  load_varplot_ig,
  height = 1080, width = 1080,
  units = "px", dpi = 300
)
