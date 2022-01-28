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

## LARGE -----
load_varplot <-
  copies_by_variant %>%
  filter(date >= "2021-01-01" & !variant == "Other") %>%
  ggplot(aes(x = date, y = copies, color = variant, fill = variant)) +
  
  # background - all counts
  geom_area(data = load_data, aes(x = date, y = copies_day_person_7day), color = 'gray50', fill = 'gray50', alpha = 0.35, lty = "blank") + 
  
  geom_line(aes(x = date, y = copies_7day, color = variant), size = 0.8) +
  geom_area(position = "identity", aes(x = date, y = copies_7day, group = variant, fill = variant, color = variant), alpha = 0.35, na.rm = T, lty = "blank") +
  
  geom_point() +
  scale_color_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776", "gray50"),
    name = "Variant "
  ) +
  scale_fill_manual(
    values = c("#84BB25", "#1D94B7", "#6D3571", "#D64776", "gray50"),
    name = "Variant "
  ) +
  
  
  scale_y_continuous(
    "Viral load (copies per person, per day)",
    labels = scales::unit_format(unit = "M")
  ) +  scale_x_date(name = "Date", breaks = "1 months", date_labels = "%b '%y") +
  labs(
    title = "COVID-19 Variants in Metro Plant Wastewater",
    caption = paste0("\nShaded areas and lines are seven-day rolling averages. Points are daily data.\n Gray area in background is the total viral load. \nLast sample date ", max(variant_data_new$date, na.rm = T), ".")
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
    axis.text.x = element_text(size =48),
    axis.title.x = element_text(size = 64),
    axis.text.y = element_text(size = 48, vjust = 0)
  )

load_varplot


ggsave("fig/copies_by_variant.png",
       load_varplot,
       scale = 1,
       height = 8.5, width = 11,
       units = "in", dpi = 300
)

