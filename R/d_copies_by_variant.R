library(tidyverse)
library(zoo)

# merge the variant and load data together
# samples for load and samples for variants are different, so we'll match at the date level.
copies_by_variant <-
  # take the variant data, by date:
  variant_data_date %>%
  select(date, variant, frequency) %>%
  # match to the load data, by date:
  left_join(load_data %>% select(date, copies_day_person_M_mn)) %>%
  # get complete data:
  filter(!is.na(frequency) & !is.na(copies_day_person_M_mn)) %>%
  # approx. number of copies of the variant is equal to the frequency of that variant, times the total # of copies:
  mutate(copies_variant = frequency * copies_day_person_M_mn) %>%
  # put this in wide format, with variant in columns:
  pivot_wider(
    names_from = "variant",
    values_from = copies_variant,
    id_cols = c(date, copies_day_person_M_mn)
  ) %>%
  # now, a row-wise calculation to figure out how much is "other" variants
  rowwise() %>%
  mutate(`Other` = copies_day_person_M_mn -
    sum(
      c(
        `Alpha, Beta & Gamma`, Delta, `Omicron BA.1`,
        `Omicron BA.2`
        #,
        #'Omicron BA.2.12.1'
      ), # turn this on when we start reporting BA.2.12.1
      
      na.rm = T
    )) %>%
  # pivot back to long format:
  pivot_longer(
    cols = c(
      `Alpha, Beta & Gamma`, Delta, `Omicron BA.1`,
      `Omicron BA.2`, # turn this on when we start reporting BA2
      #'Omicron BA.2.12.1', # turn this on when we start reporting BA2
      Other
    ),
    names_to = "variant",
    values_to = "copies"
  ) %>%
  # for each variant, get a seven-day running average:
  group_by(variant) %>%
  complete(
    date = seq.Date(min(date, na.rm = T), max(date, na.rm = T), by = "days")
  ) %>%
  # interpolate missing values up to 3 days:
  mutate(copies_gapfill = zoo::na.approx(copies, maxgap = 2, na.rm = F)) %>%
  # now getting a rolling average with a 7-day window:
  mutate(
    copies_7day = zoo::rollapply(
      copies_gapfill,
      7,
      align = "right",
      mean,
      na.rm = T,
      partial = T,
      fill = "extend"
    )
  ) %>%
  ungroup() %>%
  # now clean up and add data labels:
  arrange(date, variant) %>%
  mutate(hover_text_variant = paste0(
    format(date, "%b %d, %Y"),
    "<br>",
    "<b>",
    variant,
    "</b> ",
    round(copies, digits = 2),
    "M copies"
  )) %>%
  mutate(across(where(is.numeric), round, digits = 6)) %>%
  filter(!is.na(copies)) %>%
  mutate(hover_text_variant_7day = paste0(
    format(date, "%b %d, %Y"),
    "<br>",
    "<b>",
    variant,
    "</b> ",
    round(copies_7day, digits = 2),
    "M copies, 7-day average"
  )) %>%
  mutate(across(where(is.numeric), round, digits = 6)) %>%
  filter(!is.na(copies)) %>%
  filter(!variant == "Other")

write.csv(copies_by_variant, "data/copies_by_variant.csv", row.names = F)
write.csv(copies_by_variant, "metc-wastewater-covid-monitor/data/copies_by_variant.csv", row.names = F)
