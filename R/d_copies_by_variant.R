copies_by_variant <-
  variant_data_new %>%
  select(date, variant, frequency) %>%
  left_join(load_data %>% select(date, copies_day_person_M_mn)) %>%
  filter(!is.na(frequency) & !is.na(copies_day_person_M_mn)) %>%
  mutate(copies_variant = frequency * copies_day_person_M_mn) %>%
  pivot_wider(
    names_from = "variant",
    values_from = copies_variant,
    id_cols = c(date, copies_day_person_M_mn)
  ) %>%
  rowwise() %>%
  mutate(`Other` = copies_day_person_M_mn -
           sum(
             c(`Alpha, Beta & Gamma`, Delta, `Omicron BA.1`, `Omicron BA.2`),
             na.rm = T
           )) %>%
  # select(-copies_day_person_M_mn) %>%
  pivot_longer(
    cols = c(`Alpha, Beta & Gamma`, Delta, `Omicron BA.1`, `Omicron BA.2`, Other),
    names_to = "variant",
    values_to = "copies"
  ) %>%
  group_by(variant) %>%
  complete(variant,
           date = seq.Date(min(date, na.rm = T), max(date, na.rm = T), by = "days")) %>%
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
  filter(copies > 0) %>%
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
  mutate(across(where(is.numeric), round, digits = 6))
