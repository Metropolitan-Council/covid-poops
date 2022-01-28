copies_by_variant <-
  variant_data_new %>%
  left_join(load_data) %>%
  select(date, copies_day_person_M_mn, variant, frequency) %>%
  filter(!is.na(frequency) & !is.na(copies_day_person_M_mn)) %>%
  mutate(copies_variant = frequency * copies_day_person_M_mn) %>%
  pivot_wider(names_from = 'variant',  values_from = copies_variant) %>%
  rowwise() %>%
  mutate(`Other` = copies_day_person_M_mn -
           sum(
             c(`Alpha, Beta & Gamma`, Delta, `Omicron BA.1`, `Omicron BA.2`),
             na.rm = T
           )) %>%
  select(-copies_day_person_M_mn) %>%
  pivot_longer(
    cols = c(`Alpha, Beta & Gamma`, Delta, `Omicron BA.1`, `Omicron BA.2`, Other),
    names_to = 'Variant',
    values_to = "copies"
  ) %>%
  filter(!is.na(copies))

