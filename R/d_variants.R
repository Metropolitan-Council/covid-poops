library(readxl)
library(janitor)
library(tidyverse)

# read in raw -----
header1 <- read_excel("data/raw-variant-data.xlsx") %>% 
  janitor::clean_names() %>% 
  names()
header2 <- read_excel("data/raw-variant-data.xlsx", skip = 1) %>% 
  janitor::clean_names() %>% 
  names()
header <- paste0(header1, header2)

raw_variant_data <-
  read_excel(
    "data/raw-variant-data.xlsx",
    skip = 1
  ) %>%
  set_names(header) %>%
  # get rid of trailing numbers in header: 
  rename_all(~gsub("_[[:digit:]]$|_[[:digit:]][[:digit:]]$", "", .)) %>% 
  rename_all(~gsub("[[:digit:]]sample|[[:digit:]][[:digit:]]sample", "sample", .)) %>% 
  rename_all(~gsub("allele_[[:digit:]]|allele_[[:digit:]][[:digit:]]", "allele", .)) %>% 
  rename_all(~gsub("[[:digit:]]frequency|[[:digit:]][[:digit:]]frequency", "frequency", .)) %>% 
  rename(date = n501y_sample_start_date) %>%
  select(-contains("sample_start_date"))

# tidy up - split format of spreadsheet to long-form
# notice, sample IDs do not always line up exactly across different columns -- will need to 
# re-align this programmatically by using pivot_longer() and bind_rows() here. 
variant_split <-
  bind_rows(
    raw_variant_data %>%
      select(date, contains("n501y")) %>%
      mutate(mutation = "n501y") %>%
      rename_all(~gsub("n501y_", "", .)),
    
    raw_variant_data %>%
      select(date, contains("hv_69_70")) %>%
      mutate(mutation = "hv_69_70") %>%
      rename_all(~gsub("hv_69_70_", "", .)),
    
    raw_variant_data %>%
      select(date, contains("e484k")) %>%
      mutate(mutation = "e484k") %>%
      rename_all(~gsub("e484k_", "", .)),
    
    raw_variant_data %>%
      select(date, contains("d80a")) %>%
      mutate(mutation = "d80a") %>%
      rename_all(~gsub("d80a_", "", .)),
    
    raw_variant_data %>%
      select(date, contains("l452r")) %>%
      mutate(mutation = "l452r") %>%
      rename_all(~gsub("l452r_", "", .)),
    
    raw_variant_data %>%
      select(date, contains("k417n")) %>%
      mutate(mutation = "k417n") %>%
      rename_all(~gsub("k417n_", "", .))
      
  )


variant_data <-
  variant_split %>%
  rename(sample_id = sample, frequency = frequency_of_mutant_allele) %>%
  # sometimes multiple runs on same sample - average across these.
  group_by(sample_id, date, mutation) %>%
  summarize(frequency = mean(frequency, na.rm = T)) %>%
  filter(!is.na(sample_id) & !is.na(date)) %>%
  pivot_wider(names_from = "mutation", values_from = "frequency") %>%
  # assign variants to mutations:
  mutate(
    `Alpha, Beta & Gamma` = n501y,
    Delta = l452r,
    `Omicron BA.1` = case_when(date >= "2021-11-18" ~ k417n,
                               TRUE ~ 0),
    `Omicron BA.2` = case_when(date >= '2021-11-18' ~ `Omicron BA.1` - hv_69_70,
                               TRUE ~ 0)
  ) %>%
  select(-d80a, -e484k, -hv_69_70, -n501y, -k417n, -l452r) %>%
  pivot_longer(cols = c(`Alpha, Beta & Gamma`, Delta, `Omicron BA.1`, `Omicron BA.2`),
               names_to = 'variant', values_to = 'frequency')


# reshape-----
variant_data_new <-
  variant_data %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  # average by date
  group_by(date, variant) %>%
  summarize(frequency = mean(frequency, na.rm = T)) %>%
  ungroup() %>%
  # rolling 7 day average, by variant type
  complete(variant,
    date = seq.Date(min(date, na.rm = T), max(date, na.rm = T), by = "days")
  ) %>%
  group_by(variant) %>%
  # interpolate missing values up to 3 days:
  mutate(frequency_gapfill = zoo::na.approx(frequency, maxgap = 2, na.rm = F)) %>%
  # now getting a rolling average with a 7-day window:
  mutate(frequency_7day = zoo::rollapply(frequency_gapfill, 7, align = "right", mean, na.rm = T, partial = T, fill = "extend")) %>%
  ungroup() %>%
  arrange(date, variant) %>%
  filter(!variant == "Other") %>%
  mutate(hover_text_variant = paste0(
    format(date, "%b %d, %Y"), "<br>",
    "<b>", variant, "</b> ", round(frequency * 100, digits = 2), "%"
  )) %>%
  mutate(across(where(is.numeric), round, digits = 6)) 

write.csv(variant_data_new, "data/clean_variant_data.csv", row.names = F)
write.csv(variant_data_new, "metc-wastewater-covid-monitor/data/clean_variant_data.csv", row.names = F)
