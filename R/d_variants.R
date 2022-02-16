library(readxl)
library(janitor)
library(tidyverse)

source("R/sharepointfilepath.R")

# read in raw -----
header1 <- read_excel(file.path(paste0(sharepath, "/1 - Update data/A- Metro data - load and variants.xlsx")),
                      sheet = "variants") %>% 
  janitor::clean_names() %>% 
  names()
header2 <- read_excel(file.path(paste0(sharepath, "/1 - Update data/A- Metro data - load and variants.xlsx")),
                      sheet = "variants", skip = 1) %>% 
  janitor::clean_names() %>% 
  names()
header <- paste0(header1, header2)

raw_variant_data <-
  read_excel(
    file.path(paste0(sharepath, "/1 - Update data/A- Metro data - load and variants.xlsx")),
    sheet = "variants",
    skip = 1
  ) %>%
  set_names(header) %>%
  select(1:18) %>%
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
      rename_all(~gsub("d80a_", "", .)) %>%
      # mysteriously, this column is reading in as character  - i think it's the scientific notation.
      mutate(frequency_of_mutant_allele = as.numeric(frequency_of_mutant_allele)),
    
    raw_variant_data %>%
      select(date, contains("l452r")) %>%
      mutate(mutation = "l452r") %>%
      rename_all(~gsub("l452r_", "", .)),
    
    raw_variant_data %>%
      select(date, contains("k417n")) %>%
      mutate(mutation = "k417n") %>%
      rename_all(~gsub("k417n_", "", .))
      
  )


variant_data_run <-
  variant_split %>%
  filter(!is.na(mutation) & !is.na(date) & !is.na(sample)) %>%
  # multiple runs per sample - need a unique ID
  group_by(date, sample, mutation) %>%
  mutate(run_num = row_number()) %>%
  mutate(date = as.Date(date)) %>%
  rename(sample_id = sample, frequency = frequency_of_mutant_allele) %>%
  pivot_wider(names_from = "mutation", values_from = "frequency") %>%
  # assign variants to mutations:
  mutate(
    `Alpha, Beta & Gamma` = n501y,
    Delta = l452r,
    `Omicron BA.2` = case_when(
      date >= "2022-02-10" &
        k417n > hv_69_70  &
        !is.na(hv_69_70) &
        !is.na(k417n)
      ~ k417n - hv_69_70,
      # omicron BA.2 not detected until recently - this date is a placeholder
      date >= "2022-02-10" &
        k417n < hv_69_70 &
        !is.na(hv_69_70) &
        !is.na(k417n)  ~ 0
    ),
    # turn this on when we start detecting BA.1/2: 
    # `Omicron BA.1` = case_when(
    #   date >= "2021-11-18" &
    #     k417n > hv_69_70 &
    #     !is.na(hv_69_70) &
    #     !is.na(k417n)
    #   ~ k417n - (1 - hv_69_70),
    #   date >= "2021-11-18" &
    #     (k417n < hv_69_70 |
    #        is.na(hv_69_70))
    #   ~ k417n
    # ),
    Omicron = case_when(date >= "2021-11-18" ~ k417n)
  ) %>%
  # for now, NA-out Omicron BA.2 where ratio of hv 69/70 to k417n is above 95%
  mutate(`Omicron BA.2` = ifelse(hv_69_70/k417n >= 0.95 & !is.na(`Omicron BA.2`), NA, `Omicron BA.2`)) %>%
  select(-d80a, -e484k, -hv_69_70, -n501y, -k417n, -l452r) %>%
  pivot_longer(
    cols = c(`Alpha, Beta & Gamma`, Delta, `Omicron`, `Omicron BA.2`),
    names_to = 'variant',
    values_to = 'frequency'
  )
  
variant_data <-
  variant_data_run %>%
  # average for each sample: 
  group_by(sample_id, date, variant) %>%
  summarize(frequency = mean(frequency, na.rm = T)) %>%
  filter(!is.na(sample_id) & !is.na(date)) %>%
  arrange(date)
  


# reshape-----
variant_data_new <-
  variant_data %>%
  filter(!variant == "Omicron BA.2") %>%
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


### Omicron and BA.2 sub-lineages tracking: hv69/70 to K417N
omi_ratio_data <-
variant_split %>%
  # get key mutations: 
  filter(mutation %in% c('k417n', 'hv_69_70')) %>%
  # format date:
  mutate(date = as.Date(date)) %>%
  # get run number for each sample:
  group_by(date, sample, mutation) %>%
  mutate(run_num = row_number()) %>%
  # rename cols: 
  rename(sample_id = sample, frequency = frequency_of_mutant_allele) %>%
  # wider - put mutations in own columns
  pivot_wider(names_from = "mutation", values_from = "frequency") %>%
  mutate(ratio = hv_69_70/k417n) %>%
  # get average ratio by sample, across runs: 
  group_by(sample_id, date) %>%
  summarize(ratio = mean(ratio, na.rm = T)) %>%
  # format date: 
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  # average by date, across samples: 
  group_by(date) %>%
  summarize(ratio = mean(ratio, na.rm = T)) %>%
  ungroup() %>%
  # rolling 7 day average of ratio: 
  complete(date = seq.Date(min(date, na.rm = T), max(date, na.rm = T), by = "days")
  ) %>%
  # interpolate missing values up to 3 days:
  mutate(ratio_gapfill = zoo::na.approx(ratio, maxgap = 2, na.rm = F)) %>%
  # now getting a rolling average with a 7-day window:
  mutate(ratio_7day = zoo::rollapply(ratio, 7, align = "right", mean, na.rm = T, partial = T, fill = "extend")) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(hover_text_ratio = paste0(
    format(date, "%b %d, %Y"), "<br>",
    "<b>", "HV 69-70:K417N ratio", "</b> ", round(ratio, digits = 3)
  )) %>%
  mutate(hover_text_ratio_7day = paste0(
    format(date, "%b %d, %Y"), "<br>",
    "<b>", "7-day average", "</b> ", round(ratio_7day, digits = 3)
  )) %>%
  mutate(across(where(is.numeric), round, digits = 6)) %>%
  # just after Omicron showed up:
  filter(date >= '2022-01-01') %>%
  # only where not NA:
  filter(!is.na(ratio))

write.csv( omi_ratio_data, 'data/omi_ratio_data.csv',row.names = F)  

