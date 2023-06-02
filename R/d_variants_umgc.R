library(readxl)
library(janitor)
library(tidyverse)

source("R/sharepointfilepath.R")

# read in raw -----

raw_variant_data_umgc <- read_excel(file.path(paste0(sharepath, "/BLU EMP SEN - SARS-CoV-2/UMGC_BLU_EMP_SEN_Data_Report.xlsx")),
                               sheet = "Variants" ) %>%
  janitor::clean_names() 



variant_split_umgc <- raw_variant_data_umgc  %>%
  mutate(date = as.Date(collection_date) - 1) %>%
  select(-collection_date) %>% 
  mutate( site = substr(sample_id, 1, 3)) %>% 
  filter ( site == "MET") %>%
  mutate( amino_acid_target = ifelse( amino_acid_target == "HV69/70 Del", "HV 69-70", ifelse(amino_acid_target == "ORF1b.P1953P" ,"P1953P", ifelse(amino_acid_target == "ORF1ab:N4060S", "N4060S", ifelse(amino_acid_target == "ORF1b:Y264H", "Y264H", amino_acid_target))))) %>%
  mutate( mutation = gsub(" Mutant", "" ,amino_acid_target)) %>% 
  select (-amino_acid_target) %>%
  select (-site, -mutant_conc_copies_per_u_l, -wt_conc_copies_per_u_l, -umgc_notes, -mutant_conc_copies_per_u_l_raw, -wt_conc_copies_per_u_l_raw) 

variant_data_run_umgc <-
  variant_split_umgc %>%
  filter(!is.na(mutation) & !is.na(date) & !is.na(sample_id)) %>%
  # multiple runs per sample - need a unique ID
  group_by(date, sample_id, mutation) %>%
  mutate(run_num = row_number()) %>%
  mutate(date = as.Date(date)) %>%
  #rename(sample_id = sample, frequency = frequency_of_mutant_allele) %>%
  pivot_wider(names_from = "mutation", values_from = "frequency") %>%
  janitor::clean_names() %>%
  # assign variants to mutations:
  mutate(
    #`Alpha, Beta & Gamma` = n501y,
    Delta = case_when(
      # Only use l452r for delta until 4/25/22
      date <= "2022-04-25"
      ~ l452r 
      # The rest of the time, Delta will be NA.
    ),
    `Omicron BA.2 (Excluding BA.2.12.1)` = case_when(
      
      # Assigning values for BA2:
      # We start detecting BA 2 on 1/1:
      date >= "2022-01-01" &
        date < "2022-04-12" &
        # only calculate when k417N is greater than than hv 69/70:
        
        k417n > hv_69_70 &
        # only calculate when hv69/70 and K417N data are present:
        !is.na(hv_69_70) & !is.na(k417n)
      # omicron BA2 = k417N minus frequency of hv69/70
      ~ k417n - hv_69_70,
      
      
      # Assigning values for BA2 After Detecting BA.2.12.1
      date >= "2022-04-12" &
        date <= "2022-05-30" &
        # only calculate when k417N is greater than than hv 69/70:
        k417n > hv_69_70 &
        # only calculate when hv69/70 and K417N data are present:
        !is.na(hv_69_70) & !is.na(k417n)
      # omicron BA2 = k417N minus frequency of hv69/70 and l452q
      ~ k417n - hv_69_70 - l452q,
      
      date >= "2022-05-31" & 
        date < "2022-08-31"
      ~ 0,
      
      # date >= "2022-08-31" &
      #   # only calculate when k417N is greater than than hv 69/70:
      #   k417n > hv_69_70 &
      #   # only calculate when hv69/70 and K417N data are present:
      #   !is.na(hv_69_70) & !is.na(k417n)
      # ~ k417n - hv_69_70,
      # date >= "2022-08-31" 
      # ~ 0,
      # Assigning zeros for BA2:
      date >= "2022-01-01" & 
        date < "2022-08-31" &
        # only assign a zero when k417N is less than than hv 69/70:
        
        k417n < hv_69_70 &
        # only assign a zero when both hv69/70 or K417N data are present:
        !is.na(hv_69_70) & !is.na(k417n) ~ 0
      
      # The rest of the time, BA 2 will be NA.
    ),
    # turn this on when we start detecting BA.1/2:
    `Omicron BA.1, BA.4, BA.5 Total` = case_when(
      
      # before we detect BA2, it's just the K417 N frequency:
      date >= "2021-11-18" &
        date < "2022-01-01"
      ~ k417n,
      
      # After we detect BA2, it's the K417 N frequency minus BA2 frequency:
      date >= "2021-11-18" &
        date >= "2022-01-01" & 
        date <= "2022-05-15"  &
        k417n > hv_69_70 &
        !is.na(hv_69_70) &
        !is.na(k417n)
      
      ~ k417n - (k417n - hv_69_70)
    ),
    'Omicron BA.1' = case_when (
      date >= '2022-05-16'
      ~ t95i),
    
    'Omicron BA.2.12.1' = case_when (
      date >= '2022-04-12' & 
        date <= "2022-05-30" 
      ~ l452q,
      date >= "2022-05-31" &
        date < "2022-08-31"
      ~ k417n - hv_69_70
      # ,
      # date >= "2022-08-31"
      # ~ 0
      
    ),
    "Omicron BA.4 and BA.5" = case_when(
      date >= "2022-05-16" & 
        date <= "2022-05-30" 
      ~ hv_69_70 - t95i
    ),
    "Omicron BA.4" = case_when(
      date >= "2022-05-31"  
      ~ l11f
    ),
    "Omicron BA.5 (Excluding BQ.1)" = case_when(
      date >= "2022-05-31" &
        date < "2022-10-11"
      ~ d3n,
      date >= "2022-10-11" &
        date < "2023-02-14" &
        d3n - e316d >= 0
      ~ d3n - e316d,
      date >= "2022-10-11" &
        date < "2023-02-14" &
        d3n - e316d < 0
      ~ 0,
      date >= "2023-02-14"
      ~ 0
    ),
    "Omicron BQ.1" = case_when(
      date >= "2022-10-11" &
        date < "2023-02-14"
      ~ e316d,
      date >= "2023-02-14"
      ~ d3n
    ),
    "XBB" = case_when(
      date >= "2022-12-13" &
        date < "2023-02-14"
      ~ k417n - hv_69_70 - f157l,
      date >= "2023-02-14"
      ~ p1953p
    ),
    "Omicron BA.2.75" = case_when(
      date >= "2022-09-01" &
        date < "2023-02-14"
      ~ f157l,
      date >= "2023-02-14"
      ~ n4060s
    )
  ) %>%
  # option to NA-out Omicron BA.2 where ratio of hv 69/70 to k417n is above 95%
  # mutate(`Omicron BA.2` = ifelse(hv_69_70/k417n >= 0.95 & !is.na(`Omicron BA.2`), NA, `Omicron BA.2`)) %>%
  select(-hv_69_70, -k417n, -l452r, -l452q, -t95i, -q493r, -l11f, -d3n, -f157l, -n460k, -r346t, -hv_69_70, -e316d, -f157l, -n4060s, -p1953p, -s959p) %>%
  pivot_longer(
    cols = c(Delta, `Omicron BA.1, BA.4, BA.5 Total`, `Omicron BA.2 (Excluding BA.2.12.1)`, `Omicron BA.1`, `Omicron BA.2.12.1`, `Omicron BA.4 and BA.5`, `Omicron BA.4`, `Omicron BA.5 (Excluding BQ.1)`, `Omicron BQ.1`, "XBB", "Omicron BA.2.75"),
    names_to = "variant",
    values_to = "frequency"
  )


variant_data_sample_umgc <-
  variant_data_run_umgc %>%
  # average for each sample, across runs:
  group_by(sample_id, date, variant) %>%
  summarize(frequency = mean(frequency, na.rm = T)) %>%
  filter(!is.na(sample_id) & !is.na(date)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  arrange(date)

# reshape-----

seq_date_umgc <- function(x) seq(min(x, na.rm = T), max(x, na.rm = T), by = "day")
all_dates_umgc <- seq_date(filter(variant_data_sample)$date)

variant_data_date_umgc <-
  variant_data_sample_umgc %>%
  # turn this on when we detect Omicron BA.2:
  # filter(!variant == "Omicron BA.2") %>%
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
  mutate(across(where(is.numeric), round, digits = 6)) %>%
  mutate(
    frequency_7day = ifelse(
      # Only use l452r for delta until 4/25/22
      variant == "Delta" &
        date > "2022-04-25", NA, frequency_7day )) %>%
  mutate(
    frequency_7day = ifelse(
      variant == "Omicron BA.1, BA.4, BA.5 Total" &
        date >= "2022-05-16", NA, frequency_7day )) 




