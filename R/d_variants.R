library(readxl)
library(janitor)
library(tidyverse)

source("R/sharepointfilepath.R")

# read in raw -----
header1 <- suppressMessages(read_excel(file.path(paste0(sharepath, "/1 - Update data/A- Metro data - load and variants.xlsx")),
  sheet = "variants"
)) %>%
  janitor::clean_names() %>%
  names()
header2 <- suppressMessages(read_excel(file.path(paste0(sharepath, "/1 - Update data/A- Metro data - load and variants.xlsx")),
  sheet = "variants", skip = 1
)) %>%
  janitor::clean_names() %>%
  names()
header <- paste0(header1, header2)

raw_variant_data <-
  suppressMessages(read_excel(
    file.path(paste0(sharepath, "/1 - Update data/A- Metro data - load and variants.xlsx")),
    sheet = "variants",
    skip = 1
  )) %>%
  set_names(header) %>%
  select(1:34) %>%
  # get rid of trailing numbers in header:
  rename_all(~ gsub("_[[:digit:]]$|_[[:digit:]][[:digit:]]$", "", .)) %>%
  rename_all(~ gsub("[[:digit:]]sample|[[:digit:]][[:digit:]]sample", "sample", .)) %>%
  rename_all(~ gsub("allele_[[:digit:]]|allele_[[:digit:]][[:digit:]]", "allele", .)) %>%
  rename_all(~ gsub("[[:digit:]]frequency|[[:digit:]][[:digit:]]frequency", "frequency", .)) %>%
  rename(date = n501y_sample_start_date) %>%
  select(-contains("sample_start_date")) %>%
  mutate(date = as.Date(date))

# tidy up - split format of spreadsheet to long-form
# notice, sample IDs do not always line up exactly across different columns -- will need to
# re-align this programmatically by using pivot_longer() and bind_rows() here.
variant_split <-
  bind_rows(
    raw_variant_data %>%
      select(date, contains("n501y")) %>%
      mutate(mutation = "n501y") %>%
      rename_all(~ gsub("n501y_", "", .)),
    raw_variant_data %>%
      select(date, contains("hv_69_70")) %>%
      mutate(mutation = "hv_69_70") %>%
      rename_all(~ gsub("hv_69_70_", "", .)),
    raw_variant_data %>%
      select(date, contains("e484k")) %>%
      mutate(mutation = "e484k") %>%
      rename_all(~ gsub("e484k_", "", .)),
    raw_variant_data %>%
      select(date, contains("d80a")) %>%
      mutate(mutation = "d80a") %>%
      rename_all(~ gsub("d80a_", "", .)) %>%
      # mysteriously, this column is reading in as character  - i think it's the scientific notation.
      mutate(frequency_of_mutant_allele = as.numeric(frequency_of_mutant_allele)),
    raw_variant_data %>%
      select(date, contains("l452r")) %>%
      mutate(mutation = "l452r") %>%
      rename_all(~ gsub("l452r_", "", .)) %>%
      # mysteriously, this column is reading in as character
      mutate(frequency_of_mutant_allele = as.numeric(frequency_of_mutant_allele)),
    raw_variant_data %>%
      select(date, contains("k417n")) %>%
      mutate(mutation = "k417n") %>%
      rename_all(~ gsub("k417n_", "", .)),
    raw_variant_data %>%
      select(date, contains("l452q")) %>%
      mutate(mutation = "l452q") %>%
      rename_all(~ gsub("l452q_", "", .)) %>%
      mutate(sample = as.character(sample)) %>%
      # mysteriously, this column is reading in as character
      mutate(frequency_of_mutant_allele = as.numeric(frequency_of_mutant_allele)),
    raw_variant_data %>%
      select(date, contains("t95i")) %>%
      mutate(mutation = "t95i") %>%
      rename_all(~ gsub("t95i_", "", .)) %>%
      mutate(sample = as.character(sample)) %>%
      mutate(frequency_of_mutant_allele = as.numeric(frequency_of_mutant_allele)),
    raw_variant_data %>%
      select(date, contains("xsample"), contains("d3n")) %>%
      mutate(mutation = "d3n") %>%
      rename_all(~ gsub("xsample", "sample", .)) %>%
      rename_all(~ gsub("d3n_ba_", "", .)) %>%
      mutate(sample = as.character(sample)),
    raw_variant_data %>%
      select(date, contains("xsample"), contains("l11f")) %>%
      mutate(mutation = "l11f") %>%
      rename_all(~ gsub("xsample", "sample", .)) %>%
      rename_all(~ gsub("l11f_ba_", "", .)) %>%
      mutate(sample = as.character(sample)),
    raw_variant_data %>%
      select(date, contains("xsample"), contains("E136D")) %>%
      mutate(mutation = "e136d") %>%
      rename_all(~ gsub("xsample", "sample", .)) %>%
      rename_all(~ gsub("e136d", "", .)) %>%
      mutate(sample = as.character(sample)),
    raw_variant_data %>%
      select(date, contains("xsample"), contains("F157L")) %>%
      mutate(mutation = "f157l") %>%
      rename_all(~ gsub("xsample", "sample", .)) %>%
      rename_all(~ gsub("f157l_ba_2_", "", .)) %>%
      mutate(sample = as.character(sample))
  )


variant_data_run <-
  variant_split %>%
  filter(!is.na(mutation) & !is.na(date) & !is.na(sample)) %>%
  # multiple runs per sample - need a unique ID
  group_by(date, sample, mutation) %>%
  mutate(run_num = row_number()) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  rename(sample_id = sample, frequency = frequency_of_mutant_allele) %>%
  pivot_wider(names_from = "mutation", values_from = "frequency") %>%
  # assign variants to mutations:
  mutate(
    `Alpha, Beta & Gamma` = n501y,
    Delta = case_when(
      # Only use l452r for delta until 4/25/22
      date <= "2022-04-25"
      ~ l452r
      # The rest of the time, Delta will be NA.
    ),
    # turn this on when we start detecting BA.1/2:
    `Omicron BA.1` = case_when(

      # before we detect BA2, it's just the K417 N frequency:
      date >= "2021-11-18" &
        date < "2022-01-01"
      ~ k417n,

      # After we detect BA2, it's the K417 N frequency minus BA2 frequency:
      date >= "2021-11-18" &
        date >= "2022-01-01" &
        date <= "2022-04-25" &
        k417n > hv_69_70 &
        !is.na(hv_69_70) &
        !is.na(k417n)

      ~ k417n - (k417n - hv_69_70),

      # After we start possibliy detecting BA.4 or BA.5, it's the hv_69_70 minus L452R frequency
      date > "2022-04-25" &
        date < "2022-05-10" &
        hv_69_70 >= l452r
      ~ hv_69_70 - l452r,
      date > "2022-04-25" &
        date < "2022-05-10" &
        hv_69_70 < l452r
      ~ 0,

      # After we start detecting BA.4 and BA.5 and have the frequency of T95I, it is the the frequency of T95I
      date >= "2022-05-10"
      ~ t95i
    ),
    "Omicron BA.2.12.1" = case_when(
      date >= "2022-04-12" &
        date <= "2022-05-30"
      ~ l452q,
      date >= "2022-05-31" &
        date < "2022-08-31"
      ~ k417n - hv_69_70,
      date >= "2022-08-31"
      ~ 0
    ),
    "Omicron BA.4 and BA.5" = case_when(
      date >= "2022-05-10" &
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
      date >= "2022-10-11"
      ~ d3n - e136d
    ),
    "Omicron BQ.1" = case_when(
      date >= "2022-10-11"
      ~ e136d
    ),
    "XBB" = case_when(
      date >= "2022-12-14"
      ~ k417n - hv_69_70 - f157l
    ),
    "Omicron BA.2.75" = case_when(
      date >= "2022-09-01"
      ~ f157l
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
      #   date < "2022-09-01" &
      #   # only calculate when k417N is greater than than hv 69/70:
      #   k417n > hv_69_70 &
      #   # only calculate when hv69/70 and K417N data are present:
      #   !is.na(hv_69_70) & !is.na(k417n)
      #   ~ k417n - hv_69_70,
      date >= "2022-08-31"
      ~ 0,
      # Assigning zeros for BA2:
      date >= "2022-01-01" &
        # only assign a zero when k417N is less than than hv 69/70:

        k417n < hv_69_70 &
        # only assign a zero when both hv69/70 or K417N data are present:
        !is.na(hv_69_70) & !is.na(k417n) ~ 0

      # The rest of the time, BA 2 will be NA.
    )
  ) %>%
  # option to NA-out Omicron BA.2 where ratio of hv 69/70 to k417n is above 95%
  # mutate(`Omicron BA.2` = ifelse(hv_69_70/k417n >= 0.95 & !is.na(`Omicron BA.2`), NA, `Omicron BA.2`)) %>%
  select(-d80a, -e484k, -hv_69_70, -n501y, -k417n, -l452r, -l452q, -t95i, -l11f, -d3n, -e136d, -f157l) %>%
  pivot_longer(
    cols = c(`Alpha, Beta & Gamma`, Delta, `Omicron BA.1`, "Omicron BA.2.12.1", "Omicron BA.4 and BA.5", "Omicron BA.4", "Omicron BA.5 (Excluding BQ.1)", "Omicron BQ.1", "XBB", "Omicron BA.2.75", `Omicron BA.2 (Excluding BA.2.12.1)`),
    names_to = "variant",
    values_to = "frequency"
  )

variant_data_sample <-
  variant_data_run %>%
  # average for each sample, across runs:
  group_by(sample_id, date, variant) %>%
  summarize(frequency = mean(frequency, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(sample_id) & !is.na(date)) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .))) %>%
  arrange(date)



# reshape-----
variant_data_date <-
  variant_data_sample %>%
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
  mutate(frequency_7day = zoo::rollapply(frequency_gapfill, 7, align = "right", mean, na.rm = T, partial = F, fill = "extend")) %>%
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
        date > "2022-04-25", NA, frequency_7day
    )
  ) %>%
  mutate(
    frequency_7day = ifelse(
      # Total BA.4 and BA.5 only until data that separates the two out is available.
      variant == "Omicron BA.4 and BA.5" &
        date > "2022-05-31", NA, frequency_7day
    )
  )


write.csv(variant_data_date, "data/clean_variant_data.csv", row.names = F)
write.csv(variant_data_date, "metc-wastewater-covid-monitor/data/clean_variant_data.csv", row.names = F)
