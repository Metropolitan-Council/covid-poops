# Extracts raw data from excel sheets and compiles into clean .csvs

# toolbox -----
library(tidyverse)
library(readxl)
library(janitor)

# Sample IDs ----------
load_sample_info <- read.csv("data/load_sample_info.csv") %>%
  mutate(sample_type = "copies")

variant_sample_info <- read.csv("data/variant_sample_info.csv") %>%
  mutate(sample_type = "variants")

sample_info <- rbind(load_sample_info, variant_sample_info) %>%
  filter(!is.na(sample_start_date)) %>%
  mutate(sample_start_date  = as.Date(sample_start_date, format = "%m/%d/%Y")) %>%
  unique() %>%
  filter(!is.na(sample_id)) %>%
  # now un-nest some combined samples (+)
  mutate(sample_id_single = strsplit(sample_id, "[+]")) %>%
  unnest(sample_id_single) %>%
  relocate(sample_id_single, .after = "sample_id") %>%
  arrange(sample_start_date, sample_id_single)


# Our excel workbooks ----
raw_data_files <- list.files("data/umgc-raw/", pattern = ".xlsx")

# Search for the worksheets we need within each workbook ----

# an empty list to hold the worksheet names:
all_sheet_names_list <- list()

# for every excel file, extract all worksheet names:
for (file_num in seq_along(raw_data_files)) {
  excel_file <- file.path('data/umgc-raw', raw_data_files[file_num])
  sheet_names <- readxl::excel_sheets(excel_file)
  all_sheet_names_list[[file_num]] <- data.frame(sheet_names)
}

# bind up the worksheet names:
all_sheet_names <-
  all_sheet_names_list %>%
  bind_rows() %>%
  unique() %>%
  arrange()

# find all the ways "load" spreadsheets (that measure N1 & N2 concentration) are named:
load_sheet_names <-
  all_sheet_names %>%
  # layout sheets contain the layouts of the trays
  filter(!grepl("layout", sheet_names, ignore.case = T)) %>%
  filter(grepl("ddpcr", sheet_names, ignore.case = T)) %>%
  filter(!grepl("variant", sheet_names, ignore.case = T))

# find all the ways "variant" spreadsheets are named:
variant_sheet_names <-
  all_sheet_names %>%
  filter(!grepl("layout", sheet_names, ignore.case = T)) %>%
  filter(grepl("ddpcr", sheet_names, ignore.case = T)) %>%
  filter(!grepl("N1", sheet_names, ignore.case = F)) %>%
  filter(grepl("variant", sheet_names, ignore.case = T))

# Extract the sheets we need ------

# empty lists to hold our data:
load_data_list <- list()
variant_data_list <- list()

# a for loop to read each excel workbook, find the variant & load worksheets, extract and clean
for (file_num in seq_along(raw_data_files)) {
  excel_file <- file.path('data/umgc-raw', raw_data_files[file_num])
  
  sheet_names <- readxl::excel_sheets(excel_file)
  
  found_load_sheet_names <-
    sheet_names[sheet_names %in% load_sheet_names$sheet_names]
  
  found_variant_sheet_names <-
    sheet_names[sheet_names %in% variant_sheet_names$sheet_names]
  
  if (length(found_load_sheet_names) == 1) {
    load_data_raw <-
      suppressMessages(read_excel(excel_file, found_load_sheet_names, na = c("", "No Call"))) %>%
      janitor::clean_names() %>%
      mutate(sheet = paste0(excel_file, "----", found_load_sheet_names))
    load_data_list[[file_num]] <- load_data_raw
  } else if (length(found_load_sheet_names) > 1) {
    message(
      paste0(
        "Duplicate load data for ",
        raw_data_files[file_num],
        "-- duplicate sheet names: ",
        paste0(found_load_sheet_names, collapse = ", ")
      )
    )
  } else {
    message(paste0(
      "No load data for ",
      raw_data_files[file_num],
      "-- all sheet names: ",
      paste0(sheet_names, collapse = ", ")
    ))
  }
  
  if (length(found_variant_sheet_names) == 1) {
    variant_data_raw <-
      suppressMessages(read_excel(excel_file, found_variant_sheet_names, na = c("", "No Call"))) %>%
      janitor::clean_names()
    variant_data_list[[file_num]] <- variant_data_raw
  } else if (length(found_variant_sheet_names) > 1) {
    message(
      paste0(
        "Duplicate variant data for ",
        raw_data_files[file_num],
        "-- duplicate sheet names: ",
        paste0(found_variant_sheet_names, collapse = ", "),
        "--selecting sheet ",
        found_variant_sheet_names[[1]]
      )
    )
    variant_data_raw <-
      suppressMessages(read_excel(excel_file, found_variant_sheet_names[[1]], na = c("", "No Call"))) %>%
      janitor::clean_names()
    variant_data_list[[file_num]] <- variant_data_raw
  } else {
    message(paste0(
      "No variant data for ",
      raw_data_files[file_num],
      "-- all sheet names: ",
      paste0(sheet_names, collapse = ", ")
    ))
  }
}

# Bind the data together ----
## Load --------
load_data <-
  data.table::rbindlist(load_data_list, fill = T) %>%
  # many ways of naming "concentration in the input sample"
  mutate(
    conc_copies_m_l_of_input_sample = coalesce(
      conc_copies_m_l_of_input_sample,
      conc_copies_ml_of_input_sample,
      conc_copies_u_l_of_input_sample,
      conc_input_copies_m_l,
      conc_input_sample_copies_m_l
    )
  ) %>%
  select(
    -c(
      conc_copies_ml_of_input_sample,
      conc_copies_u_l_of_input_sample,
      conc_input_copies_m_l,
      conc_input_sample_copies_m_l
    )
  ) %>%
  # rearrange columns:
  relocate(conc_copies_m_l_of_input_sample, .after = "target") %>%
  relocate(copies_20m_l_well, .after = "target") %>%
  # get rid of completely NA columns:
  select_if(~ sum(!is.na(.)) > 0)

## Variants -------
variant_data <-
  data.table::rbindlist(variant_data_list, fill = T) %>%
  # many ways of naming "concentration in the input sample" - make this match the load data
  mutate(
    conc_copies_m_l_of_input_sample = coalesce(
      conc_copies_m_l_of_input_sample,
      conc_copies_ml_of_input_sample,
      conc_input_copies_m_l,
      conc_copies_ul_of_sample_input
    )
  ) %>%
  select(
    -c(
      conc_copies_ml_of_input_sample,
      conc_input_copies_m_l,
      conc_copies_ul_of_sample_input
    )
  ) %>%
  # rearrange columns:
  relocate(conc_copies_m_l, .after = "target") %>%
  relocate(conc_copies_m_l_of_input_sample, .after = "target") %>%
  relocate(copies_20m_l_well, .after = "target") %>%
  # get rid of completely NA columns:
  select_if(~ sum(!is.na(.)) > 0)

# Checks---------
## Samples with missing data----
sample_info %>%
  left_join(variant_data_clean %>% select(sample, target),
            by = c("sample_id" = "sample")) %>%
  left_join(
    load_data_clean %>% select(sample, target),
    by = c("sample_id" = "sample"),
    suffix = c("_variant", "_load")
  ) %>%
  filter(is.na(target_load) & is.na(target_variant)) %>%
  arrange(sample_start_date, sample_type) %>%
  View()

## Load data comparison
old_load_data <- read_excel("data/raw-load-data.xlsx",
                                             col_types = c(
                                               "text", "numeric", "numeric",
                                               "date", "skip", "skip", "skip",
                                               "skip", "skip", "skip", "skip",
                                               "skip", "skip", "skip", "skip", "skip",
                                               "skip", "skip", "skip", "skip"
                                             ),
                                             skip = 1
) %>%
  janitor::clean_names() %>%
  mutate(sample_start_date = as.Date(sample_start_date)) %>%
  # rename(N1_gene_l = copies_l_3, N2_gene_l = copies_l_4) %>%
  pivot_longer(
    cols = c("n1_copies_u_l", "n2_copies_u_l"), names_to = "target",
    values_to = "clean_conc"
  ) %>%
  mutate(target = str_remove(target, "_copies_u_l")) %>%
  mutate(target = toupper(target)) %>%
  rename(sample = sample_name)

load_data %>%
  select(well, sample, target, conc_copies_m_l_of_input_sample) %>%
  left_join(old_load_data) %>%
  filter(conc_copies_m_l_of_input_sample < 1000) %>%
  mutate(monthyr = lubridate::floor_date(sample_start_date, "month", "%B %y")) %>%
  ggplot(aes(x = clean_conc, y = conc_copies_m_l_of_input_sample, color =  as.factor(monthyr))) + 
  geom_point() + 
  geom_smooth(method = lm, se = F)

# here are the samples that do not match:
load_data %>%
  select(well, sample, target, conc_copies_m_l_of_input_sample) %>%
  group_by(sample, target) %>%
  summarize(new_conc = mean(conc_copies_m_l_of_input_sample)) %>%
  left_join(old_load_data) %>%
  # filter(!new_conc == clean_conc) %>%
  filter(!is.na(new_conc) & !is.na(clean_conc)) %>%
  mutate(monthyr = lubridate::floor_date(sample_start_date, "month", "%B %y")) %>%
  mutate(fac = clean_conc/new_conc) %>%
  # filter(fac > 1.01 | fac < 0.98) %>%
  # filter(monthyr == "2020-11-01") %>%
  View()
  
  
  group_by(monthyr) %>%
  tally() %>%
  View()
  
