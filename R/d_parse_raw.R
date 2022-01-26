# script for grabbing raw data from the various excel spreadsheets and putting them in one place

load_sample_info <-read.csv("data/load_sample_info.csv") %>%
  mutate(sample_type = "copies")
variant_sample_info <- read.csv("data/variant_sample_info.csv") %>%
  mutate(sample_type = "variants")

sample_info <- rbind(load_sample_info, variant_sample_info) %>%
  mutate(sample_start_date  = as.Date(sample_start_date, format = "%m/%d/%Y")) %>%
  unique() %>%
  filter(!is.na(sample_id)) %>%
  arrange(sample_start_date, sample_id)

raw_data_files <- list.files("data/umgc-raw/", pattern = ".xlsx")

# Find the spreadsheet names we need ------
# there are lots of naming conventions for the data sheets!
# sorry, y"all, i still just love a good for loop :D   - Ashley
all_sheet_names_list <- list()

for (file_num in seq_along(raw_data_files)) {
  excel_file <- file.path('data/umgc-raw', raw_data_files[file_num])
  sheet_names <- readxl::excel_sheets(excel_file)
  all_sheet_names_list[[file_num]] <- data.frame(sheet_names)
}

all_sheet_names <-
  all_sheet_names_list %>%
  bind_rows() %>%
  unique() %>%
  arrange()

load_sheet_names <- 
  all_sheet_names %>%
  filter(!grepl("layout", sheet_names, ignore.case = T)) %>%
  filter(grepl("ddpcr", sheet_names, ignore.case = T)) %>%
  filter(!grepl("variant", sheet_names, ignore.case = T))

variant_sheet_names <- 
  all_sheet_names %>%
  filter(!grepl("layout", sheet_names, ignore.case = T)) %>%
  filter(grepl("ddpcr", sheet_names, ignore.case = T)) %>%
  filter(!grepl("N1", sheet_names, ignore.case = F)) %>%
  filter(grepl("variant", sheet_names, ignore.case = T))

# Extract! ------
load_data_list <- list()
variant_data_list <- list()

# get all the N1 & N2 load data sheets:
for (file_num in seq_along(raw_data_files)) {
  
  excel_file <- file.path('data/umgc-raw', raw_data_files[file_num])
  
  sheet_names <- readxl::excel_sheets(excel_file)
  
  found_load_sheet_names <-
    sheet_names[sheet_names %in% load_sheet_names$sheet_names]
  
  found_variant_sheet_names <-
    sheet_names[sheet_names %in% variant_sheet_names$sheet_names]

  if (length(found_load_sheet_names) == 1) {
    load_data_raw <- suppressMessages(read_excel(excel_file, found_load_sheet_names, na = c("", "No Call"))) %>%
      janitor::clean_names()
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
    variant_data_raw <- suppressMessages(read_excel(excel_file, found_variant_sheet_names, na = c("", "No Call"))) %>%
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
    variant_data_raw <- suppressMessages(read_excel(excel_file, found_variant_sheet_names[[1]], na = c("", "No Call"))) %>%
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

load_data_raw <- data.table::rbindlist(load_data_list, fill = T)

load_data_clean <- 
load_data_raw %>%
  mutate(conc_copies_m_l_of_input_sample = coalesce(
    conc_copies_m_l_of_input_sample, 
    conc_copies_ml_of_input_sample,
    conc_copies_u_l_of_input_sample,
    conc_input_copies_m_l,
    conc_input_sample_copies_m_l)) %>%
  select(-c(conc_copies_ml_of_input_sample,
         conc_copies_u_l_of_input_sample,
         conc_input_copies_m_l,
         conc_input_sample_copies_m_l)) %>%
  # rearrange columns: 
  relocate(conc_copies_m_l_of_input_sample, .after = "target") %>%
  relocate(copies_20m_l_well, .after = "target") %>%
  # get rid of completely NA columns:
  select_if(~sum(!is.na(.)) > 0) 


variant_data_raw <- data.table::rbindlist(variant_data_list, fill = T)

variant_data_clean <-
variant_data_raw %>%
  # make this column name match that of the load data:
  mutate(conc_copies_m_l_of_input_sample = coalesce(
    conc_copies_m_l_of_input_sample, 
    conc_copies_ml_of_input_sample,
    conc_input_copies_m_l,
    conc_copies_ul_of_sample_input)) %>%
  select(-c(conc_copies_ml_of_input_sample,
            conc_input_copies_m_l,
            conc_copies_ul_of_sample_input)) %>%
  # rearrange columns:
  relocate(conc_copies_m_l, .after = "target") %>%
  relocate(conc_copies_m_l_of_input_sample, .after = "target") %>%
  relocate(copies_20m_l_well, .after = "target") %>%
  # get rid of completely NA columns:
  select_if(~sum(!is.na(.)) > 0)
  
