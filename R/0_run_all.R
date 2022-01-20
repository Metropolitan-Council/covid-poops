
# remotes::install_github("Metropolitan-Council/councilR")
library(councilR)
library(janitor)
library(tictoc)

## Data processing -----
tictoc::tic("Data processing")

source("R/d_covid_cases.R", verbose = F)
source("R/d_load.R", verbose = F)
source("R/d_variants.R", verbose = F)

combined_data <-
  case_data %>%
  left_join(load_data, by = "date") %>%
  left_join(variant_data_new, by = "date")

write.csv(combined_data, "data/combined_data.csv", row.names = F)
write.csv(combined_data, "metc-wastewater-covid-monitor/data/combined_data.csv", row.names = F)
tictoc::toc()

## Figures -----

tictoc::tic("Render figures")

source("R/fig_cases_vs_load.R", verbose = F)
source("R/fig_variants.R", verbose = F)

tictoc::toc()
