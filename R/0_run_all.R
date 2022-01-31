
# remotes::install_github("Metropolitan-Council/councilR")
library(councilR)
library(janitor)
library(tictoc)

## Data processing -----
tictoc::tic("Data processing")

source("R/d_covid_cases.R", verbose = F)
source("R/d_load.R", verbose = F)
source("R/d_variants.R", verbose = F)
source("R/d_copies_by_variant.R", verbose = F)

tictoc::toc()

## Figures -----

tictoc::tic("Render figures")

source("R/fig_cases_vs_load.R", verbose = F)
source("R/fig_variants.R", verbose = F)
source("R/fig_copies_by_variant.R", verbose = F)

tictoc::toc()
