
# remotes::install_github("Metropolitan-Council/councilR")
library(councilR)
library(janitor)
library(tictoc)
library(pagedown)

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

## Report -----

tictoc::tic("Generating Markdown Report")

library(rmarkdown)

source("R/sharepointfilepath.R")

rmarkdown::render("report/wastewater-monitoring-report.Rmd")
rmarkdown::render("report/wastewater-monitoring-report.Rmd",
                  output_file = file.path(paste0(sharepath, '/1 - Update data/wastewater-monitoring-report.html')))
chrome_print('report/wastewater-monitoring-report.html', output = 'report/wastewater-monitoring-report.pdf')
chrome_print('report/wastewater-monitoring-report.html', file.path(paste0(sharepath, '/1 - Update data/wastewater-monitoring-report.pdf')))

rmarkdown::render("report/wastewater-monitoring-report-developing-version.Rmd")
rmarkdown::render("report/wastewater-monitoring-report-developing-version.Rmd",
                  output_file = file.path(paste0(sharepath, '/1 - Update data/wastewater-monitoring-report-developing-version.html')))
chrome_print('report/wastewater-monitoring-report-developing-version.html', 
             output = 'report/wastewater-monitoring-report-developing-version.pdf')
chrome_print('report/wastewater-monitoring-report-developing-version.html', 
             file.path(paste0(sharepath, '/1 - Update data/wastewater-monitoring-report-developing-version.pdf')))

tictoc::toc()
