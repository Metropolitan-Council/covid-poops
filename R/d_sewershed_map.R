library(sf)
library(tidyverse)
library(councilR)
# Open Geopackage

pooshed <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/util_sanitary_sewersheds/gpkg_util_sanitary_sewersheds.zip")

simple_shed <-
  pooshed %>%
  st_make_valid() 

simple_shed <- 
  simple_shed %>%
  group_by(WWTP) %>% # WWTP = wastewater treatment plant
  summarize(geom = st_union(geom)) %>%
  filter(!WWTP %in% c("WATER")) %>%
  filter(!is.na(WWTP)) %>%
  st_make_valid() %>%
  st_simplify()

saveRDS(simple_shed, file = "data/simple_wwtp_sewershed.rds")
