library(sf)
library(tidyverse)
library(councilR)
# Open Geopackage

pooshed <- councilR::import_from_gpkg('https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/util_sanitary_sewersheds/gpkg_util_sanitary_sewersheds.zip')
  
simple_shed <-
pooshed %>%
  group_by(WWTP) %>% # WWTP = wastewater treatment plant
  summarize(geom = st_union(geom)) %>%
  st_simplify()


nrow(pooshed)

