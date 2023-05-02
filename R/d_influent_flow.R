library(janitor)
library(tidyverse)

raw_influent_flow_data <- read.csv(file.path(paste0("data/raw_influent_flow_data.csv")),
                                       colClasses = c("character", "numeric", "numeric", "numeric")) %>%
  janitor::clean_names() %>%
  mutate(date = as.Date(datetime, format("%m/%d/%Y"))) %>%
  select(-datetime)  


# Find Sharepoint Directory:
source("R/sharepointfilepath.R")

influent_flow_data <- read.csv(file.path(paste0(sharepath, "/BLU EMP SEN - SARS-CoV-2/raw_influent_flow_data_ma.csv")),
                               colClasses = c("character", "numeric", "numeric",
                                              "numeric")) %>%
  mutate(date = as.Date(date, format("%m/%d/%Y"))) %>%
  pivot_longer(cols = c("BLU", "SEN", "EMP"), names_to = "site", values_to = "mgd") 


write.csv(influent_flow_data, file.path(paste0(sharepath, "/BLU EMP SEN - SARS-CoV-2/clean_influent_flow_data.csv")))




influent_flow_data_pcg <- raw_influent_flow_data_pcg  %>%
  rename(MET = me_teprbcal0003)  %>%
  rename(BLU = bluab_total_influent_flow_mgd)  %>%
  rename(COT = swc_isp_fit0101_flow)  %>%
  pivot_longer(cols = c("BLU", "MET", "COT"), names_to = "site", values_to = "mgd") %>%
  filter( site != c("BLU"))

influent_flow_data <-influent_flow_data_15_min_RMF %>%
  # average by date:
  group_by(date, site) %>%
  # average across multiple samples:
  summarize(
    mgd = mean(mgd, na.rm = T),
  )%>%
  rbind( influent_flow_data_pcg)


write.csv(influent_flow_data, file.path(paste0("data/clean_influent_flow_data.csv")))