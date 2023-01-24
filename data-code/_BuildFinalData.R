if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

#########################################################################
## Build plan-level dataset
#########################################################################
source("data-code/1_Plan_Data.R")
source("data-code/2_Plan_Characteristics.R")
source("data-code/3_Service_Areas.R")
source("data-code/4_Penetration_Files.R")

#########################################################################
## Organize final data
#########################################################################
full.ma.data <- read_rds("data/output/full_ma_data.rds")
contract.service.area <- read_rds("data/output/contract_service_area.rds")
ma.penetration.data <- read_rds("data/output/ma_penetration.rds")
plan.premiums <- read_rds("data/output/plan_premiums.rds") 

final.data <- full.ma.data %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!state %in% c("VI","PR","MP","GU","AS","") &
           snp == "No" &
           (planid < 800 | planid >= 900) &
           !is.na(planid) & !is.na(fips))

final.data <- final.data %>%
  left_join( ma.penetration.data %>% ungroup() %>% select(-ssa) %>%
               rename(state_long=state, county_long=county), 
             by=c("fips", "year"))

final.state <- final.data %>% 
  group_by(state) %>% 
  summarize(state_name=last(state_long, na.rm=TRUE))

final.data <- final.data %>%
  left_join(final.state,
            by=c("state"))

final.data <- final.data %>%
  left_join( plan.premiums,
             by=c("contractid","planid","state_name"="state","county","year")) 

final.data <- final.data %>% drop_na(avg_enrollment)

write_rds(final.data,"data/output/final_ma_data.rds")
