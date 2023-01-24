if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

#1
full.ma.data<- readRDS("data/output/full_ma_data.rds")
#2
unique(full.ma.data$plan_type)

#plan.premiums<- readRDS(data/plan.premiums.rds)