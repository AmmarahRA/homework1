if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

#1
full.ma.data<- readRDS("data/output/full_ma_data.rds")
#2
unique(full.ma.data$plan_type)
#3
knitr::kable(full.ma.data, col.names=c("2010","2011","2012","2013","2014","2015"),
             type="html", caption = "Plan Count by Year", booktabs = TRUE)
#4
full.ma.data2<- full.ma.data %>% 
  filter(snp=='Yes'& eghp=='Yes')

knitr::kable(full.ma.data2, col.names=c("2010","2011","2012","2013","2014","2015"),
             type="html", caption = "Plan Count by Year", booktabs = TRUE)

#Premium data
plan.premiums<- readRDS("data/output/plan_premiums.rds")

