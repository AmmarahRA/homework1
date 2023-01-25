if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

#1
full.ma.data<- readRDS("data/output/full_ma_data.rds")
#2
unique(full.ma.data$plan_type)
#3
#knitr::kable(full.ma.data, col.names=c("2010","2011","2012","2013","2014","2015"),
             #type="html", caption = "Plan Count by Year", booktabs = TRUE)

full.ma.data %>% 
  group_by(year, plan_type)%>% 
  summarize(n_under_plan_type = n())%>% 
  spread(year, n_under_plan_type)

#4
full.ma.data2<- full.ma.data %>% 
  filter(snp == 'No' & eghp == 'No' & !(planid %in% 800:899))

full.ma.data2 %>% 
  group_by(year, plan_type)%>% 
  summarize(n_under_plan_type = n())%>% 
  spread(year, n_under_plan_type)

#5
contract.service.area <- readRDS("data/output/contract_service_area.rds")

final.data1 <- full.ma.data %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!state %in% c("VI","PR","MP","GU","AS","") &
           snp == "No" &
           (planid < 800 | planid >= 900) &
           !is.na(planid) & !is.na(fips))

final.data1 %>% 
  group_by(year,county)%>% 
  summarize(count = n())%>%
  ggplot(aes( x = year, y  = count, group = county))+
  geom_line()+
  labs( title = 'Number of enrollees per county', x = 'Year', y = 'Number of enrollees')+
  theme_minimal()

#Premium data
plan.premiums<- readRDS("data/output/plan_premiums.rds")
final.data<- readRDS("data/output/final_ma_data.rds")

#6
final.data %>% 
  group_by(year)%>% 
  summarize(prem = mean(!is.na(premium)))%>%
  ggplot( aes(year, prem))+
  geom_line(color = 'red')+
  labs( title = 'Average premium over time', x = 'Year', y = 'Average Premium')+
  theme_minimal()

#7

final.data %>% 
  filter(!is.na(premium))%>% 
  group_by(year)%>% 
  summarize(perc_0 = (sum(premium == 0)/n() * 100))%>% 
  ggplot( aes(year, perc_0))+
  geom_line(color = 'blue')+
  labs( title = 'Percentage of $0 Premium Plans over time', x = 'Year', y = 'Percentage of $0 Premium Plans')+
  theme_minimal()
