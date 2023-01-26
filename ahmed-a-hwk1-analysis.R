if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr, scales)

#1
full.ma.data<- readRDS("data/output/full_ma_data.rds")
total_obs<- as.numeric(count(full.ma.data %>% ungroup()))

#2
plan_type_table<- full.ma.data %>% group_by(plan_type) %>% count() %>% arrange(-n)
unique(full.ma.data$plan_type)

#3
plan.type.year1<- full.ma.data %>% group_by(plan_type, year) %>% count() %>% 
  arrange(year, -n) %>% filter(plan_type!="NA")

pe.year1<- pivot_wider(plan.type.year1, names_from = "year", values_from = "n", names_prefix = "Count_")

#4
full.ma.data2<- full.ma.data %>% 
  filter(snp == 'No' & eghp == 'No' & !(planid %in% 800:899))

plan.type.year2<- full.ma.data2 %>% group_by(plan_type, year) %>% count() %>% 
  arrange(year, -n) %>% filter(plan_type!="NA")

pe.year2<- pivot_wider(plan.type.year2, names_from = "year", values_from = "n", names_prefix = "Count_")

#5
contract.service.area <- readRDS("data/output/contract_service_area.rds")

final.data1 <- full.ma.data2 %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) 

fig.avg.enrollment<- final.data1 %>% filter(!is.na(avg_enrollment)) %>% group_by(fips, year) %>% 
  select(fips, year, avg_enrollment) %>%
  summarise(all_enroll = sum(avg_enrollment)) %>%
  ggplot(aes(x = year, y = all_enroll)) +
  stat_summary(fun.y = "mean", geom = "bar") +
  labs(x = "Year", y="Average Enrollees", title = "Average Enrollees per County") + 
  scale_y_continuous(labels = comma) + theme_bw()

#rm(list = c("full.ma.data", "contract.service.area", "plan.premiums", "final.plan",))
#save.image("Hw1_workspace.Rdata")

#Premium data
plan.premiums<- readRDS("data/output/plan_premiums.rds")
final.data<- readRDS("data/output/final_ma_data.rds")

#6
fig.avg.premium<- final.data %>% 
  group_by(year)%>% 
  summarize(prem = mean(!is.na(premium)))%>%
  ggplot( aes(year, prem))+
  geom_line(color = 'red')+
  labs( title = 'Average premium over time', x = 'Year', y = 'Average Premium')+
  theme_minimal()

#7

fig.0premium<- final.data %>% 
  filter(!is.na(premium))%>% 
  group_by(year)%>% 
  summarize(perc_0 = (sum(premium == 0)/n() * 100))%>% 
  ggplot( aes(year, perc_0))+
  geom_line(color = 'blue')+
  labs( title = 'Percentage of $0 Premium Plans over time', x = 'Year', y = 'Percentage of $0 Premium Plans')+
  theme_minimal()
