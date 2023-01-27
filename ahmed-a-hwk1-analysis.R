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

rm(list = c("full.ma.data", "contract.service.area", "plan.premiums", "ma.data", "contract.info",
            "ma.data.2007", "ma.data.2008", "ma.data.2008a", "ma.data.2008b", "ma.data.2009", "ma.data.2009a",
            "ma.data.2009b", "ma.data.2010", "ma.data.2010a", "ma.data.2010b", "ma.data.2011", "ma.data.2011a",
            "ma.data.2011b", "ma.data.2012", "ma.data.2012a", "ma.data.2012b", "ma.data.2013", "ma.data.2013a",
            "ma.data.2013b", "ma.data.2014", "ma.data.2014a", "ma.data.2014b", "ma.data.2015", "ma.data.2015a", 
            "ma.data.2015b", "macd.data", "ma.pene.2008", "ma.pene.2009", "ma.pene.2010", "ma.pene.2011", 
            "ma.pene.2012", "ma.pene.2013", "ma.pene.2014", "ma.pene.2015", "macd.data", "macd.data.2007", "macd.data.2008",
            "macd.data.2009", "macd.data.2010", "macd.data.2011", "macd.data.2012", "macd.data.2013", "macd.data.2014",
            "macd.data.2015"))
rm(list = c("macd.data.2008a", "macd.data.2008b", "macd.data.2009a", "macd.data.2009b", "macd.data.2010a", 
            "macd.data.2010b", "macd.data.2011a", "macd.data.2011b", "macd.data.2012a", "macd.data.2012b", 
            "macd.data.2013a", "macd.data.2013b", "macd.data.2014a", "macd.data.2014b", "macd.data.2015a", "macd.data.2015b",
            "service.area", "service.area.2006", "service.area.2007", "service.area.2008", "service.area.2009", 
            "service.area.2010", "service.area.2011", "service.area.2012", "service.area.2013",
            "service.area.2014", "service.area.2015", "service.year", "plan.year", "ma.penetration", "pene.data", "ma.macd.data", "final.state"))
save.image("Hw1_workspace.Rdata")

#Premium data
plan.premiums<- readRDS("data/output/plan_premiums.rds")
final.data<- readRDS("data/output/final_ma_data.rds")

#6

fig.avg.premium<- final.data %>% group_by(year) %>% 
  summarise(prem = mean(!is.na(premium))) %>%
  ggplot(aes(x = year, y = prem)) +
  stat_summary(fun.y = "mean", geom = "bar") +
  labs(x = "Year", y="Average Premium", title = "Average Premium over Time") + 
  scale_y_continuous(labels = comma) + theme_bw()

#7

fig.0.premium<- final.data %>% filter(!is.na(premium)) %>% group_by(year) %>% 
  summarise(perc_0 = (sum(premium == 0)/n() * 100)) %>%
  ggplot(aes(x = year, y = perc_0)) +
  stat_summary(fun.y = "mean", geom = "bar") +
  labs(x = "Year", y="Percentage $0 Premium Plans", title = "Percentage of $0 Premium Plans over Time") + 
  scale_y_continuous(labels = comma) + theme_bw()
