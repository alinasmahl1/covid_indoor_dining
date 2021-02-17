rm(list=ls())
library(tidyverse)
library(tidycensus)
library(data.table)
# population (though I can pull that one bc we're using the pop from the 2019 census county population), 
# % aged <18, age>=18-64, aged >65, 
# % below FPL, % college educated
vars<-load_variables(year=2019, "acs1")
data<-get_acs(geography = "county",
                       variables=c(
                         # pop
                         "B01001_001",
                         # age
                         paste0("B01001_", sprintf("%03d", 1:49)),
                         # education
                         "B15003_022","B15003_023","B15003_024","B15003_025",
                         "B15003_001",
                         # poverty
                         "B17001_001","B17001_002"),
                       year=2019, survey = "acs1") %>% 
  select(GEOID, variable, estimate) %>% 
  spread(variable, estimate) %>% 
  mutate(GEOID=as.numeric(GEOID))
data<-data %>% 
  mutate(pct_age0017=rowSums(across(c(B01001_003:B01001_006,B01001_027:B01001_030)))/B01001_001,
         pct_age1864=rowSums(across(c(B01001_007:B01001_019,B01001_031:B01001_043)))/B01001_001,
         pct_age65plus=rowSums(across(c(B01001_020:B01001_025,B01001_044:B01001_049)))/B01001_001,
         total_pop=B01001_001,
         pct_college=rowSums(across(c(B15003_022:B15003_025)))/B15003_001,
         pct_poverty=B17001_002/B17001_001,
         GEOID=as.numeric(GEOID)) %>% 
  select(GEOID, total_pop, pct_age0017, pct_age1864, pct_age65plus,pct_poverty, pct_college) %>% 
  mutate_at(vars(matches("pct")), ~.*100) %>% 
  mutate(total_pop=total_pop/1000000)

treat<-c(42101,18097,6075, 55079)
comparison<-c(4013, 48453, 48113, 48201,  48029, 13121,45019)
data<-data %>% filter(GEOID%in%c(treat, comparison)) %>% 
  mutate(group=ifelse(GEOID%in%treat, "0Treatment", "1Control"))

table1<-data %>% group_by(group) %>% group_modify(~{
  #.x<-data %>% filter(group=="0Treatment")
  n<-nrow(.x)
  total_pop<-paste0(format(median(.x$total_pop), nsmall=2, digits=2), "[",
               format(min(.x$total_pop), nsmall=2, digits=2), "-",
               format(max(.x$total_pop), nsmall=2, digits=2),"]")
  age0017<-paste0(format(median(.x$pct_age0017), nsmall=1, digits=1), "[",
               format(min(.x$pct_age0017), nsmall=1, digits=1), "-",
               format(max(.x$pct_age0017), nsmall=1, digits=1),"]")
  age1864<-paste0(format(median(.x$pct_age1864), nsmall=1, digits=1), "[",
               format(min(.x$pct_age1864), nsmall=1, digits=1), "-",
               format(max(.x$pct_age1864), nsmall=1, digits=1),"]")
  age65plus<-paste0(format(median(.x$pct_age65plus), nsmall=1, digits=1), "[",
               format(min(.x$pct_age65plus), nsmall=1, digits=1), "-",
               format(max(.x$pct_age65plus), nsmall=1, digits=1),"]")
  poverty<-paste0(format(median(.x$pct_poverty), nsmall=1, digits=1), "[",
                    format(min(.x$pct_poverty), nsmall=1, digits=1), "-",
                    format(max(.x$pct_poverty), nsmall=1, digits=1),"]")
  college<-paste0(format(median(.x$pct_college), nsmall=1, digits=1), "[",
                  format(min(.x$pct_college), nsmall=1, digits=1), "-",
                  format(max(.x$pct_college), nsmall=1, digits=1),"]")
  data.frame(n=n, total_pop=total_pop,
             agebelow18=age0017, age1864=age1864, age65plus=age65plus,
             poverty=poverty, college=college)
}) %>% 
  gather(variable, value, -group) %>% 
  spread(group, value) %>% 
  mutate(variable=factor(variable, levels=c("n", "total_pop", "agebelow18", "age1864", "age65plus",
                                            "poverty", "college"))) %>% 
  arrange(variable)
fwrite(table1, file="results/table1.csv")
