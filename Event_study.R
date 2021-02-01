# Event MODEL 

rm(list=ls())
#Import all libraries 
library(data.table)
library(tidyverse)
library(tidycensus)
library(lubridate)
library(fastDummies)
library(plm) 
library(ggplot2)
library(hrbrthemes)
library(zoo)
library(ggpubr)
library(stargazer)
library(arm)
library(sandwich)
library(MASS)
library(lmtest)
library(emmeans)
select<-dplyr::select
load("daily_count.Rdata")



#IMPORT COUNTY LEVEL DATA
county_cases<-fread("data/time_series_covid19_confirmed_US.csv")
county_deaths<-fread("data/time_series_covid19_deaths_US.csv")
#limit dataset to our counties/cities-- select cities wherever available 
# for austin- city is code 48015 and coutny is 48453-- using county (bc don't trust city)
cities<-c(42101,18097,6075, 55079, 4013, 48453, 48113, 48201,  48029, 13121,45019)

#get county population estimates (Downloaded from here)-https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
population<-fread("data/co-est2019-annres.csv", header=TRUE)

#names of counties of interest
cities_pop<-c(".Philadelphia County, Pennsylvania", ".Marion County, Indiana", 
              ".San Francisco County, California", ".Milwaukee County, Wisconsin", ".Maricopa County, Arizona", 
              ".Travis County, Texas", ".Dallas County, Texas", ".Harris County, Texas", 
              ".Bexar County, Texas", ".Fulton County, Georgia", ".Charleston County, South Carolina")

#limit population dataset to counties we're exploring 
population1<-population%>%
  filter(V1%in% cities_pop)%>%
  #add fips
  mutate(FIPS=case_when(
    V1==".Philadelphia County, Pennsylvania"~42101,
    V1==".Marion County, Indiana"~18097,
    V1==".San Francisco County, California"~6075, 
    V1==".Milwaukee County, Wisconsin"~55079, 
    V1==".Maricopa County, Arizona"~4013, 
    V1==".Travis County, Texas"~48453, 
    V1==".Dallas County, Texas"~48113, 
    V1==".Harris County, Texas"~48201, 
    V1==".Bexar County, Texas"~48029,
    V1==".Fulton County, Georgia"~13121, 
    V1==".Charleston County, South Carolina"~45019))%>%
  rename(pop='2019')%>%
  dplyr::select(pop, FIPS)


#create weekly vars for event model 
#create weekly vars for event model 
event_model<-county_cases1%>%
  mutate(treat_start=case_when( 
    #Atlanta
    Admin2=="Fulton"~ "2020-04-27",
    Admin2=="Philadelphia"~"2020-06-26",
    Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
    Admin2=="San Francisco"~"2020-08-31", 
    Admin2=="Milwaukee"~"2020-05-14", 
    #indianapolis
    Admin2=="Marion"~"2020-05-11",
    Admin2=="Charleston"~"2020-05-11"), 
    treat_start=as.Date(treat_start, "%Y-%m-%d"), 
    #create end of Stay at home var         
    stay_start=case_when(
      #Atlanta
      Admin2=="Fulton"~ "2020-05-01",
      Admin2=="Philadelphia"~"2020-06-05",
      Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-01",
      Admin2=="Maricopa" ~"2020-05-16",
      Admin2=="San Francisco"~"2020-05-14", 
      Admin2=="Milwaukee"~"2020-05-09", 
      #indianapolis
      Admin2=="Marion"~"2020-05-13",
      Admin2=="Charleston"~"2020-05-04"), 
    stay_start=as.Date(stay_start, "%Y-%m-%d"), 
    #create mask mandate var     
    mask_start=case_when(#Atlanta
      Admin2=="Fulton"~ "2020-07-08",
      Admin2=="Philadelphia"~"2020-07-01",
      Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-07-03",
      #phoenix
      Admin2=="Maricopa"~"2020-06-20",
      Admin2=="San Francisco"~"2020-04-17", 
      Admin2=="Milwaukee"~"2020-05-14", 
      #indianapolis
      Admin2=="Marion"~"2020-07-09",
      Admin2=="Charleston"~"2020-07-01"), 
    mask_start=as.Date(mask_start, "%Y-%m-%d"), 
    #create end of eviction ban
    eviction_start=case_when(
      #Atlanta
      Admin2=="Fulton"~ "2020-03-16",
      Admin2=="Philadelphia"~"2020-03-18",
      Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-03-19",
      Admin2=="San Francisco"~"2020-03-01", 
      Admin2=="Milwaukee"~"2020-03-27", 
      #indianapolis
      Admin2=="Marion"~"2020-03-19",
      Admin2=="Charleston"~"2020-03-19"),
    eviction_start=as.Date(eviction_start, "%Y-%m-%d"),
    #var for end of eviction ban           
    eviction_end=case_when(
      #Atlanta
      Admin2=="Fulton"~ "2020-10-31",
      Admin2=="Philadelphia"~"2020-12-31",
      Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-19",
      Admin2=="San Francisco"~"2020-12-31", 
      Admin2=="Milwaukee"~"2020-05-27", 
      #indianapolis
      Admin2=="Marion"~"2020-08-14",
      Admin2=="Charleston"~"2020-05-15"),
    eviction_end=as.Date(eviction_end, "%Y-%m-%d"),
    #create var that normalizes start date (1st treat day 1 = day 1 for all cities)
    time= as.numeric(difftime(date,treat_start, units = c("days"))),
    stay_days= as.integer(difftime(date,stay_start, units = c("days"))), 
    mask_days=as.integer(difftime(date,mask_start, units = c("days"))), 
    evict_days=as.integer(difftime(date,eviction_start, units = c("days"))), 
    cal_week=factor(week(date)))%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)


#create var that normalizes start date (1st treat day 1 = day 1 for all cities)
event_model$time= as.numeric(difftime(event_model$date,event_model$treat_start, units = c("days")))
event_model$weeks= as.integer(difftime(event_model$date,event_model$treat_start, units = c("weeks"), round()))


#not efficient but dif date uses calendar weeks, so wasn't working
# NOTE: OUR CONTROL HERE IS REOPENING!! (not staying closed)
#this is currently sensitivity 3, but probably will be main model 

#also tried to enter the weeks_prior and weeks_post as a factor into the regresssion
# and it created weird weeks_o periods. So I created dummies instead....

#Event model sensitivity 2: limit study period to 4 weeks pre and 8 weeks (2 weeks lag + 6 weeks) (or 6 pre and 6 post)
event_model3<-event_model%>%
  filter(time>=-28 & time<=56)%>%
  mutate(weeks_prior=as.factor(case_when(
    treat1==0 & time %in% c(-1:-7)~1,
    treat1==0 & time %in% c(-7:-14)~2, 
    treat1==0 & time %in% c(-15:-21)~3, 
    treat1==0 & time %in% c(-22:-28)~4, 
    TRUE~0)), 
    weeks_post=as.factor(case_when( 
      treat1==0 & time %in% c(0:7)~1, 
      treat1==0 & time %in% c(8:14)~2, 
      treat1==0 & time %in% c(15:21)~3, 
      treat1==0 & time %in% c(22:28)~4, 
      treat1==0 & time %in% c(29:35)~5, 
      treat1==0 & time %in% c(36:42)~6, 
      treat1==0 & time %in% c(42:49)~7, 
      treat1==0 & time %in% c(50:56)~8, 
      TRUE~0)),  
    #ordinal var for weeks since stay at home lifted
    #would expect beta to go in opposite direction bc cases likely increase after opening
    stay_week=as.factor(case_when(
      stay_days <0 ~0,
      stay_days %in% c(0:7)~1, 
      stay_days %in% c(8:14)~2, 
      stay_days %in% c(15:21)~3, 
      stay_days %in% c(22:28)~4, 
      stay_days >28 ~ 5)), 
    #creat var for weeks since mask mandate 
    mask_week=as.factor(case_when(
      mask_days <0 ~0, 
      mask_days %in% c(0:7)~1, 
      mask_days %in% c(8:14)~2, 
      mask_days %in% c(15:21)~3, 
      mask_days %in% c(22:28)~4, 
      mask_days >28 ~5)), 
    #create var for weeks since eviction 
    evict_days=case_when(date>eviction_end~-1, 
                         TRUE~as.numeric(evict_days)),     
    evict_week=as.factor(case_when(
      evict_days <0 ~0, 
      evict_days %in% c(0:7)~1, 
      evict_days %in% c(8:14)~2, 
      evict_days %in% c(15:21)~3, 
      evict_days %in% c(22:28)~4, 
      evict_days >28 ~5)))%>%
  #make weeks prior and weeks post dummy vars 
  dummy_cols(select_columns = 'weeks_prior')%>%
  dummy_cols(select_columns='weeks_post')

save(event_model3, file="event_model3.Rdata")


##########################################################################
#ANALYSIS 
#######################################################
#Event Model 
#######################################################
#base model 
#just weeks
#pretty sure adding each week_prio and weeks_post is the least efficient way to add to regression, but couldn't figure out alterntive. 
f<-as.formula(paste0("daily_count~", 
                     paste("weeks_prior_", 1:4, collapse="+", sep=""), "+",
                     paste("weeks_post_", 1:8, collapse="+", sep=""), 
                     "+offset(log(pop/100000))"))
summary(mod_e1<-glm.nb(f,  data=event_model3))
stargazer(mod_e1, apply.coef = exp, type='text')
#robust standard errors clustered at city level 
rse_mod_e1<-exp(coeftest(mod_e1, vcov = vcovHC,  cluster= ~cities))
rci_mod_e1<-exp(coefci(mod_e1, vcov = vcovHC,  cluster= ~cities))
all_mod_e1<-bind_cols(rownames(rse_mod_e1), as.data.frame(rse_mod_e1[,1]), as.data.frame(rci_mod_e1[,1:2])) %>% 
  slice(-1) %>% 
  rename(variable=1, est=2, lci=3, uci=4) %>% 
  mutate(time=as.numeric(gsub("weeks|prior|post|_", "", variable)),
         time=time*ifelse(grepl("post", variable), 1, -1)) %>% 
  select(time, est, lci, uci) %>% 
  add_row(time=0, est=1, lci=NA, uci=NA) %>% 
  arrange(time)
ggplot(all_mod_e1, aes(x=time, y=est)) +
  geom_ribbon(aes(ymin=lci, ymax=uci))+
  geom_line() +
  theme_bw()

#adding city fixed effects
#think I can just add in
summary(mod_e2<-glm.nb(daily_count~weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8  + cities + offset(log(pop/100000)),  data=event_model3))
rse_mod_e2<-exp(coeftest(mod_e2, vcov = vcovHC,  cluster= ~cities))
rci_mod_e2<-exp(coefci(mod_e2, vcov = vcovHC,  cluster= ~cities))
all_mod_e2<-bind_cols(rownames(rse_mod_e2), as.data.frame(rse_mod_e2[,1]), 
                      as.data.frame(rci_mod_e2[,1:2])) %>% 
  slice(-1) %>% 
  rename(variable=1, est=2, lci=3, uci=4) %>% 
  mutate(time=as.numeric(gsub("weeks|prior|post|_", "", variable)),
         time=time*ifelse(grepl("post", variable), 1, -1)) %>% 
  select(time, est, lci, uci) %>% 
  add_row(time=0, est=1, lci=NA, uci=NA) %>% 
  arrange(time)
ggplot(all_mod_e2, aes(x=time, y=est)) +
  geom_ribbon(aes(ymin=lci, ymax=uci))+
  geom_line() +
  theme_bw()

#adding calendar week fixed effects 
summary(mod_e3<-glm.nb(daily_count~weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8  + factor(cal_week) + factor(cities) + offset(log(pop/100000)),  data=event_model3))
rse_mod_e3<-exp(coeftest(mod_e3, vcov = vcovHC,  cluster= ~cities))
rci_mod_e3<-exp(coefci(mod_e3, vcov = vcovHC,  cluster= ~cities))

#adding mask mandate (factor) 
summary(mod_e4a<-glm.nb(daily_count~weeks_prior +weeks_post+ cal_week + cities +mask_week + offset(log(pop/100000)),  data=event_model3))
rse_mod_e4a<-exp(coeftest(mod_e4a, vcov = vcovHC,  cluster= ~cities))
rci_mod_e4a<-exp(coefci(mod_e4a, vcov = vcovHC,  cluster= ~cities))
levels(event_model2$cities)

#adding stay at home order (factor)
summary(mod_e4b<-glm.nb(daily_count~weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8  + factor(cal_week) + factor(cities) +mask_week + stay_week+ offset(log(pop/100000)),  data=event_model3))
rse_mod_e4b<-exp(coeftest(mod_e4b, vcov = vcovHC,  cluster= ~cities))
rci_mod_e4b<-exp(coefci(mod_e4b, vcov = vcovHC,  cluster= ~cities))

#adding eviction ban (factor)
summary(mod_e4c<-glm.nb(daily_count~ weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 + factor(cal_week) + cities +mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model3))
rse_mod_e4c<-exp(coeftest(mod_e4c, vcov = vcovHC,  cluster= ~cities))
rci_mod_e4c<-exp(coefci(mod_e4c, vcov = vcovHC,  cluster= ~cities))

event_models<-list(mod_e1, mod_e2, mod_e3, mod_e4a, mod_e4b, mod_e4c)
stargazer(event_models, apply.coef=exp, type="text", title="IRRs event model", out="results/table2_event.txt")
rse_mod_e<-list(rse_mod_e1, rse_mod_e2, rse_mod_e3, rse_mod_e4a, rse_mod_e4b, rse_mod_e4c)
rci_mod_e<-list(rci_mod_e1, rci_mod_e2, rci_mod_e3, rci_mod_e4a, rci_mod_e4b, rci_mod_e4c)

