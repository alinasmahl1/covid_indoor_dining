###***********************Data Management_ indoor dining & preemption**************************

#*Version 1 started Nov 20, 2020 by Alina Schnake-Mahl
#*Imports, cleans, & merges COVID data + policy data

#Import all libraries 
library(data.table)
library(tidyverse)
library(tidycensus)
library(lubridate)
library(fastDummies)
library(plm)

#IMPORT COUNTY LEVEL DATA
county_cases<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
county_deaths<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
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

#limit population dataset to counties/cities in study
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

### MAIN ANALYSIS -Using date of state reopening as date of treatment for treatment cities
#limit cases to counties of interest and time periods of study period

county_cases1<-county_cases%>%
  filter(FIPS %in% cities)%>%
  #create treatment/control var 
  mutate(treat1=case_when(Admin2%in% c("Philadelphia", "Marion", "San Francisco", "Milwaukee")~1, 
                                    Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                                                 "Harris", "Fulton", "Charleston")~0))%>%
  dplyr::select(UID, FIPS, Admin2, Province_State, '3/1/20':'11/1/20', treat1)%>%
  pivot_longer(!c(UID, FIPS, Admin2, Province_State,treat1), names_to="date", values_to="cases")%>%         
  #get daily count by taking difference from one day to next
  mutate(cities=as.factor(case_when(FIPS==42101~"Philadelphia",
                                    FIPS==18097~"Indianapolis", 
                                    FIPS==6075~"San Francisco", 
                                    FIPS==55079~"Milwaukee", 
                                    FIPS==4013~"Pheonix", 
                                    FIPS==48453~"Austin", 
                                    FIPS==48113~"Dallas", 
                                    FIPS==48201~"Houston", 
                                    FIPS==48029~ "San Antonio", 
                                    FIPS==13121~"Atlanta", 
                                    FIPS==45019~"Charleston")))%>%
  group_by(FIPS)%>%
  mutate(daily_count = c(cases[1],diff(cases)))%>%
  mutate(date=as.Date(date, "%m/%d/%y"),
         treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
                               Admin2=="Philadelphia"~"2020-06-26",
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-01",
                               Admin2=="Maricopa"~"2020-05-11",
                               Admin2=="Milwaukee"~"2020-05-14", 
                               Admin2=="San Francisco"~"2020-08-31", 
                               Admin2=="Marion"~"2020-05-11",
                               Admin2=="Charleston"~"2020-05-11"),
         treat_start=as.Date(treat_start, "%Y-%m-%d"), 
         stay_start=case_when(
           #Atlanta
           Admin2=="Fulton"~ "2020-05-01",
           Admin2=="Philadelphia"~"2020-06-05",
           Admin2=="Travis"~"2020-07-15",
           Admin2=="Dallas"~"2020-05-15",
           Admin2=="Harris"~ "2020-05-20",
           Admin2=="Bexar"~ "2020-06-05",
           Admin2=="Maricopa" ~"2020-05-16",
           #San Francisco
           Admin2=="San Francisco"~"2020-05-18", 
           #Milwaukee
           Admin2=="Milwaukee"~"2020-06-05", 
           #indianapolis
           Admin2=="Marion"~"2020-05-15",
           Admin2=="Charleston"~"2020-05-31"), 
         stay_start=as.Date(stay_start, "%Y-%m-%d"), 
         #create mask mandate var     
         mask_start=case_when(#Atlanta
           Admin2=="Fulton"~ "2020-07-08",
           Admin2=="Philadelphia"~"2020-07-01",
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-07-03",
           #phoenix
           Admin2=="Maricopa"~"2020-06-20",
           #San Francisco
           Admin2=="San Francisco"~"2020-04-17", 
           #Milwaukee
           Admin2=="Milwaukee"~"2020-07-16", 
           #indianapolis
           Admin2=="Marion"~"2020-07-09",
           Admin2=="Charleston"~"2020-07-01"), 
         mask_start=as.Date(mask_start, "%Y-%m-%d"), 
         #create end of eviction ban
         eviction_start=case_when(
           #Atlanta
           Admin2=="Fulton"~ "2020-03-16",
           Admin2=="Philadelphia"~"2020-03-18",
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-03-19",
           Admin2=="Maricopa"~"2020-03-24",
           #San Francisco
           Admin2=="San Francisco"~"2020-03-01", 
           #Milwaukee
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
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-19",
           Admin2=="Maricopa"~"2020-10-31",
           #San Francisco- no end during study
           Admin2=="San Francisco"~"2020-12-31", 
           #Milwaukee
           Admin2=="Milwaukee"~"2020-05-26", 
           #indianapolis
           Admin2=="Marion"~"2020-08-14",
           Admin2=="Charleston"~"2020-05-15"),
         eviction_end=as.Date(eviction_end, "%Y-%m-%d"),
         #create var that normalizes start date (1st treat day 1 = day 1 for all cities)
         #create var that normalizes start date (1st treat day 1 = day 1 for all cities)
         time= as.numeric(difftime(date,treat_start, units = c("days"))),
         stay_days= as.integer(difftime(date,stay_start, units = c("days"))), 
         mask_days=as.integer(difftime(date,mask_start, units = c("days"))), 
         evict_days=as.integer(difftime(date,eviction_start, units = c("days"))),
         evict_days_end=as.integer(difftime(date,eviction_end, units=c("days"))), 
         cal_week=factor(week(date)))
county_cases1$daily_count[county_cases1$daily_count< 0] <- 0


hist(county_cases1$daily_count)

#option 1, using 14 day lag for all NPIs - w/ 2 weeks pre + 2 weeks post as the pre period. 
county_cases2<-county_cases1%>%
  mutate(pre_post=ifelse(time>=14,1,0), 
         at_home=ifelse(stay_days<=14, 1, 0), 
         mask_mandate=ifelse(mask_days>=14, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=14, 1, 0), 
         evict_end=ifelse(evict_days_end>=14, 1, 0), 
         evict_ban=case_when(evict_start==1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>% 
  filter(time>=-14 & time<=42 )%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)

save(county_cases1, 
  county_cases2, file="data/daily_count.Rdata")

#Sensitivity Analyses 

#Sensitivity 1:no lag for other NPIs
county_cases2a<-county_cases1%>%
  mutate(pre_post=ifelse(time>=14,1,0), 
         at_home=ifelse(stay_days<=0, 1, 0), 
         mask_mandate=ifelse(mask_days>=0, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=0, 1, 0), 
         evict_end=ifelse(evict_days_end>=0, 1, 0), 
         evict_ban=case_when(evict_start=1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>% 
  filter(time>=-7 & time<=42 )%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)

#Sensivity 2: 9 day lag
county_cases2b<-county_cases1%>%
  mutate(pre_post=ifelse(time>=9,1,0), 
         at_home=ifelse(stay_days<=9, 1, 0), 
         mask_mandate=ifelse(mask_days>=9, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=9, 1, 0), 
         evict_end=ifelse(evict_days_end>=9, 1, 0), 
         evict_ban=case_when(evict_start=1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>% 
  filter(time>=-19 & time<=37 )%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)


#Sensitivity 3
#increase lag to 3 weeks (21 days)
county_cases2c<-county_cases1%>%
  mutate(pre_post=ifelse(time>=21,1,0), 
         at_home=ifelse(stay_days<=21, 1, 0), 
         mask_mandate=ifelse(mask_days>=21, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=21, 1, 0), 
         evict_end=ifelse(evict_days_end>=21, 1, 0), 
         evict_ban=case_when(evict_start=1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>% 
  filter(time>=-7 & time<=49)%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)

#sensitivity 4
#increase lag to 4 weeks (28 days)
county_cases2d<-county_cases1%>%
  mutate(pre_post=ifelse(time>=28,1,0), 
         at_home=ifelse(stay_days<=28, 1, 0), 
         mask_mandate=ifelse(mask_days>=28, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=28, 1, 0), 
         evict_end=ifelse(evict_days_end>=28, 1, 0), 
         evict_ban=case_when(evict_start=1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>% 
  filter(time>=0 & time<=56)%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)

#sensivity 5a
county_cases2e1<-county_cases1%>%
  mutate(pre_post=ifelse(time>=14,1,0), 
         at_home=ifelse(stay_days<=14, 1, 0), 
         mask_mandate=ifelse(mask_days>=14, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=14, 1, 0), 
         evict_end=ifelse(evict_days_end>=14, 1, 0), 
         evict_ban=case_when(evict_start==1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>% 
  filter(time>=-28 & time<=42 )%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)

#sensitivity 5
#Increase study period to 12 weeks 
county_cases2e<-county_cases1%>%
  mutate(pre_post=ifelse(time>=14,1,0), 
         at_home=ifelse(stay_days<=14, 1, 0), 
         mask_mandate=ifelse(mask_days>=14, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=14, 1, 0), 
         evict_end=ifelse(evict_days_end>=14, 1, 0), 
         evict_ban=case_when(evict_start=1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>% 
  #14 + 12 weeks(84days)
  filter(time>=-84 & time<=98 )%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)

#sensitivity 6
#remove the non bchc cities 
#option 1, using 14 day lag for all NPIs

bchc<-list("Philadelphia","Indianapolis", "San Francisco", "Pheonix", "Austin", "Dallas", "Houston","San Antonio") 

county_cases2f<-county_cases1%>%
  filter(cities %in% bchc)%>%
  mutate(pre_post=ifelse(time>=14,1,0), 
         at_home=ifelse(stay_days<=14, 1, 0), 
         mask_mandate=ifelse(mask_days>=14, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=14, 1, 0), 
         evict_end=ifelse(evict_days_end>=14, 1, 0), 
         evict_ban=case_when(evict_start==1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>% 
  filter(time>=-7 & time<=42 )%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)

save(county_cases2,
     county_cases2a,
     county_cases2b,
     county_cases2c,
     county_cases2d,
     county_cases2e,
     county_cases2f, file="data/daily_count_2a.Rdata")

#Event Study dataset
#increase time period to +/- 12 weeks 
##################################################################
# Event Study Model 

#create weekly vars for event model 
event_model<-county_cases1%>%
  mutate(treat_start=case_when( 
    #Atlanta
    Admin2=="Fulton"~ "2020-04-27",
    Admin2=="Philadelphia"~"2020-06-26",
    Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-01",
    Admin2=="San Francisco"~"2020-08-31", 
    Admin2=="Maricopa"~"2020-05-11",
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
      Admin2=="Travis"~"2020-07-15",
      Admin2=="Dallas"~"2020-05-15",
      Admin2=="Harris"~ "2020-05-20",
      Admin2=="Bexar"~ "2020-06-05",
      Admin2=="Maricopa" ~"2020-05-16",
      #San Francisco
      Admin2=="San Francisco"~"2020-05-18", 
      #Milwaukee
      Admin2=="Milwaukee"~"2020-06-05", 
      #indianapolis
      Admin2=="Marion"~"2020-05-15",
      Admin2=="Charleston"~"2020-05-31"), 
    stay_start=as.Date(stay_start, "%Y-%m-%d"), 
    #create mask mandate var     
    mask_start=case_when(#Atlanta
      Admin2=="Fulton"~ "2020-07-08",
      Admin2=="Philadelphia"~"2020-07-01",
      Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-07-03",
      #phoenix
      Admin2=="Maricopa"~"2020-06-20",
      Admin2=="San Francisco"~"2020-04-17", 
      Admin2=="Milwaukee"~"2020-07-16", 
      #indianapolis
      Admin2=="Marion"~"2020-07-09",
      Admin2=="Charleston"~"2020-07-01"), 
    mask_start=as.Date(mask_start, "%Y-%m-%d"), 
    #create end of eviction ban
    eviction_start=case_when(
      #Atlanta
      Admin2=="Fulton"~ "2020-03-16",
      Admin2=="Philadelphia"~"2020-03-18",
      Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-03-19",
      Admin2=="Maricopa"~"2020-03-24",
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
      Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-19",
      Admin2=="Maricopa"~"2020-10-31",
      Admin2=="San Francisco"~"2020-12-31", 
      Admin2=="Milwaukee"~"2020-05-26", 
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

# NOTE: OUR CONTROL HERE IS REOPENING!! (not staying closed)
                     
#Event model: limit study period to 4 weeks pre and 8 weeks (2 weeks lag + 6 weeks) (or 6 pre and 6 post)
event_model1<-event_model%>%
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
    stay_week=as.factor(case_when(
      stay_days <0 ~0,
      stay_days %in% c(0:7)~0, 
      stay_days %in% c(8:14)~0, 
      stay_days %in% c(15:21)~1, 
      stay_days %in% c(22:28)~2, 
      stay_days %in% c(29:35) ~ 3, 
      stay_days %in% c(36:42)~4, 
      stay_days >42~5 )), 
    #creat var for weeks since mask mandate 
    mask_week=as.factor(case_when(
      mask_days <0 ~0, 
      mask_days %in% c(0:7)~0, 
      mask_days %in% c(8:14)~0, 
      mask_days %in% c(15:21)~1, 
      mask_days %in% c(22:28)~2, 
      mask_days %in% c(29:35) ~ 3, 
      mask_days %in% c(36:42)~4, 
      mask_days >42~5)), 
    #create var for weeks since eviction 
    evict_days=case_when(date>eviction_end~-1, 
                         TRUE~as.numeric(evict_days)),     
    evict_week=as.factor(case_when(
      evict_days <0 ~0, 
      evict_days %in% c(0:7)~0, 
      evict_days %in% c(8:14)~0, 
      evict_days %in% c(15:21)~1, 
      evict_days %in% c(22:28)~2, 
      evict_days %in% c(29:35) ~ 3, 
      evict_days %in% c(36:42)~4, 
      evict_days >42~5)))%>%
  #make weeks prior and weeks post dummy vars 
  dummy_cols(select_columns = 'weeks_prior')%>%
  dummy_cols(select_columns='weeks_post')

#Sensitivity 1: period to 84 days (12 weeks) (4 before, 10 after + 2 week lag)
event_model2<-event_model%>%
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
      treat1==0 & time %in% c(57:63)~9, 
      treat1==0 & time %in% c(64:70)~10, 
      treat1==0 & time %in% c(71:77)~11, 
      treat1==0 & time %in% c(78:84)~12,
      TRUE~0)),  
    #ordinal var for weeks since stay at home lifted
    stay_week=as.factor(case_when(
      stay_days <0 ~0,
      stay_days %in% c(0:7)~0, 
      stay_days %in% c(8:14)~0, 
      stay_days %in% c(15:21)~1, 
      stay_days %in% c(22:28)~2, 
      stay_days %in% c(29:35) ~ 3, 
      stay_days %in% c(36:42)~4, 
      stay_days >42~5 )), 
    #create var for weeks since mask mandate 
    mask_week=as.factor(case_when(
      mask_days <0 ~0,
      mask_days %in% c(0:7)~0, 
      mask_days %in% c(8:14)~0, 
      mask_days %in% c(15:21)~1, 
      mask_days %in% c(22:28)~2, 
      mask_days %in% c(29:35) ~ 3, 
      mask_days %in% c(36:42)~4, 
      mask_days >42~5)), 
    #create var for weeks since eviction 
    evict_days=case_when(date>eviction_end~-1, 
                         TRUE~as.numeric(evict_days)),     
    evict_week=as.factor(case_when(
      evict_days <0 ~0, 
      evict_days %in% c(0:7)~0, 
      evict_days %in% c(8:14)~0, 
      evict_days %in% c(15:21)~1, 
      evict_days %in% c(22:28)~2, 
      evict_days %in% c(29:35) ~ 3, 
      evict_days %in% c(36:42)~4, 
      evict_days >42~5)))%>%
  filter(time>=-28 & time<84)%>%
  dummy_cols(select_columns = 'weeks_prior')%>%
  dummy_cols(select_columns='weeks_post')

#sensitivity 2--no limit on NPI indicators
event_model3<-event_model%>%
  filter(time>=-28 & time<84)%>%
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
      treat1==0 & time %in% c(57:63)~9, 
      treat1==0 & time %in% c(64:70)~10, 
      treat1==0 & time %in% c(71:77)~11, 
      treat1==0 & time %in% c(78:84)~12,
      TRUE~0)),  
    #ordinal var for weeks since stay at home lifted
    stay_week=as.factor(case_when(
      stay_days <0 ~0,
      stay_days %in% c(0:7)~0, 
      stay_days %in% c(8:14)~0, 
      stay_days %in% c(15:21)~1, 
      stay_days %in% c(22:28)~2, 
      stay_days %in% c(29:35)~3, 
      stay_days %in% c(36:42)~4, 
      stay_days %in% c(42:49)~5, 
      stay_days %in% c(50:56)~6, 
      stay_days %in% c(57:63)~7, 
      stay_days %in% c(64:70)~8, 
      stay_days %in% c(71:77)~9, 
      stay_days %in% c(78:84)~10, 
      stay_days %in% c(85:91)~11, 
      stay_days %in% c(92:98)~12,
      stay_days %in% c(99:105) ~13, 
      stay_days %in% c(106:112)~14,
      TRUE~0)), 
    #creat var for weeks since mask mandate 
    mask_week=as.factor(case_when(
      mask_days <0 ~0, 
      mask_days %in% c(0:7)~0, 
      mask_days %in% c(8:14)~0, 
      mask_days %in% c(15:21)~1, 
      mask_days %in% c(22:28)~2, 
      mask_days %in% c(29:35)~3, 
      mask_days %in% c(36:42)~4, 
      mask_days %in% c(42:49)~5, 
      mask_days %in% c(50:56)~6, 
      mask_days %in% c(57:63)~7, 
      mask_days %in% c(64:70)~8, 
      mask_days %in% c(71:77)~9, 
      mask_days %in% c(78:84)~10, 
      mask_days %in% c(85:91)~11, 
      mask_days %in% c(92:98)~12,
      mask_days %in% c(99:105)~13, 
      mask_days %in% c(106:112)~14,
      TRUE~0)), 
    #create var for weeks since eviction 
    evict_days=case_when(date>eviction_end~-1, 
                         TRUE~as.numeric(evict_days)),     
    evict_week=as.factor(case_when(
      evict_days <0 ~0, 
      evict_days %in% c(0:7)~0, 
      evict_days %in% c(8:14)~0, 
      evict_days %in% c(15:21)~1, 
      evict_days %in% c(22:28)~2, 
      evict_days %in% c(29:35)~3, 
      evict_days %in% c(36:42)~4, 
      evict_days %in% c(42:49)~5, 
      evict_days %in% c(50:56)~6, 
      evict_days %in% c(57:63)~7, 
      evict_days %in% c(64:70)~8, 
      evict_days %in% c(71:77)~9, 
      evict_days %in% c(78:84)~10,
      evict_days %in% c(85:91)~11, 
      evict_days %in% c(92:98)~12,
      evict_days %in% c(99:105)~13, 
      evict_days %in% c(106:112)~14, 
      TRUE~0)))%>%
  dummy_cols(select_columns = 'weeks_prior')%>%
  dummy_cols(select_columns='weeks_post')


#sensitivity 3: change NPI lag to 3 weeks
event_model4<-event_model%>%
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
    stay_week=as.factor(case_when(
      stay_days <0 ~0,
      stay_days %in% c(0:7)~0, 
      stay_days %in% c(8:14)~0, 
      stay_days %in% c(15:21)~0, 
      stay_days %in% c(22:28)~1, 
      stay_days %in% c(29:35) ~ 2, 
      stay_days %in% c(36:42)~3, 
      stay_days %in% c(43:49)~4, 
      stay_days >49~5 )), 
    #creat var for weeks since mask mandate 
    mask_week=as.factor(case_when(
      mask_days <0 ~0, 
      mask_days %in% c(0:7)~0, 
      mask_days %in% c(8:14)~0, 
      mask_days %in% c(15:21)~0, 
      mask_days %in% c(22:28)~1, 
      mask_days %in% c(29:35) ~ 2, 
      mask_days %in% c(36:42)~3, 
      mask_days %in% c(43:49)~4, 
      mask_days >49~5 )),  
    #create var for weeks since eviction 
    evict_days=case_when(date>eviction_end~-1, 
                         TRUE~as.numeric(evict_days)),     
    evict_week=as.factor(case_when(
      evict_days <0 ~0, 
      evict_days %in% c(0:7)~0, 
      evict_days %in% c(8:14)~0, 
      evict_days %in% c(15:21)~0, 
      evict_days %in% c(22:28)~1, 
      evict_days %in% c(29:35) ~ 2, 
      evict_days %in% c(36:42)~3, 
      evict_days %in% c(43:49)~4, 
      evict_days >49~5 )))%>% 
  #make weeks prior and weeks post dummy vars 
  dummy_cols(select_columns = 'weeks_prior')%>%
  dummy_cols(select_columns='weeks_post')

#change lag to 4 weeks
event_model5<-event_model%>%
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
    stay_week=as.factor(case_when(
      stay_days <0 ~0,
      stay_days %in% c(0:7)~0, 
      stay_days %in% c(8:14)~0, 
      stay_days %in% c(15:21)~0, 
      stay_days %in% c(22:28)~0, 
      stay_days %in% c(29:35) ~1, 
      stay_days %in% c(36:42)~2, 
      stay_days %in% c(43:49)~3, 
      stay_days %in% c(50:56)~4, 
      stay_days >56~5)), 
    #creat var for weeks since mask mandate 
    mask_week=as.factor(case_when(
      mask_days <0 ~0, 
      mask_days %in% c(0:7)~0, 
      mask_days %in% c(8:14)~0, 
      mask_days %in% c(15:21)~0, 
      mask_days %in% c(22:28)~0, 
      mask_days %in% c(29:35) ~ 1, 
      mask_days %in% c(36:42)~2, 
      mask_days %in% c(43:49)~3, 
      mask_days %in% c(50:56)~4, 
      mask_days >56~5 )),  
    #create var for weeks since eviction 
    evict_days=case_when(date>eviction_end~-1, 
                         TRUE~as.numeric(evict_days)),     
    evict_week=as.factor(case_when(
      evict_days <0 ~0, 
      evict_days %in% c(0:7)~0, 
      evict_days %in% c(8:14)~0, 
      evict_days %in% c(15:21)~0, 
      evict_days %in% c(22:28)~0, 
      evict_days %in% c(29:35) ~1, 
      evict_days %in% c(36:42)~2, 
      evict_days %in% c(43:49)~3, 
      evict_days %in% c(50:56)~4, 
      evict_days >56~5 )))%>% 
  #make weeks prior and weeks post dummy vars 
  dummy_cols(select_columns = 'weeks_prior')%>%
  dummy_cols(select_columns='weeks_post')

save(event_model1,
     event_model2, 
     event_model3,
     event_model4, 
     event_model5, 
     file="data/event_model1.Rdata")

#event model for graphing 
event_model2m<-event_model%>%
  filter(time>=-28 & time<=42)%>%
  mutate(weeks= as.factor(case_when(
    time %in% c(-1:-7)~-1,
    time %in% c(-7:-14)~-2, 
    time %in% c(-15:-21)~-3, 
    time %in% c(-22:-28)~-4, 
    time %in% c(-29:-35)~-5, 
    time %in% c(-36:-42)~-6, 
    time %in% c(-42:-49)~-7, 
    time %in% c(-50:-56)~-8, 
    time %in% c(-57:-63)~-9, 
    time %in% c(-64:-70)~-10, 
    time %in% c(-71:-77)~-11, 
    time %in% c(-78:-84)~-12,  
    time %in% c(0:7)~1,
    time %in% c(8:14)~2, 
    time %in% c(15:21)~3, 
    time %in% c(22:28)~4, 
    time %in% c(29:35)~5, 
    time %in% c(36:42)~6, 
    time %in% c(42:49)~7, 
    time %in% c(50:56)~8, 
    time %in% c(57:63)~9, 
    time %in% c(64:70)~10, 
    time %in% c(71:77)~11, 
    time %in% c(78:84)~12)))%>%
  group_by(weeks, treat1, cities)%>%
  summarize(weekly_newcases=sum(daily_count),
            weekly_rate=sum(case_rate))%>%
  ungroup()

table(event_model2$weeks)

save(event_model2, file="data/event_model2.Rdata")
###MOVING AVERAGES
#calculate moving average 
roll_avg <- county_cases2 %>%
  dplyr::arrange(desc(cities)) %>% 
  dplyr::group_by(cities) %>% 
  dplyr::mutate(case_03da = zoo::rollmean(daily_count, k = 3, fill = NA),
                case_05da = zoo::rollmean(daily_count, k = 5, fill = NA),
                case_07da = zoo::rollmean(daily_count, k = 7, fill = NA),
                case_15da = zoo::rollmean(daily_count, k = 15, fill = NA),
                case_21da = zoo::rollmean(daily_count, k = 21, fill = NA), 
                caserate_07da = zoo::rollmean(case_rate, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

save(roll_avg, file="data/roll_avg.Rdata")

#------------------------------------------------------------------------------#
                        ###############Deaths ####################
#------------------------------------------------------------------------------#

county_deaths1<-county_deaths%>%
  filter(FIPS %in% cities)%>%
  #create treatment/control var 
  mutate(treat1=as.factor(case_when(Admin2%in% c("Philadelphia", "Marion", "San Francisco", "Milwaukee")~1, 
                                    Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                                                 "Harris", "Fulton", "Charleston")~0)))%>%
  dplyr::select(UID, FIPS, Admin2, Province_State, '3/1/20':'11/1/20', treat1)%>%
  pivot_longer(!c(UID, FIPS, Admin2, Province_State,treat1), names_to="date", values_to="deaths")%>%         
  #get daily count by taking difference from one day to next
  mutate(cities=as.factor(case_when(FIPS==42101~"Philadelphia",
                                    FIPS==18097~"Indianapolis", 
                                    FIPS==6075~"San Francisco", 
                                    FIPS==55079~"Milwaukee", 
                                    FIPS==4013~"Pheonix", 
                                    FIPS==48453~"Austin", 
                                    FIPS==48113~"Dallas", 
                                    FIPS==48201~"Houston", 
                                    FIPS==48029~ "San Antonio", 
                                    FIPS==13121~"Atlanta", 
                                    FIPS==45019~"Charleston")))%>%
  group_by(FIPS)%>%
  mutate(daily_deaths = c(deaths[1],diff(deaths)))%>%
  mutate(date=as.Date(date, "%m/%d/%y"),
         treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
                               Admin2=="Philadelphia"~"2020-06-26",
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-01",
                              Admin2=="Maricopa"~"2020-05-11",
                                Admin2=="Milwaukee"~"2020-05-14", 
                               Admin2=="San Francisco"~"2020-08-31", 
                               Admin2=="Marion"~"2020-05-11",
                               Admin2=="Charleston"~"2020-05-11"),
         treat_start=as.Date(treat_start, "%Y-%m-%d"),
         #create var that normalizes start date (1st treat day 1 = day 1 for all cities)
         stay_start=case_when(
           #Atlanta
           Admin2=="Fulton"~ "2020-05-01",
           Admin2=="Philadelphia"~"2020-06-05",
           Admin2=="Travis"~"2020-07-15",
           Admin2=="Dallas"~"2020-05-15",
           Admin2=="Harris"~ "2020-05-20",
           Admin2=="Bexar"~ "2020-06-05",
           Admin2=="Maricopa" ~"2020-05-16",
           #San Francisco
           Admin2=="San Francisco"~"2020-05-18", 
           #Milwaukee
           Admin2=="Milwaukee"~"2020-06-05", 
           #indianapolis
           Admin2=="Marion"~"2020-05-16",
           Admin2=="Charleston"~"2020-05-31"),
         stay_start=as.Date(stay_start, "%Y-%m-%d"), 
         #create mask mandate var     
         mask_start=case_when(#Atlanta
           Admin2=="Fulton"~ "2020-07-08",
           Admin2=="Philadelphia"~"2020-07-01",
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-07-03",
           #phoenix
           Admin2=="Maricopa"~"2020-06-18",
           Admin2=="San Francisco"~"2020-07-01", 
           Admin2=="Milwaukee"~"2020-07-16", 
           #indianapolis
           Admin2=="Marion"~"2020-07-09",
           Admin2=="Charleston"~"2020-07-01"), 
         mask_start=as.Date(mask_start, "%Y-%m-%d"), 
         #create end of eviction ban
         eviction_start=case_when(
           #Atlanta
           Admin2=="Fulton"~ "2020-03-16",
           Admin2=="Philadelphia"~"2020-03-18",
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-03-19",
           Admin2=="Maricopa"~"2020-03-24",
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
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-19",
           Admin2=="Maricopa"~"2020-10-31",
           Admin2=="San Francisco"~"2020-12-31", 
           Admin2=="Milwaukee"~"2020-05-26", 
           #indianapolis
           Admin2=="Marion"~"2020-08-14",
           Admin2=="Charleston"~"2020-05-15"),
         eviction_end=as.Date(eviction_end, "%Y-%m-%d"),
         #create var that normalizes start date (1st treat day 1 = day 1 for all cities)
         time= as.numeric(difftime(date,treat_start, units = c("days"))),
         stay_days= as.integer(difftime(date,stay_start, units = c("days"))), 
         mask_days=as.integer(difftime(date,mask_start, units = c("days"))), 
         evict_days=as.integer(difftime(date,eviction_start, units = c("days"))),
         evict_days_end=as.integer(difftime(date,eviction_end, units=c("days"))))
county_deaths1$daily_deaths[county_deaths1$daily_deaths< 0] <- 0

hist(county_deaths1$daily_deaths)

#create pre post with 35 days after
#we want period post intervention that reflects the cases 4 weeks before the intervention= 7-35 days post 
# 14 days for cases, + 21 days for death lag  
county_deaths2<-county_deaths1%>%
  mutate(pre_post=ifelse(time>35,1,0), 
         at_home=ifelse(stay_days<=34, 1, 0), 
         mask_mandate=ifelse(mask_days>=34, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=34, 1, 0), 
         evict_end=ifelse(evict_days_end>=34, 1, 0), 
         evict_ban=case_when(evict_start==1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>%
  #limit to 2 weeks before + 4 weeks post lag 
  filter(time>=7 & time<=64)%>%
  left_join(population1)%>%
  #create rates  
  mutate(death_rate=daily_deaths/pop*100000)%>%
  ungroup()



save(county_deaths2, file="data/daily_count_deaths.Rdata")


###MOVING AVERAGES
#calculate moving average 
roll_avg_death <- county_deaths2 %>%
  mutate(log=log(daily_deaths))%>%
  dplyr::arrange(desc(cities)) %>% 
  dplyr::group_by(cities) %>% 
  dplyr::mutate(deaths_03da = zoo::rollmean(daily_deaths, k = 3, fill = NA),
                deaths_05da = zoo::rollmean(daily_deaths, k = 5, fill = NA),
                deaths_07da = zoo::rollmean(daily_deaths, k = 7, fill = NA),
                deaths_15da = zoo::rollmean(daily_deaths, k = 15, fill = NA),
                deaths_21da = zoo::rollmean(daily_deaths, k = 21, fill = NA), 
                deathrate_07da = zoo::rollmean(death_rate, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

save(roll_avg_death, file="data/roll_avg_death.Rdata")


#Demographic Data (for table 1, Propensity score, and pre-treatment covariate adjustment)
 # population ,  % aged <18, age>=18-64, aged >65, % below FPL, % college educated
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
                "B17001_001","B17001_002", 
                #sex (total, female)
                "B01001_001", "B01001_026", 
                #race/ethnicity (total,NH white, NH black, hispanic), 
                "B03002_001", 
                "B03002_003", 
                "B03002_012", 
                "B03002_004", 
                # overcrowding denominator
                "B25014_001", 
                #overcrowding 1 and more
                "B25014_005", "B25014_011",
                #overcrowding 1.5more
                "B25014_006", "B25014_012",
                #overcrowding 2more
                "B25014_007", "B25014_013",
                # citizenship
                "B05001_001", "B05001_006",
                # foreign born
                "B06001_001", "B06001_049", 
                # public transit excluding taxicab
                "B08006_001","B08006_008", 
                #service workers
                "C24010_024","C24010_060","C24010_026","C24010_062",
                #total occupations
                "C24010_001"),
              year=2019, survey = "acs1") %>%
  dplyr::select(GEOID, variable, estimate) %>%
  spread(variable, estimate) %>% 
  mutate(GEOID=as.numeric(GEOID))

data<-data %>% 
  mutate(pct_age0017=rowSums(across(c(B01001_003:B01001_006,B01001_027:B01001_030)))/B01001_001,
         pct_age1864=rowSums(across(c(B01001_007:B01001_019,B01001_031:B01001_043)))/B01001_001,
         pct_age65plus=rowSums(across(c(B01001_020:B01001_025,B01001_044:B01001_049)))/B01001_001,
         total_pop=B01001_001,
         pct_college=rowSums(across(c(B15003_022:B15003_025)))/B15003_001,
         pct_poverty=B17001_002/B17001_001,
         pct_female= B01001_026/B01001_001,
         pct_hisp=B03002_012/B03002_001,
         pct_black=B03002_004/B03002_001,
         pct_nhwhite=B03002_003/B03002_001,
         pct_noncitizen=B05001_006/B05001_001,
         pct_foreignborn=B06001_049/B06001_001,
         pct_overcrowded1=rowSums(across(c(B25014_007, B25014_006,B25014_005,
                                           B25014_011,B25014_012, B25014_013)))/B25014_001,
         pct_transit=B08006_008/B08006_001,
         pct_service=rowSums(across(c(C24010_024, C24010_026, C24010_060, C24010_062)))/C24010_001,
         GEOID=as.numeric(GEOID)) %>% 
 dplyr:: select(GEOID, total_pop, pct_age0017, pct_age1864, pct_age65plus,pct_poverty, pct_college, pct_female, pct_nhwhite, pct_black, pct_hisp, pct_noncitizen, pct_foreignborn, pct_overcrowded1, pct_transit, pct_service ) %>% 
  mutate_at(vars(matches("pct")), ~.*100) %>% 
  mutate(total_pop=total_pop/1000000)


treat<-c(42101,18097,6075, 55079)
comparison<-c(4013, 48453, 48113, 48201,  48029, 13121,45019)
data<-data %>% dplyr::filter(GEOID%in%c(treat, comparison)) %>% 
  mutate(group=ifelse(GEOID%in%treat, "0Treatment", "1Control"))

#need to add in charleston race/ethnicity because not in 2019 tidycensus acs -- hand code in 

data$pct_nhwhite[is.na(data$pct_nhwhite)]<-69.6
  data$pct_black[is.na(data$pct_black)]<-26.3
  data$pct_hisp[is.na(data$pct_hisp)]<-5.3

  
  save(data, file="data/demographic_data.Rdata")

