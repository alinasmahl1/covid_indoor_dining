###***********************Data Management_ indoor dining & preemption**************************

#*Version 1 started Nov 20, 2020 by Alina Schnake-Mahl
#*Imports, cleans, & merges COVID data + policy data

#Import all libraries 
library(data.table)
library(tidyverse)
library(tidycensus)
library(lubridate)

#IMPORT COUNTY LEVEL DATA
county_cases<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
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
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
                               Admin2=="Milwaukee"~"2020-05-14", 
                               Admin2=="San Francisco"~"2020-08-31", 
                               Admin2=="Marion"~"2020-05-11",
                               Admin2=="Charleston"~"2020-05-11"),
         treat_start=as.Date(treat_start, "%Y-%m-%d"), 
         stay_start=case_when(
           #Atlanta
           Admin2=="Fulton"~ "2020-05-01",
           Admin2=="Philadelphia"~"2020-06-05",
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-01",
           Admin2=="Maricopa" ~"2020-05-16",
           #San Francisco
           Admin2=="San Francisco"~"2020-05-14", 
           #Milwaukee
           Admin2=="Milwaukee"~"2020-05-13", 
           #indianapolis
           Admin2=="Marion"~"2020-05-18",
           Admin2=="Charleston"~"2020-05-04"), 
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
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-19",
           #San Francisco- no end during study
           Admin2=="San Francisco"~"2020-12-31", 
           #Milwaukee
           Admin2=="Milwaukee"~"2020-05-27", 
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
         evict_days_end=as.integer(difftime(date,eviction_end, units=c("days"))))
county_cases1$daily_count[county_cases1$daily_count< 0] <- 0


hist(county_cases1$daily_count)

#option 1, using 14 day lag for all NPIs
county_cases2<-county_cases1%>%
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

save(county_cases1, 
  county_cases2, file="daily_count.Rdata")

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
  filter(time>=-28 & time<=42 )%>%
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
  filter(time>=-28 & time<=37 )%>%
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
  filter(time>=-28 & time<=49)%>%
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
  filter(time>=-28 & time<=56)%>%
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
  filter(time>=-28 & time<=42 )%>%
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
     county_cases2f, file="daily_count_2a.Rdata")

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

# NOTE: OUR CONTROL HERE IS REOPENING!! (not staying closed)

#not efficient but dif date uses calendar weeks, so wasn't working
# our control here is re-opening 

event_model1<-event_model%>%
  mutate(weeks1=as.factor(floor(time/7)+1), 
  stay_week=as.factor(floor(stay_days/7)))
                      
#Event model sensitivity 2: limit study period to 4 weeks pre and 8 weeks (2 weeks lag + 6 weeks) (or 6 pre and 6 post)
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

save(event_model1, file="event_model3.Rdata")

#Sensitivity 1: period to 84 days (12 weeks) (10 before, 10 after + 2 week lag)
event_model2<-event_model%>%
  mutate(weeks_prior=as.factor(case_when(
    treat1==0 & time %in% c(-1:-7)~1,
    treat1==0 & time %in% c(-7:-14)~2, 
    treat1==0 & time %in% c(-15:-21)~3, 
    treat1==0 & time %in% c(-22:-28)~4, 
    treat1==0 & time %in% c(-29:-35)~5, 
    treat1==0 & time %in% c(-36:-42)~6, 
    treat1==0 & time %in% c(-42:-49)~7, 
    treat1==0 & time %in% c(-50:-56)~8, 
    treat1==0 & time %in% c(-57:-63)~9, 
    treat1==0 & time %in% c(-64:-70)~10, 
    treat1==0 & time %in% c(-71:-77)~11, 
    treat1==0 & time %in% c(-78:-84)~12, 
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
    ########need to fix this bc eviction bans had end dates
    evict_days=case_when(date>eviction_end~-1, 
                         TRUE~as.numeric(evict_days)),     
    evict_week=as.factor(case_when(
      evict_days <0 ~0, 
      evict_days %in% c(0:7)~1, 
      evict_days %in% c(8:14)~2, 
      evict_days %in% c(15:21)~3, 
      evict_days %in% c(22:28)~4, 
      evict_days >28 ~5)))%>%
  filter(time>-70 & time<84)%>%
  dummy_cols(select_columns = 'weeks_prior')%>%
  dummy_cols(select_columns='weeks_post')



#sensitivity 2--no limit on NPI indicators
event_model3<-event_model%>%
  filter(time>-70 & time<84)%>%
  mutate(weeks_prior=as.factor(case_when(
    treat1==0 & time %in% c(-1:-7)~1,
    treat1==0 & time %in% c(-7:-14)~2, 
    treat1==0 & time %in% c(-15:-21)~3, 
    treat1==0 & time %in% c(-22:-28)~4, 
    treat1==0 & time %in% c(-29:-35)~5, 
    treat1==0 & time %in% c(-36:-42)~6, 
    treat1==0 & time %in% c(-42:-49)~7, 
    treat1==0 & time %in% c(-50:-56)~8, 
    treat1==0 & time %in% c(-57:-63)~9, 
    treat1==0 & time %in% c(-64:-70)~10, 
    treat1==0 & time %in% c(-71:-77)~11, 
    treat1==0 & time %in% c(-78:-84)~12,  
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
    #would expect beta to go in opposite direction bc cases likely increase after opening
    stay_week=as.factor(case_when(
      stay_days <0 ~0,
      stay_days %in% c(0:7)~1, 
      stay_days %in% c(8:14)~2, 
      stay_days %in% c(15:21)~3, 
      stay_days %in% c(22:28)~4, 
      stay_days %in% c(29:35)~5, 
      stay_days %in% c(36:42)~6, 
      stay_days %in% c(42:49)~7, 
      stay_days %in% c(50:56)~8, 
      stay_days %in% c(57:63)~9, 
      stay_days %in% c(64:70)~10, 
      stay_days %in% c(71:77)~11, 
      stay_days %in% c(78:84)~12, 
      stay_days %in% c(85:91)~13, 
      stay_days %in% c(92:98)~14,
      stay_days %in% c(99:105) ~15, 
      stay_days %in% c(106:112)~16,
      TRUE~0)), 
    #creat var for weeks since mask mandate 
    mask_week=as.factor(case_when(
      mask_days <0 ~0, 
      mask_days %in% c(0:7)~1, 
      mask_days %in% c(8:14)~2, 
      mask_days %in% c(15:21)~3, 
      mask_days %in% c(22:28)~4, 
      mask_days %in% c(29:35)~5, 
      mask_days %in% c(36:42)~6, 
      mask_days %in% c(42:49)~7, 
      mask_days %in% c(50:56)~8, 
      mask_days %in% c(57:63)~9, 
      mask_days %in% c(64:70)~10, 
      mask_days %in% c(71:77)~11, 
      mask_days %in% c(78:84)~12, 
      mask_days %in% c(85:91)~13, 
      mask_days %in% c(92:98)~14,
      mask_days %in% c(99:105) ~15, 
      mask_days %in% c(106:112)~16,
      TRUE~0)), 
    #create var for weeks since eviction 
    ########need to fix this bc eviction bans had end dates
    evict_days=case_when(date>eviction_end~-1, 
                         TRUE~as.numeric(evict_days)),     
    evict_week=as.factor(case_when(
      evict_days <0 ~0, 
      evict_days %in% c(0:7)~1, 
      evict_days %in% c(8:14)~2, 
      evict_days %in% c(15:21)~3, 
      evict_days %in% c(22:28)~4, 
      evict_days %in% c(29:35)~5, 
      evict_days %in% c(36:42)~6, 
      evict_days %in% c(42:49)~7, 
      evict_days %in% c(50:56)~8, 
      evict_days %in% c(57:63)~9, 
      evict_days %in% c(64:70)~10, 
      evict_days %in% c(71:77)~11, 
      evict_days %in% c(78:84)~12,
      evict_days %in% c(85:91)~13, 
      evict_days %in% c(92:98)~14,
      evict_days %in% c(99:105) ~15, 
      evict_days %in% c(106:112)~16, 
      TRUE~0)))%>%
  dummy_cols(select_columns = 'weeks_prior')%>%
  dummy_cols(select_columns='weeks_post')

  

save(event_model1,
     event_model2, 
     event_model3, file="event_model1.Rdata")


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

save(event_model2, file="event_model2.Rdata")
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

save(roll_avg, file="roll_avg.Rdata")

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
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
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
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-01",
           Admin2=="Maricopa" ~"2020-05-16",
           Admin2=="San Francisco"~"2020-05-14", 
           Admin2=="Milwaukee"~"2020-05-13", 
           #indianapoli
           Admin2=="Marion"~"2020-05-18",
           Admin2=="Charleston"~"2020-05-04"), 
         stay_start=as.Date(stay_start, "%Y-%m-%d"), 
         #create mask mandate var     
         mask_start=case_when(#Atlanta
           Admin2=="Fulton"~ "2020-07-08",
           Admin2=="Philadelphia"~"2020-07-01",
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-07-03",
           #phoenix
           Admin2=="Maricopa"~"2020-06-18",
           Admin2=="San Francisco"~"2020-07-01", 
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
         evict_days_end=as.integer(difftime(date,eviction_end, units=c("days"))))
county_deaths1$daily_deaths[county_deaths1$daily_deaths< 0] <- 0

hist(county_deaths1$daily_deaths)

# Option 1: create pre post with 34 days after
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
  filter(time>=-14 & time<=62 )%>%
  left_join(population1)%>%
  #create rates  
  mutate(death_rate=daily_deaths/pop*100000)%>%
  ungroup()


#sensitivity 1:create pre post with 42 days after
# 14 days for cases, + 28 for death lag  
county_deaths2b<-county_deaths1%>%
  mutate(pre_post=ifelse(time>42,1,0), 
         at_home=ifelse(stay_days<=41, 1, 0), 
         mask_mandate=ifelse(mask_days>=41, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=42, 1, 0), 
         evict_end=ifelse(evict_days_end>=42, 1, 0), 
         evict_ban=case_when(evict_start==1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>%
  #limit to two weeks before + 4 weeks post lag 
  filter(time>=-14 & time<=70)%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(death_rate=daily_deaths/pop*100000)%>%
  ungroup()


#sensitivity analysis 2: 
#remove lag for other NPIs
county_deaths2c<-county_deaths1%>%
  mutate(pre_post=ifelse(time>34,1,0), 
         at_home=ifelse(stay_days<=1, 1, 0), 
         mask_mandate=ifelse(mask_days>=1, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=1, 1, 0), 
         evict_end=ifelse(evict_days_end>=1, 1, 0), 
         evict_ban=case_when(evict_start==1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>%
  #limit to 2 weeks before + 4 weeks post lag 
  filter(time>=-14 & time<=28)%>%
  left_join(population1)%>%
  #create rates  
  mutate(death_rate=daily_deaths/pop*100000)%>%
  ungroup()


##sensitivity 3:create pre post with 28 days after
# 7 days for cases, + 21 for death lag  
county_deaths2d<-county_deaths1%>%
  mutate(pre_post=ifelse(time>28,1,0), 
         at_home=ifelse(stay_days<=28, 1, 0), 
         mask_mandate=ifelse(mask_days>=28, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=28, 1, 0), 
         evict_end=ifelse(evict_days_end>=28, 1, 0), 
         evict_ban=case_when(evict_start==1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>%
  #limit to two weeks before + 4 weeks post lag 
  filter(time>=-14 & time<=56)%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(death_rate=daily_deaths/pop*100000)%>%
  ungroup()

#Sensitivity 4
#remove the non bchc cities 
bchc<-list("Philadelphia","Indianapolis", "San Francisco", "Pheonix", "Austin", "Dallas", "Houston","San Antonio") 

county_deaths2e<-county_deaths1%>%
  filter(cities %in% bchc)%>%
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
  mutate(case_rate=daily_deaths/pop*100000)

save(county_deaths2,
     county_deaths2b,
     county_deaths2c,
     county_deaths2d,
     county_deaths2e, file="daily_count_deaths.Rdata")

#Event Study dataset
#increase time period to +/- 12 weeks 
##################################################################
# Event Study Model - DEATHS

#create weekly vars for event model 
event_model_death<-county_deaths1%>%
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
      Admin2=="Milwaukee"~"2020-05-13", 
      #indianapolis
      Admin2=="Marion"~"2020-05-18",
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
  mutate(death_rate=daily_deaths/pop*100000)

event_model_death1<-event_model_death%>%
  filter(time>=-28 & time<=62)%>%
  mutate(weeks_prior=as.factor(case_when(
    treat1==0 & time %in% c(-1:-7)~1,
    treat1==0 & time %in% c(-7:-14)~2, 
    treat1==0 & time %in% c(-15:-21)~3, 
    treat1==0 & time %in% c(-22:-28)~4, 
    treat1==0 & time %in% c(-29:-35)~5, 
    treat1==0 & time %in% c(-36:-42)~6, 
    treat1==0 & time %in% c(-42:-49)~7, 
    treat1==0 & time %in% c(-50:-56)~8, 
    treat1==0 & time %in% c(-57:-63)~9, 
    treat1==0 & time %in% c(-64:-70)~10, 
    treat1==0 & time %in% c(-71:-77)~11, 
    treat1==0 & time %in% c(-78:-84)~12, 
    treat1==0 & time %in% c(-85:-91)~13, 
    treat1==0 & time %in% c(-92:-98)~14,
    treat1==0 & time %in% c(-99:-105)~15, 
    treat1==0 & time %in% c(-106:-112)~16,
    treat1==0 ~0, 
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
      treat1==0 & time %in% c(85:91)~13, 
      treat1==0 & time %in% c(92:98)~14,
      treat1==0 & time %in% c(99:105) ~15, 
      treat1==0 & time %in% c(106:112)~16,
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
    ########need to fix this bc eviction bans had end dates
    evict_days=case_when(date>eviction_end~-1, 
                         TRUE~as.numeric(evict_days)),     
    evict_week=as.factor(case_when(
      evict_days <0 ~0, 
      evict_days %in% c(0:7)~1, 
      evict_days %in% c(8:14)~2, 
      evict_days %in% c(15:21)~3, 
      evict_days %in% c(22:28)~4, 
      evict_days >28 ~5)))
save(event_model_death1, file="event_model_death1.Rdata")


#event model for graphing 
#fix this 
event_model_death2<-event_model_death%>%
    filter(time>=-28 & time<=62)%>%
      mutate(weeks= as.factor(case_when(
        time %in% c(-1:-7)~1,
        time %in% c(-7:-14)~2, 
        time %in% c(-15:-21)~3, 
        time %in% c(-22:-28)~4, 
        time %in% c(-29:-35)~5, 
        time %in% c(-36:-42)~6, 
        time %in% c(-42:-49)~7, 
        time %in% c(-50:-56)~8, 
        time %in% c(-57:-63)~9, 
        time %in% c(-64:-70)~10, 
        time %in% c(-71:-77)~11, 
        time %in% c(-78:-84)~12,  
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
      summarize(weekly_newdeaths=sum(daily_deaths),
                weekly_rate=sum(death_rate))%>%
      ungroup()

save(event_model_death2, file="event_model_death2.Rdata")
###MOVING AVERAGES
#calculate moving average 
roll_avg_death <- county_deaths2 %>%
  dplyr::arrange(desc(cities)) %>% 
  dplyr::group_by(cities) %>% 
  dplyr::mutate(deaths_03da = zoo::rollmean(daily_deaths, k = 3, fill = NA),
                deaths_05da = zoo::rollmean(daily_deaths, k = 5, fill = NA),
                deaths_07da = zoo::rollmean(daily_deaths, k = 7, fill = NA),
                deaths_15da = zoo::rollmean(daily_deaths, k = 15, fill = NA),
                deaths_21da = zoo::rollmean(daily_deaths, k = 21, fill = NA), 
                deathrate_07da = zoo::rollmean(death_rate, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

save(roll_avg_death, file="roll_avg_death.Rdata")

#-----------------------------------------------------------#
#SENSITIVITY ANALYSIS 
#NOTE: We do not include this analysis in the article-- it's just an ITS, so weaker than DID
#but we do show the figure based on this dataset.

#Dates using dates of reopenings for each city (not our actual study periods)
#limit cases to counties of interest and time periods of study period
county_cases_sens1<-county_cases%>%
  filter(FIPS %in% cities)%>%
  #create treatment/control var 
  mutate(treat1=case_when(Admin2%in% c("Philadelphia", "Marion", "San Francisco", "Milwaukee")~1, 
                          Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                                       "Harris", "Fulton", "Charleston")~0))%>%
  dplyr::select(UID, FIPS, Admin2, Province_State, '4/1/20':'12/1/20', treat1)%>%
  pivot_longer(!c(UID, FIPS, Admin2, Province_State, treat1), names_to="date", values_to="cases")%>%         
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
                                    FIPS==13121~"Atlanta")))%>%
  group_by(FIPS)%>%
  mutate(daily_count = c(cases[1],diff(cases)))%>%
  mutate(date=as.Date(date, "%m/%d/%y"),
         treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
                               Admin2=="Philadelphia"~"2020-09-08",
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
                               Admin2=="Milwaukee"~"2020-06-05", 
                               Admin2=="San Francisco"~"2020-09-30", 
                               Admin2=="Marion"~"2020-06-01",
                               Admin2=="Charleston"~"2020-05-11"),
         treat_start=as.Date(treat_start, "%Y-%m-%d"), 
         stay_start=case_when(  #Atlanta
           Admin2=="Fulton"~ "2020-05-01",
           Admin2=="Philadelphia"~"2020-06-05",
           Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-01",
           Admin2=="Maricopa" ~"2020-05-16",
           Admin2=="San Francisco"~"2020-05-14", 
           Admin2=="Milwaukee"~"2020-05-13", 
           #indianapolis
           Admin2=="Marion"~"2020-05-18",
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
         evict_days_end=as.integer(difftime(date,eviction_end, units=c("days"))))
county_cases_sens1$daily_count[county_cases_sens1$daily_count< 0] <- 0


#14 day lag 
county_cases_sens2<-county_cases_sens1%>%
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


#9 day lag +  6 weeks after
county_cases_sens3<-county_cases_sens1%>%
  mutate(pre_post=ifelse(time>=9,1,0), 
         at_home=ifelse(stay_days<=9, 1, 0), 
         mask_mandate=ifelse(mask_days>=9, 1,0), 
         #including start and end dates to ensure we only code as 1 during time eviction ban was active
         evict_start=ifelse(evict_days>=9, 1, 0), 
         evict_end=ifelse(evict_days_end>=9, 1, 0), 
         evict_ban=case_when(evict_start==1 & evict_end==0 ~1, 
                             (evict_start==0 & evict_end==1)|(evict_start==1 & evict_end==1)|(evict_start==0 & evict_end==0) ~0))%>% 
  filter(time>=-28 & time<=64 )%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)


#calculate moving average 
roll_avg_sens2 <- county_cases_sens2 %>%
  dplyr::arrange(desc(cities)) %>% 
  dplyr::group_by(cities) %>% 
  dplyr::mutate(case_03da = zoo::rollmean(daily_count, k = 3, fill = NA),
                case_05da = zoo::rollmean(daily_count, k = 5, fill = NA),
                case_07da = zoo::rollmean(daily_count, k = 7, fill = NA),
                case_15da = zoo::rollmean(daily_count, k = 15, fill = NA),
                case_21da = zoo::rollmean(daily_count, k = 21, fill = NA), 
                caserate_07da = zoo::rollmean(case_rate, k = 7, fill = NA)) %>% 
  dplyr::ungroup()


#repeated for the 9 day lag + 64 days post
roll_avg_sens3 <- county_cases_sens3 %>%
  dplyr::arrange(desc(cities)) %>% 
  dplyr::group_by(cities) %>% 
  dplyr::mutate(case_03da = zoo::rollmean(daily_count, k = 3, fill = NA),
                case_05da = zoo::rollmean(daily_count, k = 5, fill = NA),
                case_07da = zoo::rollmean(daily_count, k = 7, fill = NA),
                case_15da = zoo::rollmean(daily_count, k = 15, fill = NA),
                case_21da = zoo::rollmean(daily_count, k = 21, fill = NA), 
                caserate_07da = zoo::rollmean(case_rate, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

save(county_cases_sens3, 
     county_cases_sens2, 
     roll_avg_sens2, 
     roll_avg_sens3, file="sens2_all.Rdata")

