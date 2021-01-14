###***********************Data Management_ indoor dining & preemption**************************

#*Version 1 started Nov 20, 2020 by Alina Schnake-Mahl
#*Imports, cleans, & merges COVID data + policy data

#Import all libraries 
library(data.table)
library(tidyverse)
library(tidycensus)
library(lubridate)

#IMPORT COUNTY LEVEL DATA
county_cases<-fread("data/time_series_covid19_confirmed_US.csv")
county_deaths<-fread("data/time_series_covid19_deaths_US.csv")
#limit dataset to our counties/cities-- select cities wherever available 
# for austin- city is code 48015 and coutny is 48453-- using county (bc don't trust city)
cities<-c(42101,18097,32003, 41051, 4013, 48453, 48113, 48201,  48029, 13121,45019)

#get county population estimates (Downloaded from here)-https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
population<-fread("data/co-est2019-annres.csv", header=TRUE)

#names of counties of interest
cities_pop<-c(".Philadelphia County, Pennsylvania", ".Marion County, Indiana", 
              ".Clark County, Nevada", ".Multnomah County, Oregon", ".Maricopa County, Arizona", 
              ".Travis County, Texas", ".Dallas County, Texas", ".Harris County, Texas", 
              ".Bexar County, Texas", ".Fulton County, Georgia", ".Charleston County, South Carolina")

#limit population dataset to counties we're exploring 
population1<-population%>%
  filter(V1%in% cities_pop)%>%
#add fips
mutate(FIPS=case_when(
                      V1==".Philadelphia County, Pennsylvania"~42101,
                      V1==".Marion County, Indiana"~18097,
                      V1==".Clark County, Nevada"~32003, 
                      V1==".Multnomah County, Oregon"~41051, 
                      V1==".Maricopa County, Arizona"~4013, 
                      V1==".Travis County, Texas"~48453, 
                      V1==".Dallas County, Texas"~48113, 
                      V1==".Harris County, Texas"~48201, 
                      V1==".Bexar County, Texas"~48029,
                      V1==".Fulton County, Georgia"~13121, 
                      V1==".Charleston County, South Carolina"~45019))%>%
  rename(pop='2019')%>%
  select(pop, FIPS)

str(population1)


### MAIN ANALYSIS -Using date of state reopening as date of treatment for treatment cities
#limit cases to counties of interest and time periods of study period


county_cases1<-county_cases%>%
  filter(FIPS %in% cities)%>%
  #create treatment/control var 
  mutate(treat1=as.factor(case_when(Admin2%in% c("Philadelphia", "Marion", "Clark", "Multnomah")~1, 
                          Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                                       "Harris", "Fulton", "Charleston")~0)))%>%
  select(UID, FIPS, Admin2, Province_State, '3/1/20':'10/1/20', treat1)%>%
  pivot_longer(!c(UID, FIPS, Admin2, Province_State, treat1), names_to="date", values_to="cases")%>%         
  #get daily count by taking difference from one day to next
  mutate(cities=as.factor(case_when(FIPS==42101~"Philadelphia",
                                    FIPS==18097~"Indianapolis", 
                                    FIPS==32003~"Las Vegas", 
                                    FIPS==41051~"Portland", 
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
         #post date starts 14 days after reopening/remaining closed date 
        pre_post=case_when(
          #indianapolis & charleston 
            (FIPS %in% c(18097, 45019) &
              between(date, "2020-04-13", "2020-05-24"))| 
             #las vegas 
            (FIPS==32003 & 
                between(date, "2020-04-11", "2020-05-22"))|
             #portland
            (FIPS==41051 &
                between(date, "2020-04-17", "2020-05-28"))|
            #philadelphia
              (FIPS==42101 &
                between(date,"2020-06-05", "2020-07-16"))|
             #all of texas + arizona (Maricopa) have same dates
             (FIPS %in% c(48453,48113, 48201, 48029, 4013) & 
                between(date, "2020-04-03", "2020-05-14"))|
            #atlanta
              (FIPS==13121 & 
                between(date, "2020-03-30", "2020-05-10"))~0,  
           #indianapolis & charleston 
            (FIPS %in% c(18097,45019) & 
              between(date, "2020-05-25", "2020-06-22"))|
           #last vegas
               (FIPS==32003 & 
                between(date, "2020-05-23", "2020-06-20"))| 
            #portland
              (FIPS==41051 &
                between(date, "2020-05-29", "2020-06-26"))|
            #philadelphia
              (FIPS==42101 & 
                between(date,"2020-07-17", "2020-08-14"))|
             #all of texas + arizona (Maricopa) have same dates
               (FIPS %in% c(48453,48113, 48201, 48029, 4013) & 
                between(date, "2020-05-15", "2020-06-12"))|
            #Atlanta
              (FIPS==13121 &
                between(date, "2020-05-11", "2020-06-08"))~1), 
###########CREATE VAR FOR MASK MANDATES##############
        at_home=case_when(
 #indianapolis 
   (FIPS==18097 & date<"2020-05-18")|
 #Charleston
  (FIPS==45019 & date<"2020-05-04")|
 #las vegas 
  (FIPS==32003 & date<"2020-05-09")|
#portland
 (FIPS==41051 & date<"2020-06-19")|
#philadelphia
 (FIPS==42101 & date<"2020-06-05")|
#pheonix 
  (FIPS==4013 & date<"2020-05-11")|
#all of texas has same dates
  (FIPS %in% c(48453,48113, 48201, 48029) & 
     date<"2020-05-01")|
#atlanta
  (FIPS==13121 & date<"2020-05-04")~1, 
#indianapolis 
   (FIPS==18097 & date>="2020-05-18")|
#Charleston
  (FIPS==45019 & date>="2020-05-04")|
#las vegas 
  (FIPS==32003 & date>="2020-05-09")|
#portland
  (FIPS==41051 &  date>="2020-06-19")|
#philadelphia
  (FIPS==42101 & date>="2020-06-05")|
#pheonix 
  (FIPS==4013 & date>="2020-05-11")|
#all of texas has same dates
  (FIPS %in% c(48453,48113, 48201, 48029) & 
     date>="2020-05-01")|
#Atlanta
  (FIPS==13121 & date>="2020-05-04")~0), 
#######CREATE VAR FOR MASK MANDATES#######
    mask_mandate=case_when(
#indianapolis 
 (FIPS==18097 & date<"2020-07-27")|
#Charleston
 (FIPS==45019 & date<"2020-10-01")|
#las vegas 
  (FIPS==32003 & date<"2020-06-26")|
#portland
  (FIPS==41051 & date<"2020-07-01")|
#philadelphia
  (FIPS==42101 & date<"2020-05-07")|
#pheonix 
  (FIPS==4013 & date<"2020-06-20")|
#all of texas has same dates
  (FIPS %in% c(48453,48113, 48201, 48029) & 
     date<"2020-07-03")|
#atlanta
  (FIPS==13121 & date<"2020-07-08")~0, 
#indianapolis 
  (FIPS==18097 & date>="2020-07-27")|
#las vegas 
  (FIPS==32003 & date>="2020-06-26")|
#portland
  (FIPS==41051 &  date>="2020-07-01")|
#philadelphia
  (FIPS==42101 & date>="2020-07-01")|
#pheonix 
  (FIPS==4013 & date>="2020-06-20")|
#all of texas has same dates
  (FIPS %in% c(48453,48113, 48201, 48029) & 
       date>="2020-07-03")|
#Atlanta
  (FIPS==13121 & date>="2020-07-08")~1),
#####CREATE MANDATE FOR EVICTION MORATORIUM
end_evict=case_when(
#indianapolis 
  (FIPS==18097 & between(date, "2020-03-19", "2020-08-15"))|
#Charleston
  (FIPS==45019 & between(date, "2020-03-17", "2020-05-15"))|
#las vegas # ends 10-15, so coded through end of study period   
  (FIPS==32003 & date>= "2020-03-29")|
#portland - no end date yet (as of 12/10)
  (FIPS==41051 & date>="2020-04-01")|
#philadelphia
  (FIPS==42101 & between(date, "2020-03-18", "2020-09-01"))|
#pheonix  (ends 10-31; so coded through end of study period
  (FIPS==4013 & date>="2020-03-24")|
#Austin (ends 12-31; so coded through end of study period)
  (FIPS==48453 & date>="2020-03-19")|
#Dallas, Houston, San Antonio 
   (FIPS %in% c(48113, 48201, 48029) & 
     between(date, "2020-03-18", "2020-05-20"))~1, 
#indianapolis 
  (FIPS==18097 & date>"2020-08-15")|
#no las vegas b/c mortorium ended after study period
#portland 
  (FIPS==41051 & date<"2020-04-01")|  
#philadelphia
  (FIPS==42101 & date>"2020-09-01")|
#pheonix - ended 10/31 so coded as1 whole study period 
  #austin 1 the whole time 
#Dallas, Houston, San ANtonio
  (FIPS %in% c(48453,48113, 48201, 48029) & 
     date>="2020-05-19")|
  (FIPS==45019 & date>"2020-05-15")|
#Atlanta
  (FIPS==13121 & date>"2020-03-01")~0))%>%
#add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)%>%
  ungroup()

str(population1)
str(county_cases1)
levels(county_cases1$cities)

#recode counts of <0 cases to zero 
county_cases1$daily_count[county_cases1$daily_count< 0] <- 0


########################
#filter out the dates outside the study period (4 weeks before and after + two week lag)
county_cases2<-county_cases1%>%
  filter(pre_post%in%c(0,1)) %>%
  mutate(treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
                               Admin2=="Philadelphia"~"2020-07-03",
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
                               Admin2=="Multnomah"~"2020-05-15", 
                               Admin2=="Clark"~"2020-05-09", 
                               Admin2=="Marion"~"2020-05-11",
                               Admin2=="Charleston"~"2020-05-11"),
         #will need to add MIAMI
         treat_start=as.Date(treat_start, "%Y-%m-%d")) 
#create var that normalizes start date (1st treat day 1 = day 1 for all cities)
county_cases2$time=as.numeric(difftime(county_cases2$date,county_cases2$treat_start, units = c("days")))

save(county_cases2, file="daily_count.Rdata")

#Event Study dataset
#increase time period to +/- 12 weeks 
##################################################################
# Event Study Model 

#create weekly vars for event model 
event_model<-county_cases1%>%
  mutate(treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
                               Admin2=="Philadelphia"~"2020-09-08",
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
                               Admin2=="Multnomah"~"2020-06-19", 
                               Admin2=="Clark"~"2020-06-04", 
                               Admin2=="Marion"~"2020-06-01",
                               Admin2=="Miami-Dade"~"2020-05-18"),
         #will need to add MIAMI
         treat_start=as.Date(treat_start, "%Y-%m-%d")) 
#create var that normalizes start date (1st treat day 1 = day 1 for all cities)
event_model$time= as.numeric(difftime(event_model$date,event_model$treat_start, units = c("days")))

event_model1<-event_model%>%
  mutate(week= as.factor(case_when(
                      treat1==1 & time %in% c(0:7)~1, 
                      treat1==1 & time %in% c(8:14)~2, 
                      treat1==1 & time %in% c(15:21)~3, 
                      treat1==1 & time %in% c(22:28)~4, 
                      treat1==1 & time %in% c(29:35)~5, 
                      treat1==1 & time %in% c(36:42)~6, 
                      treat1==1 & time %in% c(42:49)~7, 
                      treat1==1 & time %in% c(50:56)~8, 
                      treat1==1 & time %in% c(57:63)~9, 
                      treat1==1 & time %in% c(64:70)~10, 
                      treat1==1 & time %in% c(71:77)~11, 
                      treat1==1 & time %in% c(78:74)~12))) 
                         
levels(event_model1$week)
 save(event_model1, file="event_model1.Rdata")

#event model for graphing 
 
 event_model2<-event_model%>%
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
     time %in% c(-78:-74)~-12,  
     time %in% c(1:7)~1,
     time==0~0,
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
       time %in% c(78:74)~12)))%>%
    group_by(weeks, treat1, cities)%>%
       summarize(weekly_newcases=sum(daily_count),
               weekly_rate=sum(case_rate))%>%
   ungroup()
 
 levels(event_model2$weeks)
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

#tidy data 
NewCasesTidy <- roll_avg %>% 
  # pivot longer
  tidyr::pivot_longer(names_to = "new_conf_av_key", 
                      values_to = "new_conf_av_value", 
                      cols = c(case_03da,
                               case_07da)) %>% 
  # better labels for printing
  dplyr::mutate(new_conf_av_key = dplyr::case_when(
    new_conf_av_key == "case_03da" ~ "3-day new confirmed cases",
    new_conf_av_key == "case_07da" ~ "7-day new confirmed cases",
    TRUE ~ NA_character_)) %>% 
  # reduce vars
  dplyr::select(time, 
                date, 
                cities, 
                Province_State,
                new_conf_av_value, 
                new_conf_av_key)

save(NewCasesTidy, file="NewCasesTidy.Rdata")

#-------------------------------------------------#
###############Deaths ####################
#-------------------------------------------------#
county_deaths1<-county_deaths%>%
  filter(FIPS %in% cities)%>%
  #create treatment/control var 
  mutate(treat1=as.factor(case_when(Admin2%in% c("Philadelphia", "Marion", "Clark", "Multnomah")~1, 
                                    Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                                                 "Harris", "Fulton", "Charleston")~0)))%>%
  select(UID, FIPS, Admin2, Province_State, '3/1/20':'10/1/20', treat1)%>%
  pivot_longer(!c(UID, FIPS, Admin2, Province_State, treat1), names_to="date", values_to="deaths")%>%         
  #get daily count by taking difference from one day to next
  mutate(cities=as.factor(case_when(FIPS==42101~"Philadelphia",
                                    FIPS==18097~"Indianapolis", 
                                    FIPS==32003~"Las Vegas", 
                                    FIPS==41051~"Portland", 
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
         #post date starts 14 days after reopening/remaining closed date 
         pre_post=case_when(
           #indianapolis & charleston 
           (FIPS %in% c(18097, 45019) &
              between(date, "2020-04-13", "2020-05-24"))| 
             #las vegas 
             (FIPS==32003 & 
                between(date, "2020-04-11", "2020-05-22"))|
             #portland
             (FIPS==41051 &
                between(date, "2020-04-17", "2020-05-28"))|
             #philadelphia
             (FIPS==42101 &
                between(date,"2020-06-05", "2020-07-16"))|
             #all of texas + arizona (Maricopa) have same dates
             (FIPS %in% c(48453,48113, 48201, 48029, 4013) & 
                between(date, "2020-04-03", "2020-05-14"))|
             #atlanta
             (FIPS==13121 & 
                between(date, "2020-03-30", "2020-05-10"))~0,  
           #indianapolis & charleston 
           (FIPS %in% c(18097,45019) & 
              between(date, "2020-05-25", "2020-06-22"))|
             #last vegas
             (FIPS==32003 & 
                between(date, "2020-05-23", "2020-06-20"))| 
             #portland
             (FIPS==41051 &
                between(date, "2020-05-29", "2020-06-26"))|
             #philadelphia
             (FIPS==42101 & 
                between(date,"2020-07-17", "2020-08-14"))|
             #all of texas + arizona (Maricopa) have same dates
             (FIPS %in% c(48453,48113, 48201, 48029, 4013) & 
                between(date, "2020-05-15", "2020-06-12"))|
             #Atlanta
             (FIPS==13121 &
                between(date, "2020-05-11", "2020-06-08"))~1), 
  ###########CREATE VAR FOR MASK MANDATES##############
     at_home=case_when(
#indianapolis 
   (FIPS==18097 & date<"2020-05-18")|
#Charleston
   (FIPS==45019 & date<"2020-05-04")|
#las vegas 
  (FIPS==32003 & date<"2020-05-09")|
#portland 
  (FIPS==41051 & date<"2020-06-19")|
#philadelphia
  (FIPS==42101 & date<"2020-06-05")|
#pheonix 
  (FIPS==4013 & date<"2020-05-11")|
#all of texas has same dates
 (FIPS %in% c(48453,48113, 48201, 48029) & 
        date<"2020-05-01")|
#atlanta
   (FIPS==13121 & date<"2020-05-04")~1, 
#indianapolis 
    (FIPS==18097 & date>="2020-05-18")|
#Charleston
    (FIPS==45019 & date>="2020-05-04")|
#las vegas 
     (FIPS==32003 & date>="2020-05-09")|
#portland
     (FIPS==41051 &  date>="2020-06-19")|
#philadelphia
     (FIPS==42101 & date>="2020-06-05")|
#pheonix 
     (FIPS==4013 & date>="2020-05-11")|
#all of texas has same dates
     (FIPS %in% c(48453,48113, 48201, 48029) & 
        date>="2020-05-01")|
#Atlanta
  (FIPS==13121 & date>="2020-05-04")~0), 
#######CREATE VAR FOR MASK MANDATES#######
     mask_mandate=case_when(
#indianapolis 
   (FIPS==18097 & date<"2020-07-27")|
#Charleston
   (FIPS==45019 & date<"2020-10-01")|
#las vegas 
   (FIPS==32003 & date<"2020-06-26")|
#portland
   (FIPS==41051 & date<"2020-07-01")|
#philadelphia
   (FIPS==42101 & date<"2020-05-07")|
#pheonix 
  (FIPS==4013 & date<"2020-06-20")|
#all of texas has same dates
   (FIPS %in% c(48453,48113, 48201, 48029) & 
          date<"2020-07-03")|
#atlanta
     (FIPS==13121 & date<"2020-07-08")~0, 
#indianapolis 
     (FIPS==18097 & date>="2020-07-27")|
#las vegas 
     (FIPS==32003 & date>="2020-06-26")|
#portland
     (FIPS==41051 &  date>="2020-07-01")|
#philadelphia
     (FIPS==42101 & date>="2020-07-01")|
#pheonix 
     (FIPS==4013 & date>="2020-06-20")|
#all of texas has same dates
     (FIPS %in% c(48453,48113, 48201, 48029) & 
        date>="2020-07-03")|
#Atlanta
     (FIPS==13121 & date>="2020-07-08")~1),
 #####CREATE MANDATE FOR EVICTION MORATORIUM
       end_evict=case_when(
#indianapolis 
     (FIPS==18097 & between(date, "2020-03-19", "2020-08-15"))|
#Charleston
     (FIPS==45019 & between(date, "2020-03-17", "2020-05-15"))|
#las vegas # ends 10-15, so coded through end of study period   
     (FIPS==32003 & date>= "2020-03-29")|
#portland - no end date yet (as of 12/10)
     (FIPS==41051 & date>="2020-04-01")|
#philadelphia
     (FIPS==42101 & between(date, "2020-03-18", "2020-09-01"))|
#pheonix  (ends 10-31; so coded through end of study period
     (FIPS==4013 & date>="2020-03-24")|
#Austin (ends 12-31; so coded through end of study period)
     (FIPS==48453 & date>="2020-03-19")|
#Dallas, Houston, San Antonio 
     (FIPS %in% c(48113, 48201, 48029) & 
          between(date, "2020-03-18", "2020-05-20"))~1, 
#indianapolis 
     (FIPS==18097 & date>"2020-08-15")|
#no las vegas b/c mortorium ended after study period
#portland 
     (FIPS==41051 & date<"2020-04-01")|  
#philadelphia
     (FIPS==42101 & date>"2020-09-01")|
#pheonix - ended 10/31 so coded as1 whole study period 
#austin 1 the whole time 
#Dallas, Houston, San ANtonio
     (FIPS %in% c(48453,48113, 48201, 48029) & 
            date>="2020-05-19")|
#Charleston
  (FIPS==45019 & date>"2020-05-15")|
#Atlanta
     (FIPS==13121 & date>"2020-03-01")~0))%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_deaths/pop*100000)%>%
  ungroup()

str(population1)
str(county_cases1)
levels(county_cases1$cities)

#recode counts of <0 cases to zero 
county_deaths1$daily_deaths[county_deaths1$daily_deaths< 0] <- 0


########################
#filter out the dates outside the study period (4 weeks before and after + two week lag)
county_deaths2<-county_deaths1%>%
  filter(pre_post%in%c(0,1)) %>%
  mutate(treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
                               Admin2=="Philadelphia"~"2020-07-03",
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
                               Admin2=="Multnomah"~"2020-05-15", 
                               Admin2=="Clark"~"2020-05-09", 
                               Admin2=="Marion"~"2020-05-11",
                               Admin2=="Charleston"~"2020-05-11"),
         treat_start=as.Date(treat_start, "%Y-%m-%d")) 
#create var that normalizes start date (1st treat day 1 = day 1 for all cities)
county_deaths2$time=as.numeric(difftime(county_deaths2$date,county_deaths2$treat_start, units = c("days")))

save(county_deaths2, file="daily_count_deaths.Rdata")

#Event Study dataset
#increase time period to +/- 12 weeks 
##################################################################
# Event Study Model 

#create weekly vars for event model 
event_model_death<-county_deaths2%>%
  mutate(treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
                               Admin2=="Philadelphia"~"2020-09-08",
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
                               Admin2=="Multnomah"~"2020-06-19", 
                               Admin2=="Clark"~"2020-06-04", 
                               Admin2=="Marion"~"2020-06-01",
                               Admin2=="Miami-Dade"~"2020-05-18"),
         #will need to add MIAMI
         treat_start=as.Date(treat_start, "%Y-%m-%d")) 
#create var that normalizes start date (1st treat day 1 = day 1 for all cities)
event_model_death$time= as.numeric(difftime(event_model_death$date,event_model_death$treat_start, units = c("days")))

event_model_death1<-event_model_death%>%
  mutate(week= as.factor(case_when(
    treat1==1 & time %in% c(0:7)~1, 
    treat1==1 & time %in% c(8:14)~2, 
    treat1==1 & time %in% c(15:21)~3, 
    treat1==1 & time %in% c(22:28)~4, 
    treat1==1 & time %in% c(29:35)~5, 
    treat1==1 & time %in% c(36:42)~6, 
    treat1==1 & time %in% c(42:49)~7, 
    treat1==1 & time %in% c(50:56)~8, 
    treat1==1 & time %in% c(57:63)~9, 
    treat1==1 & time %in% c(64:70)~10, 
    treat1==1 & time %in% c(71:77)~11, 
    treat1==1 & time %in% c(78:74)~12))) 

levels(event_model_death1$week)
save(event_model_death1, file="event_model_death1.Rdata")

#event model for graphing 

event_model_death2<-event_model_death%>%
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
    time %in% c(-78:-74)~-12,  
    time %in% c(1:7)~1,
    time==0~0,
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
    time %in% c(78:74)~12)))%>%
  group_by(weeks, treat1, cities)%>%
  summarize(newdeaths=sum(daily_deaths),
            death_rate=sum(death_rate))%>%
  ungroup()

levels(event_model_death2$weeks)
table(event_model_death2$weeks)

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
                deathrate_07da = zoo::rollmean(case_rate, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

save(roll_avg_death, file="roll_avg_death.Rdata")

#tidy data 
NewDeathsTidy <- roll_avg_death %>% 
  # pivot longer
  tidyr::pivot_longer(names_to = "new_conf_av_key", 
                      values_to = "new_conf_av_value", 
                      cols = c(death_03da,
                               death_07da)) %>% 
  # better labels for printing
  dplyr::mutate(new_conf_av_key = dplyr::case_when(
    new_conf_av_key == "death_03da" ~ "3-day new deaths",
    new_conf_av_key == "death_07da" ~ "7-day new deaths",
    TRUE ~ NA_character_)) %>% 
  # reduce vars
  dplyr::select(time, 
                date, 
                cities, 
                Province_State,
                new_conf_av_value, 
                new_conf_av_key)

save(NewDeathsTidy, file="NewdeathsTidy.Rdata")




#SENSITIVITY ANALYSIS 

# NEED TO REMOVE MIAMI AND ADD CHARLESTO

#Dates using dates of reopenings for each city (not our actual study periods)
#limit cases to counties of interest and time periods of study period
county_cases_sens1<-county_cases%>%
  filter(FIPS %in% cities)%>%
  #create treatment/control var 
  mutate(treat1=case_when(Admin2%in% c("Philadelphia", "Marion", "Clark", "Multnomah")~1, 
                          Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                          "Harris", "Fulton", "Charleston", "Miami-Dade")~0))%>%
 select(UID, FIPS, Admin2, Province_State, '4/1/20':'10/1/20', treat1)%>%
  pivot_longer(!c(UID, FIPS, Admin2, Province_State, treat1), names_to="date", values_to="cases")%>%         
#get daily count by taking difference from one day to next
mutate(cities=as.factor(case_when(FIPS==42101~"Philadelphia",
                        FIPS==18097~"Indianapolis", 
                        FIPS==32003~"Las Vegas", 
                        FIPS==41051~"Portland", 
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
#NEED TO DETERMINE IF WE WANT TO INCLUDE CHARLESTON
 pre_post=case_when(
    (Admin2=="Marion" &
       between(date, "2020-05-04", "2020-05-31"))| 
    (Admin2=="Clark" & 
       between(date, "2020-05-07", "2020-06-03"))|
    (Admin2=="Multnomah" &
       between(date, "2020-05-22", "2020-06-18"))|
    (Admin2=="Philadelphia" &
       between(date,"2020-08-11", "2020-09-07"))|
      #allof texas + arizona (Maricopa) have same dates
    (Admin2 %in% c("Travis","Dallas", "Harris", "Bexar", "Maricopa") & 
        between(date, "2020-04-03", "2020-05-01"))|
    (Admin2=="Fulton" & 
        between(date, "2020-03-30", "2020-04-26"))|
   (Admin2=="Miami-Dade" &
         between(date, "2020-04-20", "2020-05-17"))~0,  
    (Admin2=="Marion" & 
       between(date, "2020-06-01", "2020-06-29"))|
    (Admin2=="Clark" & 
       between(date, "2020-06-04", "2020-07-02"))| 
    (Admin2=="Multnomah" &
       between(date, "2020-06-19", "2020-07-17"))|
    (Admin2=="Philadelphia"& 
       between(date,"2020-09-08","2020-10-06"))|
    (Admin2 %in% c("Travis","Dallas", "Harris", "Bexar", "Maricopa") & 
        between(date, "2020-05-01", "2020-05-29"))|
   (Admin2=="Fulton" &
     between(date, "2020-04-27", "2020-05-25"))|
     (Admin2=="Miami-Dade" &
    between(date, "2020-05-18", "2020-06-15"))~1))%>%
#add in population counts
left_join(population1)%>%
  #create rates  
  mutate(case_rate=daily_count/pop*100000)%>%
  ungroup()

str(population1)
str(county_cases1)
levels(county_cases1$cities)

#TBD on Miami-- need to confirm dates THEN ADD     
#recode counts of <0 cases to zero 
county_cases1$daily_count[county_cases1$daily_count< 0] <- 0


########################
#filter out the dates outside the study period (4 weeks before and after)
county_cases_sens2<-county_cases_sens1%>%
  filter(pre_post%in%c(0,1)) %>%
  mutate(treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
                               Admin2=="Philadelphia"~"2020-09-08",
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
                               Admin2=="Multnomah"~"2020-06-19", 
                               Admin2=="Clark"~"2020-06-04", 
                               Admin2=="Marion"~"2020-06-01",
                               Admin2=="Miami-Dade"~"2020-05-18"),
         #will need to add MIAMI
treat_start=as.Date(treat_start, "%Y-%m-%d")) 
#create var that normalizes start date (1st treat day 1 = day 1 for all cities)
county_cases2$time= difftime(county_cases2$date,county_cases2$treat_start, units = c("days"))

save(county_cases_sens2, file="daily_count_sens.Rdata")



###############Deaths ####################
county_deaths1<-county_deaths%>%
  filter(FIPS %in% cities)%>%
  #create treatment/control var 
  mutate(treat1=case_when(Admin2%in% c("Orleans", "Philadelphia","Cook", "Multnomah")~1, 
                          Admin2%in% c("Maricopa","Austin", "Bexar", "Dallas",	
                                       "Harris", "Fulton", "Charleston")~0))%>%
  select(UID, FIPS, Admin2, Province_State, '4/1/20':'10/1/20')%>%
  pivot_longer(!c(UID, FIPS, Admin2, Province_State), names_to="date", values_to="deaths")%>%         
  #get daily count by taking difference from one day to next
  group_by(FIPS)%>%
  mutate(daily_deaths = c(deaths[1],diff(deaths)))%>%
  mutate(date=as.Date(date, "%m/%d/%y"))
#recode negatives to zero
county_deaths1$daily_deaths[county_deaths1$daily_deaths< 0] <- 0

