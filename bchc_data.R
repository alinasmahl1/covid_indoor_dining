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


#import BCHC data
#philly data
city_cases<-fread("data/covid_trends_city_bchc.txt")
#BCHC JH data
county_cases<-fread("data/covid_trends_county_proxy_bchc.txt")
#jh data for non BCHC data
county_casesjh<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
county_deathsjh<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

#FIPS crosswalk
FIPS<-fread("data/xwalk_fips_bchc.txt")

#limit to cities in our study
cities<-c(42101,18097,6075, 55079, 4013, 48453, 48113, 48201,  48029, 55079,13121, 45019)
#non bchc cities --milwaukee, atlanta, charleston
non_bchc<-c(55079,13121,45019)

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


#limit to study cities
FIPS1<-FIPS%>%  
  dplyr::select(fips, city_name, county_name)%>%
  rename(city='city_name')%>%
  filter(fips %in% cities)
  
#join FIPS to county_cases
city_cases1<-city_cases%>%
  inner_join(FIPS1, by='city')%>%
dplyr::select(city, date, new_cases_raw, new_deaths_raw, fips, county_name)

#take JH data and pivot 
county_casesjh1<-county_casesjh%>%
  filter(FIPS %in% non_bchc)%>%
  dplyr::select(UID, FIPS, Admin2, Province_State, '3/1/20':'11/1/20')%>%
  pivot_longer(!c(UID, FIPS, Admin2, Province_State), names_to="date", values_to="cases")%>% 
  group_by(FIPS)%>%
  mutate(daily_count = c(cases[1],diff(cases)))%>%
  mutate(date=as.Date(date, "%m/%d/%y"))%>%
  dplyr::select(FIPS, date, daily_count)

#combine philly, bchc jh, and jh data
#join the philly and county datasets together

county_cases1bchc<-county_cases%>%
  #keep data from philly city data
  filter(city!="Philadelphia")%>%
  inner_join(FIPS1, by="city")%>%
  dplyr::select(city, date, new_cases_raw, new_deaths_raw, fips, county_name)%>%
  rbind(city_cases1)%>%
  rename(daily_count="new_cases_raw", daily_deaths="new_deaths_raw", FIPS="fips")%>%
 dplyr::select(FIPS, date, daily_count)%>%
  mutate(FIPS=as.numeric(FIPS), 
         date=as.Date(date, "%m/%d/%y"))%>%
  rbind(county_casesjh1)%>%
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
                                    FIPS==45019~"Charleston")), 
         Admin2=as.factor(case_when(FIPS==42101~"Philadelphia", 
                                    FIPS==18097 ~"Marion", 
                                    FIPS==6075~"San Francisco", 
                                    FIPS==55079~"Milwaukee", 
                                    FIPS==4013~"Maricopa", 
                                    FIPS==48453~ "Travis", 
                                    FIPS==48201~ "Harris", 
                                    FIPS==48113~ "Dallas", 
                                    FIPS==48029~ "Bexar", 
                                    FIPS==13121~ "Fulton", 
                                    FIPS==45019~ "Charleston")))%>%
  mutate(treat1=case_when(Admin2%in% c("Philadelphia", "Marion", "San Francisco", "Milwaukee")~1, 
                          Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                                       "Harris", "Fulton", "Charleston")~0))%>%
  mutate(treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
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
              Admin2=="Travis"~"2020-07-15",
              Admin2=="Dallas"~"2020-05-15",
              Admin2=="Harris"~ "2020-05-20",
              Admin2=="Bexar"~ "2020-06-05",
              Admin2=="Maricopa" ~"2020-05-16",
              #San Francisco
              Admin2=="San Francisco"~"2020-05-14", 
              #Milwaukee
              Admin2=="Milwaukee"~"2020-06-05", 
              #indianapolis
              Admin2=="Marion"~"2020-05-18",
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
            evict_days_end=as.integer(difftime(date,eviction_end, units=c("days"))), 
            cal_week=factor(week(date)))
            county_cases1bchc$daily_count[county_cases1bchc$daily_count< 0] <- 0

#-------------------------------------------------------------
#repeat for deaths 
#-------------------------------------------------------------
county_deathsjh1<-county_deathsjh%>%
   filter(FIPS %in% non_bchc)%>%
      dplyr::select(UID, FIPS, Admin2, Province_State, '3/1/20':'11/1/20')%>%
      pivot_longer(!c(UID, FIPS, Admin2, Province_State), names_to="date", values_to="deaths")%>% 
      group_by(FIPS)%>%
      mutate(daily_deaths = c(deaths[1],diff(deaths)))%>%
      mutate(date=as.Date(date, "%m/%d/%y"))%>%
      dplyr::select(FIPS, date, daily_deaths)
            
                        
county_deaths1bchc<-county_cases%>%
  #keep data from philly city data
  filter(city!="Philadelphia")%>%
  inner_join(FIPS1, by="city")%>%
  dplyr::select(city, date, new_cases_raw, new_deaths_raw, fips, county_name)%>%
  rbind(city_cases1)%>%
  rename(daily_count="new_cases_raw", daily_deaths="new_deaths_raw", FIPS="fips")%>%
  dplyr::select(FIPS, date, daily_deaths)%>%
  mutate(FIPS=as.numeric(FIPS), 
         date=as.Date(date, "%m/%d/%y"))%>%
  rbind(county_deathsjh1)%>%
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
                                    FIPS==45019~"Charleston")), 
         Admin2=as.factor(case_when(FIPS==42101~"Philadelphia", 
                                    FIPS==18097 ~"Marion", 
                                    FIPS==6075~"San Francisco", 
                                    FIPS==55079~"Milwaukee", 
                                    FIPS==4013~"Maricopa", 
                                    FIPS==48453~ "Travis", 
                                    FIPS==48201~ "Harris", 
                                    FIPS==48113~ "Dallas", 
                                    FIPS==48029~ "Bexar", 
                                    FIPS==13121~ "Fulton", 
                                    FIPS==45019~ "Charleston")))%>%
  mutate(treat1=case_when(Admin2%in% c("Philadelphia", "Marion", "San Francisco", "Milwaukee")~1, 
                          Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                                       "Harris", "Fulton", "Charleston")~0))%>%
  mutate(treat_start=case_when(Admin2=="Fulton"~ "2020-04-27",
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
           Admin2=="Travis"~"2020-07-15",
           Admin2=="Dallas"~"2020-05-15",
           Admin2=="Harris"~ "2020-05-20",
           Admin2=="Bexar"~ "2020-06-05",
           Admin2=="Maricopa" ~"2020-05-16",
           #San Francisco
           Admin2=="San Francisco"~"2020-05-14", 
           #Milwaukee
           Admin2=="Milwaukee"~"2020-06-05", 
           #indianapolis
           Admin2=="Marion"~"2020-05-18",
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
         evict_days_end=as.integer(difftime(date,eviction_end, units=c("days"))), 
         cal_week=factor(week(date)))
county_deaths1bchc$daily_deaths[county_deaths1bchc$daily_deaths< 0] <- 0
