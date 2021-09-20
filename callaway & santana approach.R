### Callaway and Sant'anna 
library(did)
library(tableone)
#import data

load("data/demographic_data.Rdata")
#we will need to invert the treatment here, so treatment= reopening 

#divide the treatment cities into 3 cohorts 
county_cases_cs<-county_cases%>%
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
  mutate(date=as.Date(date, "%m/%d/%y"),date=as.Date(date, "%m/%d/%y"),
         first_treat=case_when(Admin2=="Fulton"~ "2020-04-27",
                               #actual re-opening is 05-01 for texas, but group 04-27 and 05-11 to create a group. 
                               Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-04-27",
                               Admin2=="Maricopa"~"2020-05-11",
                               Admin2=="Charleston"~"2020-05-11"),
         first_treat=as.Date(first_treat, "%Y-%m-%d"), 
         time= as.numeric(difftime(date,first_treat, units = c("days"))),
         cal_week=factor(week(date)))
county_cases_cs$daily_count[county_cases_cs$daily_count< 0] <- 0

#two methods to create periods-- 1 use calendar weeks (but then small n/week), or create periods using dates of treatment start (C&S method)

county_cases2_cs1<-county_cases_cs%>%
  
  #rather than filtering by time-- limit to 28 days before the first period and 42 days after last (using actual start date, not lagged)
  #making periods start 14 days after treat start
         mutate(period=case_when(date>="2020-03-31"& date<"2020-5-11"~1, 
                          date>="2020-05-11" & date <"2020-05-26"~2, 
                          date>="2020-05-26" & date<"2020-06-26"~3, 
                          TRUE~0),
        group= case_when(Admin2%in% c("Travis", "Dallas", "Harris", "Bexar", "Fulton")~2, 
                        Admin2%in%c("Maricopa", "Charleston")~3, 
                              TRUE~0),
  period=as.integer(period),
  group=as.integer(group))%>%
  filter(period!=0)%>%
  #add in population counts
  left_join(population1)%>%
  #create rates  
  mutate(case_rate=(daily_count/pop)*100000, 
  #temporary solution for taking the log -- since we have zero's
         log_case=log1p(case_rate))
  #create a unique id for each row
county_cases2_cs1$id<-1:nrow(county_cases2_cs1)

table(county_cases2_cs$first.treat, county_cases2_cs$period)

str(county_cases2_cs)

  ##Analysis
  mw.attgt<-att_gt(yname="case_rate", 
                   gname="group", 
                   idname="id", 
                   tname="period", 
                   xformla=~pop,
                   data=county_cases2_cs1,
                   allow_unbalanced_panel = TRUE,
                   clustervars="FIPS"
                   )
  summary(mw.attgt)
  
  ggdid(mw.attgt)

  #takes a simple weighted average ( weights proportional to the group size)
    agg.simple <- aggte(mw.attgt, type = "simple")
summary(agg.simple)

# average treatment effects weighted by different lengths of exposure to the treatment
agg.es <- aggte(mw.attgt, type = "dynamic")
summary(agg.es)
#plot 
ggdid(agg.es)

####################################################################################
#SENSITIVITY ANALYSIS OUTPUT
#logged cases
  
  mw.attgt_log<-att_gt(yname="log_case", 
                       gname="group", 
                       idname="id", 
                       tname="period", 
                       data=county_cases2_cs1,
                       allow_unbalanced_panel = TRUE, 
                       clustervars="FIPS")  
  
  summary(mw.attgt_log)
  
  ggdid(mw.attgt_log)
  
  ####Callaway and Sant'anna sensitivity 1
  #takes a simple weighted average ( weights proportional to the group size)
  
    agg.simple_log <- aggte(mw.attgt_log, type = "simple")
  summary(agg.simple_log)
#calculate the OR using ATT from above 
  1/exp(0.82)
  #exponentiate the CI's
 1/exp(0.21)
  1/exp(1.4287)

###Callaway and Sant'anna sensitivity 2
  # average treatment effects weighted by different lengths of exposure to the treatment
    agg.es_log <- aggte(mw.attgt_log, type = "dynamic")
  summary(agg.es_log)
1/exp(0.8622)
  1/exp(0.2276)
  1/exp(1.4968)
  #plot 
  ggdid(agg.es_log)
  
  ###repeated using weekly periods (but this makes no sense because then we have too small N's/wk)
  #create weekly vars for event model 
  event_model_cs<-county_cases1%>%
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
      #create var that normalizes start date (1st treat day 1 = day 1 for all cities)
      time= as.numeric(difftime(date,treat_start, units = c("days"))),
      stay_days= as.integer(difftime(date,stay_start, units = c("days"))), 
      mask_days=as.integer(difftime(date,mask_start, units = c("days"))), 
      evict_days=as.integer(difftime(date,eviction_start, units = c("days"))), 
      #make periods defined by calendar week
      period=as.integer(week(date)), 
      period1=as.integer(month(date)),
      group=case_when(Admin2%in% c("Travis", "Dallas", "Harris", "Bexar", "Fulton")~19, 
                       Admin2%in%c("Maricopa", "Charleston")~21, 
                       TRUE~0), 
      group=as.integer(group))%>%
    #add in population counts
    left_join(population1)%>%
    #create rates  
    mutate(case_rate=daily_count/pop*100000, 
           log_case=log(daily_county))
  event_model_cs$id<-1:nrow(event_model_cs)
  
   mw.attgt<-att_gt(yname="cases", 
                   gname="group", 
                   idname="id", 
                   tname="period", 
                   xformla=~pop,
                   data=event_model_cs,
                   allow_unbalanced_panel = TRUE
  )
   mw.attgt_log
   
   #logged cases (w/ population as offset. )
   mw.attgt_log<-att_gt(yname="log_case", 
                    gname="group", 
                    idname="id", 
                    tname="period", 
                    xformla=~pop,
                    data=event_model_cs,
                    allow_unbalanced_panel = TRUE
   )
  summary(mw.attgt_log)
  table(event_model_cs$group, event_model_cs$period)
  
#Doubly Robust Approach 
  
#generate a propensity score 1st, using key unbalanced vars. 
  #
 psm<-county_cases2_cs1%>%
   rename(GEOID='FIPS')%>%
  left_join(data,by='GEOID')%>%
   mutate( 
       #weeks before treatment
         weeks_pre=case_when(Admin2=="Indianapolis"~7.0, 
                             Admin2=="Philadelphia"~13.6, 
                             Admin2=="Milwaukee"~7.0, 
                             Admin2=="Maricopa"~5.9, 
                             Admin2=="Fulton"~4.9, 
                             Admin2=="Charleston"~6.4, 
                             Admin2=="Travis"~5.4,
                             Admin2=="Dallas"~5.7, 
                             Admin2=="Harris"~5.4, 
                             Admin2=="Bexar"~5.4), 
       mask_wearing=case_when(Admin2=="San Francisco"~93.8, 
                              Admin2=="Indianapolis"~79.4, 
                              Admin2=="Philadelphia"~91.4, 
                              Admin2=="Milwaukee"~76.1, 
                              Admin2=="Maricopa"~89.2, 
                              Admin2=="Fulton"~94.9, 
                              Admin2=="Charleston"~84.3, 
                              Admin2=="Travis"~95.3,
                              Admin2=="Dallas"~89.8, 
                              Admin2=="Harris"~88.8, 
                              Admin2=="Bexar"~91.2))
 psm$pct_nhwhite[is.na(psm$pct_nhwhite)]<-69.6
 psm$pct_black[is.na(psm$pct_black)]<-26.3
 psm$pct_hisp[is.na(psm$pct_hisp)]<-5.3
 #create 
 
 
 
 str(psm$treat1)
 table(psm$pct_nhwhite, psm$Admin2)
 #run ps 
 psm.model<-glm(treat1~pct_age0017+ pct_age1864+pct_age65plus +mask_wearing + pct_nhwhite,data=psm ,family=binomial("logit"))
 
 prs_df <- data.frame(pr_score = predict(psm.model, type = "response"),
                      treat1 = psm.model$model$treat1)
 
  head(prs_df)
 
  prs_df %>%
   ggplot(aes(x = pr_score)) +
   geom_histogram(color = "white") +
   facet_wrap(~treat1) +
   xlab("Probability of being in the treatment") +
   theme_bw()
 
 
 #create table 1 w/ p
 xvars<-c("pct_age0017", "pct_age1864", "pct_age65plus")
 table1<-CreateTableOne(vars=xvars, strata="treat1", data=psm)
table1 
#view SMD
print(table1, smd=TRUE)
