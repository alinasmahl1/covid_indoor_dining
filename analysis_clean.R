###***********************Data Analysis_ indoor dining & preemption**************************

#*Version 1 started Nov 30, 2020 by Alina Schnake-Mahl
#*Descriptive and regression analysis 
rm(list=ls())
#Import all libraries 
library(ggplot2)
library(tidyverse) 
library(hrbrthemes)
library(zoo)
library(lubridate)
library(data.table)
library(ggpubr)
library(stargazer)
library(arm)
library(sandwich)
library(MASS)
library(lmtest)
library(emmeans)
library(inauguration)
library(foreign)

#import data files
load("daily_count.Rdata")
load("event_model1.Rdata")
load("roll_avg.Rdata")
load("NewCasesTidy.Rdata")
load("roll_avg_death.Rdata")
load("daily_count_deaths.Rdata")
load("county_cases_sens2.Rdata")
load("daily_count_2a.Rdata")
load("daily_count_deaths.Rdata")
load("sens2_all.Rdata")
cities<-c(42101,18097,6075, 55079, 4013, 48453, 48113, 48201,  48029, 13121,45019)

# export county_cases2 for stata (for marginal mean estimates)
write.dta(county_cases2,"county_cases2.dta")
####################################################
#Figure 1
####################################################
#figure of all cities w/ opening dates
figure1_data<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  dplyr::select(UID, FIPS, Admin2,'3/1/20':'11/1/20')%>%
  pivot_longer(!c(UID, FIPS, Admin2), names_to="date", values_to="cases")%>%
  filter(FIPS %in% cities)%>%
  mutate(date=as.Date(date, "%m/%d/%y"))%>%
 mutate(treat1=case_when(Admin2%in% c("Philadelphia", "Marion", "San Francisco", "Milwaukee")~1, 
                   Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                                "Harris", "Fulton", "Charleston")~0))%>%
  mutate(cities=factor(case_when(FIPS==42101~"Philadelphia",
                                    FIPS==18097~"Indianapolis", 
                                    FIPS==6075~"San Francisco", 
                                    FIPS==55079~"Milwaukee", 
                                    FIPS==4013~"Pheonix", 
                                    FIPS==48453~"Austin", 
                                    FIPS==48113~"Dallas", 
                                    FIPS==48201~"Houston", 
                                    FIPS==48029~ "San Antonio", 
                                    FIPS==13121~"Atlanta", 
                                    FIPS==45019~"Charleston"), 
                          levels=c("Indianapolis", "Milwaukee", "Philadelphia", "San Francisco",
                                   "Atlanta", "Austin", "Charleston", "Dallas", 
                                   "Houston", "Pheonix", "San Antonio")))%>%
  group_by(cities)%>%
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T))%>%
  mutate(cases2=cases2-lag(cases2))

#code for horizontal lines 
annotation1<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  dplyr::select(Admin2, FIPS)%>%
 filter(FIPS %in% cities)%>%
  mutate(treat1=case_when(Admin2%in% c("Philadelphia", "Marion", "San Francisco", "Milwaukee")~1, 
                          Admin2%in% c("Maricopa","Travis", "Bexar", "Dallas",	
                                       "Harris", "Fulton", "Charleston")~0))%>%
  mutate(cities=factor(case_when(FIPS==42101~"Philadelphia",
                              FIPS==18097~"Indianapolis", 
                              FIPS==6075~"San Francisco", 
                              FIPS==55079~"Milwaukee", 
                              FIPS==4013~"Pheonix", 
                              FIPS==48453~"Austin", 
                              FIPS==48113~"Dallas", 
                              FIPS==48201~"Houston", 
                              FIPS==48029~ "San Antonio", 
                              FIPS==13121~"Atlanta", 
                              FIPS==45019~"Charleston"), 
                       levels=c("Indianapolis", "Milwaukee", "Philadelphia", "San Francisco",
                                "Atlanta", "Austin", "Charleston", "Dallas", 
                                "Houston", "Pheonix", "San Antonio")))%>%
  mutate(state_allowed=case_when(
                      Admin2=="Philadelphia"~"2020-06-26",
                      Admin2=="Marion"~"2020-05-11",
                      Admin2=="San Francisco"~"2020-08-31", 
                      Admin2=="Milwaukee"~"2020-05-14", 
                      Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar", "Maricopa")~ "2020-05-01",
                      Admin2=="Fulton"~ "2020-04-27",
                      Admin2=="Charleston"~"2020-05-11"),
state_allowed=as.Date(state_allowed, "%Y-%m-%d"), 
                      city_opened=case_when( 
                        Admin2=="Philadelphia"~"2020-09-08",
                        Admin2=="Milwaukee"~"2020-06-05", 
                        Admin2=="San Francisco"~"2020-09-30", 
                        Admin2=="Marion"~"2020-06-01"), 
city_opened=as.Date(city_opened, "%Y-%m-%d"), 
    dining_closed=case_when(
      Admin2=="Philadelphia"~"2020-03-23",
      Admin2=="Marion"~"2020-03-23",
      Admin2=="San Francisco"~"2020-03-17", 
      Admin2=="Milwaukee"~"2020-03-26", 
      Admin2=="Maricopa"~"2020-03-31", 
      Admin2=="Travis"~"2020-03-24", 
      Admin2=="Dallas"~"2020-03-22",
      Admin2=="Harris"~"2020-03-24", 
      Admin2=="Bexar"~"2020-03-24", 
      Admin2=="Fulton"~ "2020-03-24",
      Admin2=="Charleston"~"2020-03-27"), 
dining_closed=as.Date(dining_closed, "%Y-%m-%d"))


pdf(file="results/figure_1.pdf")

figure1<-figure1_data%>%
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  scale_linetype_manual(values=c(1, 2, 3))+
  labs(title = "Rolling 7-day average new COVID-19 cases", 
       y = "New Cases",
       x = "Date") +
  scale_x_date(breaks="1 month", date_labels = "%b", limits=as.Date(c('2020-03-01', '2020-10-01')))+
  facet_wrap(~ cities) +
  theme(legend.position = "bottom") +
  theme_bw()+ theme(plot.title = element_text(size=10))+
  geom_vline(aes(xintercept=state_allowed, color="state_allowed"), annotation1, color="black")+
  geom_vline(aes(xintercept=city_opened, color="city_opened"), annotation1, color="red")+
  geom_vline(aes(xintercept=dining_closed, color="dining_closed"), annotation1, color="grey", lty=1)
  

figure1
dev.off()

plots = replicate(8, qplot(1,1), simplify=FALSE)
library(gridExtra)
p <- do.call(marrangeGrob, c(figure1,ncol=1,nrow=1))

ggsave("multipage.pdf", p, width=11, height=8.5)

####################################################
#Figure 2:Parallel trends assumption 
####################################################

#find means by treat and week
means <- roll_avg %>% 
  group_by(treat1, time) %>% 
  summarise(casemean = mean(daily_count), 
            mean7da = mean(case_07da), 
            ratemean=mean(caserate_07da),
            wt_meanrate=weighted.mean(caserate_07da, pop),
            logcasemean=log(casemean))
means


pdf(file="results/figure_2.pdf")
rate_mean1 <- ggplot(data=means, aes(x=time, y=wt_meanrate, color=factor(treat1, labels = c("Comparison", "Treatment")))) +
  geom_line(size=1.5)+
  geom_vline(xintercept = 14)+
  labs(title = " Rolling 7 day average rate new COVID cases", 
       color="Treat v Comparison",
       y = "New case rate per 100,000",
       x = "Days Since Reopening (comparison)/Delayed Reopening (treatment)") +
  coord_cartesian(ylim = c(0, 15))+ 
  theme(legend.position="bottom") + theme_bw() +
  scale_color_manual(values=c("#5445b1", "#cd3341"))

rate_mean1
dev.off()
###############################################################################
#Parallel trends analysis 
#limit data to pre-period

pre_period<-county_cases2%>%
  filter(time<1)

summary(pretrend<-lm(daily_count ~treat1*time,  data = pre_period))

#w/ offset
pretrend<-lm(daily_count ~treat1*time + offset(log(pop/100000)),  data = pre_period)

#repeated w/ negative binomial
summary(pretrend<-glm.nb(daily_count ~treat1*time + offset(log(pop/100000)),  data = pre_period))

#repeated w/ deaths 
pre_period_d<-county_deaths2%>%
  filter(time<1)

summary(pretrend_d<-lm(daily_deaths ~treat1*time,  data = pre_period_d))


#w/ offset
summary(pretrend<-lm(daily_deaths ~treat1*time + offset(log(pop/100000)),  data = pre_period_d))

#w/ negative binomial
summary(pretrend<-glm.nb(daily_deaths ~treat1*time + offset(log(pop/100000)),  data = pre_period_d))

###############################################################################
#APPENDIX FIGURE 2
#final figure for deaths 
means_death <- roll_avg_death %>% 
  group_by(treat1, time) %>% 
  summarise(deathmean = mean(daily_deaths), 
            mean7da = mean(deaths_07da), 
            wt_ratemean=weighted.mean(deathrate_07da, pop),
            ratemean=mean(deathrate_07da))

pdf(file="results/figure_2b_appendix.pdf")
rate_mean1_death <- ggplot(data=means_death, aes(x=time, y=wt_ratemean, color=factor(treat1, labels = c("Comparison", "Treatment")))) +
  geom_line(size=1.5)+
  geom_vline(xintercept = 35)+
  labs(title = "Rolling 7 day average rate new COVID cases", 
       color="Treat v Comparison",
       y = "New Death Rate per 100,000",
       x = "Days Since Reopening (comparison)/Delayed Reopening (treatment)") +
  theme(legend.position="bottom") +
  #using colors from inauguration_2021
  scale_color_manual(values=c("#5445b1", "#cd3341"))
rate_mean1_death

dev.off()
###############################################################################
#Appendix Figure 1
#map the sensitivity analysis w/ date of opening across cities
means_sens2 <- roll_avg_sens2 %>% 
  group_by(treat1, time) %>% 
  summarise(casemean = mean(daily_count), 
            mean7da = mean(case_07da), 
            ratemean=mean(caserate_07da),
            wt_meanrate=weighted.mean(caserate_07da, pop),
            logcasemean=log(casemean))
means_sens2

pdf(file="results/figure_2_sens.pdf")

rate_mean_sens2 <- ggplot(data=means_sens2, aes(x=time, y=wt_meanrate, color=factor(treat1, labels = c("Comparison", "Treatment"))))+ 
  geom_line(size=1.5) +
  geom_vline(xintercept = 14)+
  labs(title = " Rolling 7 day average rate new COVID cases", 
       color="Treat & Comparison",
       y = "New case rate per 100,000",
       x = "Days Since Reopening ") +
  coord_cartesian(ylim = c(0, 17.5))+
  theme(legend.position="bottom") +
  scale_color_manual(values=c("#5445b1", "#cd3341"))

rate_mean_sens2
dev.off()

######################################################
#Model Building 
######################################################

#start w/ just pre/post
county_cases2$pre_post<-as.factor(county_cases2$pre_post)

#in earlier model building we found Neg binomial a better fit for the model, so using this for model building
summary(mod_nb1 <- glm.nb(daily_count ~pre_post,  data = county_cases2))
stargazer(mod_nb1, apply.coef = exp, type='text')
AIC(mod_nb1)
BIC(mod_nb1)
rse_mod_nb1<-exp(coeftest(mod_nb1, vcov=vcovCL(mod_nb1,type="HC1",cluster=~FIPS + Province_State)))
rci_mod_nb1<-exp(coefci(mod_nb1, vcov=vcovCL(mod_nb1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb1
rci_mod_nb1

#add offset (could also consider test offset)
##we add the per 100000 to make a per 100,000 rate offset
summary(mod_nb1off<-glm.nb(daily_count~pre_post + offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb1off, apply.coef = exp, type='text')
AIC(mod_nb1off)
BIC(mod_nb1off)
##add heteroskedasticity robust standard errors and cluster at city 
#controls for mild violation of distribution assumption that var=mean
# this is equivalent to vce cluster in stata, per https://rmcd1024.github.io/R_and_Stata/stata_and_R_clustering.pdf
rse_mod_nb1off<-exp(coeftest(mod_nb1off, vcov=vcovCL(mod_nb1off,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_nb1off<-exp(coefci(mod_nb1off, vcov=vcovCL(mod_nb1off,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb1off
rci_mod_nb1off
#offset better fit for model- include from here on. 

#add treat 
summary(mod_nb2off<-glm.nb(daily_count~pre_post +treat1+ offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb2off, apply.coef = exp, type='text')
rse_mod_nb2off<-exp(coeftest(mod_nb2off, vcov=vcovCL(mod_nb2off,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_nb2off<-exp(coefci(mod_nb2off, vcov=vcovCL(mod_nb2off,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb2off
rci_mod_nb2off

################################
#Final unadjusted model 
summary(mod_nb3<-glm.nb(daily_count~treat1*pre_post+ offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb3, apply.coef = exp, type='text')
rse_mod_nb3<-exp(coeftest(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_nb3<-exp(coefci(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb3
rci_mod_nb3

########add other policies########
#stay at home order
summary(mod_nb3a<-glm.nb(daily_count~treat1*pre_post + factor(cities)+ factor(cal_week) + at_home + offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb3a, apply.coef = exp, type='text')
anova(mod_nb3, mod_nb3a,  test="Chisq")
rse_mod_nb3f<-exp(coeftest(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_nb3<-exp(coefci(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb3
rci_mod_nb3

#mask mandate
summary(mod_nb3b<-glm.nb(daily_count~treat1*pre_post + factor(cities)+ factor(cal_week) + at_home + mask_mandate + offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb3b, apply.coef = exp, type='text')
anova(mod_nb3a, mod_nb3b,  test="Chisq")
rse_mod_nb3b<-exp(coeftest(mod_nb3b, vcov=vcovCL(mod_nb3b,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_nb3b<-exp(coefci(mod_nb3b, vcov=vcovCL(mod_nb3b,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb3b
rci_mod_nb3b
#add eviction ban
#Final adjusted model
summary(mod_nb3c<-glm.nb(daily_count~treat1*pre_post + factor(cities)+ factor(cal_week) + at_home + mask_mandate + evict_end + offset(log(pop/100000)),  data=county_cases2))
stargazer(mod_nb3c, apply.coef = exp, type='text')
anova(mod_nb3b, mod_nb3c,  test="Chisq")
rse_mod_nb3c<-exp(coeftest(mod_nb3c, vcov=vcovCL(mod_nb3c,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_nb3c<-exp(coefci(mod_nb3c, vcov=vcovCL(mod_nb3c,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb3c
rci_mod_nb3c
#compare models 
models <- list(mod_nb1, mod_nb1off, mod_nb2off, mod_nb3)
models1<-list(mod_nb3a, mod_nb3b, mod_nb3c)

stargazer(models, apply.coef=exp, type = "text", ci = TRUE, title="Base Models", out="results/table1.txt")
stargazer(models1, apply.coef=exp, type="text", ci = TRUE, title="Models w/ NPIs", out="results/table1a.txt")

##############################################################################################################################################
#Rerun models w/ linear regression (basic did assumption)
#unadjusted
summary(lin_unajd<-lm(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb3, apply.coef = exp, type='text')
rse_mod_nb3f<-exp(coeftest(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS)))
rci_mod_nb3<-exp(coefci(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS)))
rse_mod_nb3
rci_mod_nb3


#adjusted
summary(lin_adj<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_end + offset(log(pop/100000)),  data=county_cases2))
stargazer(mod_nb3c, apply.coef = exp, type='text')
anova(mod_nb3b, mod_nb3c,  test="Chisq")
rse_mod_nb3c<-exp(coeftest(mod_nb3c, vcov=vcovCL(mod_nb3c,type="HC1",cluster=~FIPS)))
rse_mod_nb3c<-exp(coefci(mod_nb3c, vcov=vcovCL(mod_nb3c,type="HC1",cluster=~FIPS)))
rse_mod_nb3c
rci_mod_nb3c


#####################################################################################
####code for testing negative vs binomial model. 
#test poisson vs negative binomial-- for final model
#can compare LR using LR test bc poisson nested in neg. binomial
summary(mod_3c<-glm(daily_count~treat1*pre_post + at_home + mask_mandate + end_evict + offset(log(pop/100000)), family="poisson", data=county_cases2))

summary(mod_negbin <- glm.nb(daily_count ~treat1*pre_post + at_home + mask_mandate + end_evict + offset(log(pop/100000)), data = county_cases2))
#conduct LR test since poisson is nested in neg binomial 
as.numeric(2 * (logLik(mod_3c) - logLik(mod_negbin)), df = 1, lower.tail = FALSE)
lrtest(mod_3c, mod_negbin)
AIC(mod_negbin)
AIC(mod_2off)
BIC(mod_negbin)
BIC(mod_2off)
#create df w/ model results 
ic <- data.frame(Model = c("negative-binomial", "poisson"),
                 AIC = c(AIC(mod_negbin), AIC(mod_2off)),
                 BIC = c(BIC(mod_negbin), BIC(mod_2off)), 
                 stringsAsFactors = FALSE) 
print(xtable(ic, caption = "Information criteria results",
             label = "tab:ic_models", type="html"))


#####################################################################################

#Sensitivity analysis for cases 
#testing alternative periods 

#**********************************

#**********************************
#*two way fixed effects model (controlling for city and calendar week time)
#create new var that is essentially interaction of treat and pre_post (0 in pre 1 for treat and control, 1 for treat 14 days post time zero)
county_cases2<-county_cases2%>%
  mutate(policy=treat1*pre_post)

summary(mod_nb3<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week)+at_home + mask_mandate + evict_ban+ offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb3, apply.coef = exp, type='text')
rse_mod_nb3<-exp(coeftest(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_nb3<-exp(coefci(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb3
rci_mod_nb3

#state fixed effect
summary(mod_s2e2<-glm.nb(daily_count~treat1*pre_post + factor(Province_State)+ at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2e))
stargazer(mod_s2e2, apply.coef = exp, type='text')
rse_mod_s2e2<-exp(coeftest(mod_s2e2, vcov=vcovCL(mod_s2e2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2e2<-exp(coefci(mod_s2e2, vcov=vcovCL(mod_s2e2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2e2
rci_mod_s2e2

#*#sensitivity 1
#Increase study period to 12 weeks 

summary(mod_s2e1<-glm.nb(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2e))
rse_mod_s2e1-exp(coeftest(mod_s2e1, vcov=vcovCL(mod_s2e1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2e1<-exp(coefci(mod_s2e1, vcov=vcovCL(mod_s2e1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2e1
rci_mod_s2e1

summary(mod_s2e2<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2e))
stargazer(mod_s2e2, apply.coef = exp, type='text')
rse_mod_s2e2<-exp(coeftest(mod_s2e2, vcov=vcovCL(mod_s2e2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2e2<-exp(coefci(mod_s2e2, vcov=vcovCL(mod_s2e2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2e2
rci_mod_s2e2

###Sensivity 2: 9 day lag

summary(mod_s2b1<-glm.nb(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2b))
rse_mod_s2b1<-exp(coeftest(mod_s2b1, vcov=vcovCL(mod_s2b1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2b1<-exp(coefci(mod_s2b1, vcov=vcovCL(mod_s2b1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2b1
rci_mod_s2b1

summary(mod_s2b2<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2b))
stargazer(mod_s2b2, apply.coef = exp, type='text')
rse_mod_s2b2<-exp(coeftest(mod_s2b2, vcov=vcovCL(mod_s2b2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2b2<-exp(coefci(mod_s2b2, vcov=vcovCL(mod_s2b2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2b2
rci_mod_s2b2
#**********************************
###Sensitivity 3: increase lag to 3 weeks (21 days)
summary(mod_s2c1<-glm.nb(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2c))
rse_mod_s2c1<-exp(coeftest(mod_s2c1, vcov=vcovCL(mod_s2c1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2c1<-exp(coefci(mod_s2c1, vcov=vcovCL(mod_s2c1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2c1
rci_mod_s2c1

summary(mod_s2c2<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2c))
stargazer(mod_s2c2, apply.coef = exp, type='text')
rse_mod_s2c2<-exp(coeftest(mod_s2c2, vcov=vcovCL(mod_s2c2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2c2<-exp(coefci(mod_s2c2, vcov=vcovCL(mod_s2c2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2c2
rci_mod_s2c2
###Sensivity 4: increase lag to 4 weeks (28 days)
summary(mod_s2d1<-glm.nb(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2d))
rse_mod_s2d1<-exp(coeftest(mod_s2d1, vcov=vcovCL(mod_s2d1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2d1<-exp(coefci(mod_s2d1, vcov=vcovCL(mod_s2d1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2d1
rci_mod_s2d1

summary(mod_s2d2<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2d))
stargazer(mod_s2d2, apply.coef = exp, type='text')
rse_mod_s2d2<-exp(coeftest(mod_s2d2, vcov=vcovCL(mod_s2d2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2d2<-exp(coefci(mod_s2d2, vcov=vcovCL(mod_s2d2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2d2
rci_mod_s2d2

###remove the non bchc cities 
summary(mod_s2f1<-glm.nb(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2f))
rse_mod_s2f1<-exp(coeftest(mod_s2f1, vcov=vcovCL(mod_s2f1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2f1<-exp(coefci(mod_s2f1, vcov=vcovCL(mod_s2f1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2f1
rci_mod_s2f1

summary(mod_s2f2<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2f))
stargazer(mod_s2f2, apply.coef = exp, type='text')
rse_mod_s2f2<-exp(coeftest(mod_s2f2, vcov=vcovCL(mod_s2f2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2f2<-exp(coefci(mod_s2f2, vcov=vcovCL(mod_s2f2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2f2
rci_mod_s2f2

##remove SF 
summary(mod_s2g2<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_end + offset(log(pop/100000)),  data=filter(county_cases2, Admin2!="San Francisco")))
stargazer(mod_s2g2, apply.coef = exp, type='text')
rse_mod_s2g2<-exp(coeftest(mod_s2g2, vcov=vcovCL(mod_s2g2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2g2<-exp(coefci(mod_s2g2, vcov=vcovCL(mod_s2g2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2g2
rci_mod_s2g2

#add in the calendar week & city fixed effects
summary(mod_s2h2c<-glm.nb(daily_count~treat1 + at_home + mask_mandate + evict_end +factor(FIPS)+  factor(cal_week)+ offset(log(pop/100000)),  data=county_cases2))
stargazer(mod_s2h2, apply.coef = exp, type='text')
rse_mod_s2h2-exp(coeftest(mod_s2h2, vcov=vcovCL(mod_s2h2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2h2<-exp(coefci(mod_s2h2, vcov=vcovCL(mod_s2h2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2h2
rci_mod_s2h2

##model output 
#changes in lag periods  
#just interaction models
models_sen <- list(mod_s2a1,  mod_s2b1, mod_s2c1, mod_s2d1)
#just final models
models_sen1 <- list(mod_s2a2, mod_s2b2, mod_s2c2, mod_s2d2)

stargazer(models_sen, apply.coef=exp, type = "text", ci = TRUE, title="Sensitivity_change in lags", out="results/table_1sa_cases.txt")
stargazer(models_sen1, apply.coef=exp, type = "text", ci = TRUE, title="Sensitivity_change in lags_full model", out="results/table_1sa2_cases.txt")

#model output additional sensitivity analysis 
models_sen2 <- list(mod_s2e1, mod_s2e2, mod_s2f1,mod_s2f2)
stargazer(models_sen2, apply.coef=exp, type = "text", ci = TRUE, title="Sensitivity_period, BCHC", out="results/table_1sb_cases.txt")


#two way fixed effects DiD model

#create new var that is already an interaction of policy + pre-post 
county_casestest<-county_cases2%>%
  mutate(anypolicy=treat1*pre_post)

#unadjusted 
summary(mod_nb3<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week)+ offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb3, apply.coef = exp, type='text')
rse_mod_nb3<-exp(coeftest(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_nb3<-exp(coefci(mod_nb3, vcov=vcovCL(mod_nb3,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb3
rci_mod_nb3

#adjusted
summary(fixed<-glm.nb(daily_count~anypolicy + factor(cities)+ factor(cal_week) + at_home + mask_mandate + evict_ban + offset(log(pop/100000)), data=county_casestest))
stargazer(fixed, apply.coef = exp, type='text')
rse_fixed<-exp(coeftest(fixed, vcov=vcovCL(fixed,type="HC1",cluster=~FIPS+Province_State)))
rci_fixed<-exp(coefci(fixed, vcov=vcovCL(fixed,type="HC1",cluster=~FIPS+Province_State)))
rse_fixed
rci_fixed

summary(mod_nb3<-glm.nb(daily_count~treat1*pre_post+ factor(cities)+  factor(cal_week)+ offset(log(pop/100000)), data=county_casestest))
stargazer(mod_nb3, apply.coef = exp, type='text')



#######################################################
#Event Model 
#######################################################
#base model 
#just weeks
summary(mod_e1<-glm.nb(daily_count~ weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 + offset(log(pop/100000)),  data=event_model1))
stargazer(mod_e1, apply.coef = exp, type='text')
#robust standard errors clustered at city level 
rse_mod_e1<-exp(coeftest(mod_e1, vcov=vcovCL(mod_e1,type="HC1",cluster=~FIPS++Province_State)))
rci_mod_e1<-exp(coefci(mod_e1, vcov=vcovCL(mod_e1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e1
rci_mod_e1

#adding city fixed effects
summary(mod_e2<-glm.nb(daily_count~ weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8  + factor(cities) + offset(log(pop/100000)),  data=event_model1))
rse_mod_e2<-exp(coeftest(mod_e2, vcov=vcovCL(mod_e2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e2<-exp(coefci(mod_e2, vcov=vcovCL(mod_e2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e2
rci_mod_e2
#adding calendar week fixed effects 
summary(mod_e3<-glm.nb(daily_count~weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8  + factor(cal_week) + factor(cities) + offset(log(pop/100000)),  data=event_model1))
rse_mod_e3<-exp(coeftest(mod_e3, vcov=vcovCL(mod_e3,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e3<-exp(coefci(mod_e3, vcov=vcovCL(mod_e3,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e3
rci_mod_e3


#adding mask mandate (factor) 
summary(mod_e4a<-glm.nb(daily_count~weeks_prior +weeks_post+ cal_week + cities +mask_week + offset(log(pop/100000)),  data=event_model3))
rse_mod_e4a<-exp(coeftest(mod_e4a, vcov=vcovCL(mod_e4a,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e4a<-exp(coefci(mod_e4a, vcov=vcovCL(mod_e4a,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e4a
rci_mod_e4a

#adding stay at home order (factor)
summary(mod_e4b<-glm.nb(daily_count~weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8  + factor(cal_week) + factor(cities) +mask_week + stay_week+ offset(log(pop/100000)),  data=event_model1))
rse_mod_e4b<-exp(coeftest(mod_e4b, vcov=vcovCL(mod_e4b,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e4b<-exp(coefci(mod_e4b, vcov=vcovCL(mod_e4b,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e4b
rci_mod_e4b
#adding eviction ban (factor)
####################################
####Final Event Model (Main Model)
####################################
summary(mod_e4c<-glm.nb(daily_count~  weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 + factor(cal_week) + factor(cities) + mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model1))
stargazer(mod_e4c, apply.coef=exp, type="text", title="IRRs event model")
rse_mod_e4c<-exp(coeftest(mod_e4c, vcov=vcovCL(mod_e4c,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e4c<-exp(coefci(mod_e4c, vcov=vcovCL(mod_e4c,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e4c
rci_mod_e4c


#store model output
event_models<-list(mod_e1, mod_e2, mod_e3, mod_e4a, mod_e4b, mod_e4c)
stargazer(event_models, apply.coef=exp, type="text", title="IRRs event model", out="results/table2_event.txt")

#sensitivity 1: extend to 12 weeks post (10 before)
#using final model 
summary(mod_e4b<-glm.nb(daily_count~ weeks_prior_2+ weeks_prior_3+weeks_prior_4  + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 +weeks_post_9 +weeks_post_10 + weeks_post_11 +weeks_post_12  + factor(cal_week) + cities +mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model2))
stargazer(mod_e4b, apply.coef=exp, type="text", title="IRRs event model")
rse_mod_e4b<-exp(coeftest(mod_e4b, vcov=vcovCL(mod_e4b,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e4b<-exp(coefci(mod_e4b, vcov=vcovCL(mod_e4b,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e4b
rci_mod_e4b

#sensivity 2
summary(mod_e4d<-glm.nb(daily_count~ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 +weeks_post_9 +weeks_post_10 + weeks_post_11 +weeks_post_12  + factor(cal_week) + cities +mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model3))
stargazer(mod_e4d, apply.coef=exp, type="text", title="IRRs event model")
rse_mod_e4d<-exp(coeftest(mod_e4d, vcov=vcovCL(mod_e4d,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e4d<-exp(coefci(mod_e4d, vcov=vcovCL(mod_e4d,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e4d
rci_mod_e4d



####################################################
# Models for Deaths 
####################################################

#########WE DON"T RUN ANY OF THIS FOR THE ARTICLE - will need to update SE if we rerun for some reason

#start w/ just pre/post
county_deaths2$pre_post<-as.factor(county_deaths2$pre_post)

#in earlier model building we found Neg binomial a better fit for the model, so using this for model building
summary(mod_nb1 <- glm.nb(daily_deaths ~pre_post,  data = county_deaths2))
stargazer(mod_nb1, apply.coef = exp, type='text')
AIC(mod_nb1)
BIC(mod_nb1)
rse_mod_nb1<-exp(coeftest(mod_nb1, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb1<-exp(coefci(mod_nb1, vcov = vcovHC,  cluster= ~cities))

#add offset (could also consider test offset)
##we add the per 100000 to make a per 100,000 rate offset
summary(mod_nb1off<-glm.nb(daily_deaths~pre_post + offset(log(pop/100000)), data=county_deaths2))
stargazer(mod_nb1off, apply.coef = exp, type='text')
AIC(mod_nb1off)
BIC(mod_nb1off)
rse_mod_nb1off<-exp(coeftest(mod_nb1off, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb1off<-exp(coefci(mod_nb1off, vcov = vcovHC,  cluster= ~cities))

#offset better fit for model- include from here on. 

#add treat 
summary(mod_nb2off<-glm.nb(daily_deaths~pre_post +treat1+ offset(log(pop/100000)), data=county_deaths2))
stargazer(mod_nb2off, apply.coef = exp, type='text')
rse_nb2off<-exp(coeftest(mod_nb2off, vcov = vcovHC,  cluster= ~cities))
rci_nb2off<-exp(coefci(mod_nb2off, vcov = vcovHC,  cluster= ~cities))

#add interaction 
summary(mod_nb3<-glm.nb(daily_deaths~treat1*pre_post + offset(log(pop/100000)), data=county_deaths2))
stargazer(mod_nb3, apply.coef = exp, type='text', ci = TRUE)
rse_mod_nb3<-exp(coeftest(mod_nb3, vcov = vcovHC,  cluster= ~cities))
rse_mod_nb3
rci_mod_nb3<-exp(coefci(mod_nb3, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3
########add other policies########
#stay at home order
summary(mod_nb3a<-glm.nb(daily_deaths~treat1*pre_post + at_home + offset(log(pop/100000)), data=county_deaths2))
stargazer(mod_nb3a, apply.coef = exp, type='text')
anova(mod_nb3, mod_nb3a,  test="Chisq")
rse_mod_nb3a<-exp(coeftest(mod_nb3a, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3a<-exp(coefci(mod_nb3a, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3a
summary(county_cases2$date)
#mask mandate
summary(mod_nb3b<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + offset(log(pop/100000)), data=county_deaths2))
stargazer(mod_nb3b, apply.coef = exp, type='text')
anova(mod_nb3a, mod_nb3b,  test="Chisq")
rse_mod_nb3b<-exp(coeftest(mod_nb3b, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3b<-exp(coefci(mod_nb3b, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3b
#eviction ban 
summary(mod_nb3c<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2))
stargazer(mod_nb3c, apply.coef = exp, type='text')
anova(mod_nb3b, mod_nb3c,  test="Chisq")
rse_mod_nb3c<-exp(coeftest(mod_nb3c, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3c<-exp(coefci(mod_nb3c, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3c
#compare models 
modelsdeath <- list(mod_nb1, mod_nb1off, mod_nb2off, mod_nb3)
modelsdeath1<-list(mod_nb3a, mod_nb3b, mod_nb3c)

stargazer(modelsdeath, apply.coef=exp, type = "text", title="Base Models", out="results/table1deaths.txt")
stargazer(modelsdeath1, apply.coef=exp, type="text", title="Models w/ NPIs", out="results/table1bdeaths.txt")

#################################################################################
#Sensitivity Analysis
#repeat above models for different treatment period 

#sensitivity 1:create pre post with 42 days after
# 14 days for cases, + 28 for death lag  
summary(mod_ds1b<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2b))
rse_mod_ds1b<-exp(coeftest(mod_ds1b, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1b<-exp(coefci(mod_ds1b, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1b
#sensitivity analysis 2: 
#remove lag for other NPIs
summary(mod_ds1c<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2c))
stargazer(mod_ds1c, apply.coef=exp, type="text")
rse_mod_ds1c<-exp(coeftest(mod_ds1c, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1c<-exp(coefci(mod_ds1c, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1c
# sensitivity analysis 3
#only 7 day case lag + 21 day death 
summary(mod_ds1d<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2d))
rse_mod_ds1d<-exp(coeftest(mod_ds1d, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1d<-exp(coefci(mod_ds1d, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1d
#Sensitivity 4
#remove the non bchc cities 
summary(mod_ds1e<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2e))
rse_mod_ds1e<-exp(coeftest(mod_ds1e, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1e<-exp(coefci(mod_ds1e, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1e

model_deaths_s1<-list(mod_ds1b, mod_ds1c, mod_ds1d, mod_ds1e)
stargazer(model_deaths_s1, apply.coef=exp, type="text", title="sensitivity death 1", out="results/table1a_deathsS4.txt")

robust_se_death<-list(rse_mod_ds1b, rse_mod_ds1c, rse_mod_ds1d, rse_mod_ds1e)
robust_ci_death<-list(rci_mod_ds1b, rci_mod_ds1c, rci_mod_ds1d, rci_mod_ds1e)


###############################################################################
#Event Models-Deaths
###############################################################################

summary(mod_e1<-glm.nb(daily_count~weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 + offset(log(pop/100000)),  data=event_model_death1))
stargazer(mod_e1, apply.coef = exp, type='text')
#robust standard errors clustered at city level 
rse_mod__e1<-exp(coeftest(mod_e1, vcov = vcovHC,  cluster= ~cities))
rci_mod_e1<-exp(coefci(mod_e1, vcov = vcovHC,  cluster= ~cities))

#adding city fixed effects
summary(mod_e2<-glm.nb(daily_count~weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8  + cities + offset(log(pop/100000)),  data=event_model_death1))
rse_mod_e2<-exp(coeftest(mod_e2, vcov = vcovHC,  cluster= ~cities))
rci_mod_e2<-exp(coefci(mod_e2, vcov = vcovHC,  cluster= ~cities))

#adding calendar week fixed effects 
summary(mod_e3<-glm.nb(daily_count~weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8  + factor(cal_week) + factor(cities) + offset(log(pop/100000)),  data=event_model_death1))
rse_mod_e3<-exp(coeftest(mod_e3, vcov = vcovHC,  cluster= ~cities))
rci_mod_e3<-exp(coefci(mod_e3, vcov = vcovHC,  cluster= ~cities))

#adding mask mandate (factor) 
summary(mod_e4a<-glm.nb(daily_count~weeks_prior +weeks_post+ cal_week + cities +mask_week + offset(log(pop/100000)),  data=event_model3))
rse_mod_e4a<-exp(coeftest(mod_e4a, vcov = vcovHC,  cluster= ~cities))
rci_mod_e4a<-exp(coefci(mod_e4a, vcov = vcovHC,  cluster= ~cities))
levels(event_model2$cities)

#adding stay at home order (factor)
summary(mod_e4b<-glm.nb(daily_count~weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8  + factor(cal_week) + factor(cities) +mask_week + stay_week+ offset(log(pop/100000)),  data=event_model_death1))
rse_mod_e4b<-exp(coeftest(mod_e4b, vcov = vcovHC,  cluster= ~cities))
rci_mod_e4b<-exp(coefci(mod_e4b, vcov = vcovHC,  cluster= ~cities))

#adding eviction ban (factor)
summary(mod_e4c<-glm.nb(daily_count~ weeks_prior_1+ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 + factor(cal_week) + cities +mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model_death1))
stargazer(mod_e4c, apply.coef=exp, type="text", title="IRRs event model")
rse_mod_e4c<-exp(coeftest(mod_e4c, vcov = vcovHC,  cluster= ~cities))
rci_mod_e4c<-exp(coefci(mod_e4c, vcov = vcovHC,  cluster= ~cities))
rci_mod_e4c
rse_mod_e4c

################################################################################
#      ************************Descriptive data***********************
################################################################################

#county cases 
#total cases 
cumulative1<-county_cases2%>%
  summarize(total=sum(daily_count))%>%
  summarize(tots=sum(total))


means<-county_cases2%>%
  group_by(treat1, pre_post, FIPS)%>%
  summarize(sum=sum(daily_count))%>%
  left_join(population1)%>%
  mutate(rate=sum/pop*100000)

#daily case rate, pre and post 
means_2<-county_cases2%>%
  group_by(treat1, pre_post)%>%
  summarize(mean=mean(case_rate), 
            weighted.mean(case_rate, pop))

means_3<-county_cases2%>%
  group_by(cities, pre_post)%>%
  summarize(mean=mean(case_rate))

#total population 
total_pop<-population1%>%
  summarize(total=sum(pop))

popmean<-county_cases2%>%
  group_by(treat1)%>%
  summarize(median=median(pop), 
            mean=mean(pop))

#table1


