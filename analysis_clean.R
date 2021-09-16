###***********************Data Analysis_ indoor dining & preemption**************************

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
library(tidycensus)
#import data files
load("data/daily_count.Rdata")
load("data/event_model1.Rdata")
load("data/roll_avg.Rdata")
load("data/roll_avg_death.Rdata")
load("data/daily_count_deaths.Rdata")
load("data/county_cases_sens2.Rdata")
load("data/daily_count_2a.Rdata")
load("data/daily_count_deaths.Rdata")
load("data/sens2_all.Rdata")
cities<-c(42101,18097,6075, 55079, 4013, 48453, 48113, 48201,  48029, 13121,45019)

# export county_cases2 for stata (for marginal mean estimates)
write.dta(county_cases2,"county_cases2.dta")
####################################################
#Appendix Figure A1
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
                      Admin2 %in% c("Travis", "Dallas", "Harris", "Bexar")~ "2020-05-01",
                      Admin2=="Maricopa" ~"2020-05-11",
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


pdf(file="results/figure_1a.pdf")

figure1a<-figure1_data%>%
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
  

figure1a
dev.off()


plots = replicate(8, qplot(1,1), simplify=FALSE)
library(gridExtra)
p <- do.call(marrangeGrob, c(figure1a,ncol=1,nrow=1))

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
#APPENDIX Figure D4
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


##############################################################################
#Model Building 
#############################################################################

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
##############################
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
##########################
#Final Adjusted MOdel 
#########################
#add eviction ban
summary(mod_nb3c<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_end + offset(log(pop/100000)),  data=county_cases2))
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
stargazer(models1, apply.coef=exp, type="text", ci = TRUE, title="Models w/ NPIs & FE", out="results/table1a.txt")

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
#Sensitivity analysis
#####################################################################################

#**********************************

#add city fixed effects
#Final adjusted model
summary(mod_nb3d<-glm.nb(daily_count~treat1*pre_post + factor(cities)+ at_home + mask_mandate + evict_end + offset(log(pop/100000)),  data=county_cases2))
stargazer(mod_nb3d, apply.coef = exp, type='text')
anova(mod_nb3c, mod_nb3d,  test="Chisq")
rse_mod_nb3d<-exp(coeftest(mod_nb3d, vcov=vcovCL(mod_nb3d,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_nb3d<-exp(coefci(mod_nb3d, vcov=vcovCL(mod_nb3d,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_nb3d
rci_mod_nb3d

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

AIC(mod_nb3)
AIC(mod_nb3a)

#state fixed effect
summary(mod_s2e2<-glm.nb(daily_count~treat1*pre_post + factor(Province_State)+ at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2e))
stargazer(mod_s2e2, apply.coef = exp, type='text')
rse_mod_s2e2<-exp(coeftest(mod_s2e2, vcov=vcovCL(mod_s2e2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2e2<-exp(coefci(mod_s2e2, vcov=vcovCL(mod_s2e2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2e2
rci_mod_s2e2

#Increase study period to 12 weeks 
county_cases2e<-county_cases2e%>%
  mutate(policy=treat1*pre_post)

summary(mod_s2e1<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + offset(log(pop/100000)), data=county_cases2e))
stargazer(mod_s2e1, apply.coef = exp, type='text')
rse_mod_s2e1-exp(coeftest(mod_s2e1, vcov=vcovCL(mod_s2e1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2e1<-exp(coefci(mod_s2e1, vcov=vcovCL(mod_s2e1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2e1
rci_mod_s2e1

summary(mod_s2e2<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week)+ at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2e))
stargazer(mod_s2e2, apply.coef = exp, type='text')
rse_mod_s2e2<-exp(coeftest(mod_s2e2, vcov=vcovCL(mod_s2e2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2e2<-exp(coefci(mod_s2e2, vcov=vcovCL(mod_s2e2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2e2
rci_mod_s2e2

###Sensivity: 9 day lag
county_cases2b<-county_cases2b%>%
  mutate(policy=treat1*pre_post)
summary(mod_s2b1<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + offset(log(pop/100000)), data=county_cases2b))
rse_mod_s2b1<-exp(coeftest(mod_s2b1, vcov=vcovCL(mod_s2b1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2b1<-exp(coefci(mod_s2b1, vcov=vcovCL(mod_s2b1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2b1
rci_mod_s2b1

summary(mod_s2b2<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2b))
stargazer(mod_s2b2, apply.coef = exp, type='text')
rse_mod_s2b2<-exp(coeftest(mod_s2b2, vcov=vcovCL(mod_s2b2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2b2<-exp(coefci(mod_s2b2, vcov=vcovCL(mod_s2b2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2b2
rci_mod_s2b2
###Sensitivity: increase lag to 3 weeks (21 days)
county_cases2c<-county_cases2c%>%
  mutate(policy=treat1*pre_post)

summary(mod_s2c1<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week)+ offset(log(pop/100000)), data=county_cases2c))
rse_mod_s2c1<-exp(coeftest(mod_s2c1, vcov=vcovCL(mod_s2c1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2c1<-exp(coefci(mod_s2c1, vcov=vcovCL(mod_s2c1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2c1
rci_mod_s2c1

summary(mod_s2c2<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2c))
stargazer(mod_s2c2, apply.coef = exp, type='text')
rse_mod_s2c2<-exp(coeftest(mod_s2c2, vcov=vcovCL(mod_s2c2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2c2<-exp(coefci(mod_s2c2, vcov=vcovCL(mod_s2c2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2c2
rci_mod_s2c2
###Sensivity 4: increase lag to 4 weeks (28 days)
county_cases2d<-county_cases2d%>%
  mutate(policy=treat1*pre_post)

summary(mod_s2d1<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + offset(log(pop/100000)), data=county_cases2d))
rse_mod_s2d1<-exp(coeftest(mod_s2d1, vcov=vcovCL(mod_s2d1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2d1<-exp(coefci(mod_s2d1, vcov=vcovCL(mod_s2d1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2d1
rci_mod_s2d1

summary(mod_s2d2<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2d))
stargazer(mod_s2d2, apply.coef = exp, type='text')
rse_mod_s2d2<-exp(coeftest(mod_s2d2, vcov=vcovCL(mod_s2d2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2d2<-exp(coefci(mod_s2d2, vcov=vcovCL(mod_s2d2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2d2
rci_mod_s2d2

###remove the non bchc cities 
county_cases2f<-county_cases2f%>%
  mutate(policy=treat1*pre_post)
summary(mod_s2f1<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + offset(log(pop/100000)), data=county_cases2f))
rse_mod_s2f1<-exp(coeftest(mod_s2f1, vcov=vcovCL(mod_s2f1,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2f1<-exp(coefci(mod_s2f1, vcov=vcovCL(mod_s2f1,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2f1
rci_mod_s2f1

summary(mod_s2f2<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2f))
stargazer(mod_s2f2, apply.coef = exp, type='text')
rse_mod_s2f2<-exp(coeftest(mod_s2f2, vcov=vcovCL(mod_s2f2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2f2<-exp(coefci(mod_s2f2, vcov=vcovCL(mod_s2f2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2f2
rci_mod_s2f2

##remove SF 
county_cases2<-county_cases2%>%
  mutate(policy=treat1*pre_post)

summary(mod_s2g2<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + at_home + mask_mandate + evict_end + offset(log(pop/100000)),  data=filter(county_cases2, Admin2!="San Francisco")))
stargazer(mod_s2g2, apply.coef = exp, type='text')
rse_mod_s2g2<-exp(coeftest(mod_s2g2, vcov=vcovCL(mod_s2g2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2g2<-exp(coefci(mod_s2g2, vcov=vcovCL(mod_s2g2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2g2
rci_mod_s2g2

##control for weeks pre re-opening
#add the weeks data to the dataset 

county_cases_weeks<-county_cases2%>%
  mutate(weeks_pre=case_when(Admin2=="San Francisco"~23.9, 
                             Admin2=="Indianapolis"~7.0, 
                             Admin2=="Philadelphia"~13.6, 
                             Admin2=="Milwaukee"~7.0, 
                             Admin2=="Maricopa"~5.9, 
                             Admin2=="Fulton"~4.9, 
                             Admin2=="Charleston"~6.4, 
                             Admin2=="Travis"~5.4,
                             Admin2=="Dallas"~5.7, 
                             Admin2=="Harris"~5.4, 
                             Admin2=="Bexar"~5.4), 
        policy=treat1*pre_post)

summary(mod_s2h2<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + at_home + mask_mandate + evict_end + weeks_pre+ offset(log(pop/100000)),  data=county_cases_weeks))
stargazer(mod_s2h2, apply.coef=exp, type='text')
rse_mod_s2h2<-exp(coeftest(mod_s2h2, vcov=vcovCL(mod_s2h2,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_s2h2<-exp(coefci(mod_s2h2, vcov=vcovCL(mod_s2h2,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_s2h2
rci_mod_s2h2

#adjust for unbalanced pre-trend variables (and pre weeks)
county_cases_pretrend<-county_cases_weeks%>%
  rename(GEOID='FIPS')%>%
  left_join(data,by='GEOID')%>%
mutate(mask_wearing=case_when(Admin2=="San Francisco"~93.8, 
            Admin2=="Indianapolis"~79.4, 
            Admin2=="Philadelphia"~91.4, 
            Admin2=="Milwaukee"~76.1, 
            Admin2=="Maricopa"~89.2, 
            Admin2=="Fulton"~94.9, 
            Admin2=="Charleston"~84.3, 
            Admin2=="Travis"~95.3,
            Admin2=="Dallas"~89.8, 
            Admin2=="Harris"~88.8, 
            Admin2=="Bexar"~91.2), 
       mutate(policy=treat1*pre_post)
  

summary(mod_s2i2<-glm.nb(daily_count~policy + factor(cities)+ factor(cal_week) + at_home + mask_mandate + evict_end + weeks_pre+ pct_age0017+ pct_age1864+ pct_age65plus+pct_transit + mask_wearing+ offset(log(pop/100000)),  data=county_cases_pretrend))
stargazer(mod_s2i2, apply.coef=exp, type='text')
rse_mod_s2i2<-exp(coeftest(mod_s2i2, vcov=vcovCL(mod_s2i2,type="HC1",cluster=~GEOID+Province_State)))
rci_mod_s2i2<-exp(coefci(mod_s2i2, vcov=vcovCL(mod_s2i2,type="HC1",cluster=~GEOID+Province_State)))
rse_mod_s2i2
rci_mod_s2i2
  
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

#sensitivity 1: extend to 12 weeks post (4 before)
#using final model 
summary(mod_e4b<-glm.nb(daily_count~ weeks_prior_2+ weeks_prior_3+weeks_prior_4  + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 +weeks_post_9 +weeks_post_10 + weeks_post_11 +weeks_post_12  + factor(cal_week) + factor(cities) +mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model2))
stargazer(mod_e4b, apply.coef=exp, type="text", title="IRRs event model")
rse_mod_e4b<-exp(coeftest(mod_e4b, vcov=vcovCL(mod_e4b,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e4b<-exp(coefci(mod_e4b, vcov=vcovCL(mod_e4b,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e4b
rci_mod_e4b

#sensivity 2
summary(mod_e4d<-glm.nb(daily_count~ weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 +weeks_post_9 +weeks_post_10 + weeks_post_11 +weeks_post_12  + factor(cal_week) + factor(cities) +mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model3))
stargazer(mod_e4d, apply.coef=exp, type="text", title="IRRs event model")
rse_mod_e4d<-exp(coeftest(mod_e4d, vcov=vcovCL(mod_e4d,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e4d<-exp(coefci(mod_e4d, vcov=vcovCL(mod_e4d,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e4d
rci_mod_e4d

#sensitivity 3: changing lags
#same as main event model, but using event_model4 & event_model5
#3 week NPI lag 

summary(mod_e3wk<-glm.nb(daily_count~  weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 + factor(cal_week) + factor(cities) + mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model4))
stargazer(mod_e3wk, apply.coef=exp, type="text", title="IRRs event model")
rse_mod_e3wk<-exp(coeftest(mod_e3wk, vcov=vcovCL(mod_e3wk,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e3wk<-exp(coefci(mod_e3wk, vcov=vcovCL(mod_e3wk,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e3wk
rci_mod_e3wk

#4 week NPI lag

summary(mod_e4wk<-glm.nb(daily_count~  weeks_prior_2+ weeks_prior_3+weeks_prior_4 + weeks_post_1 +weeks_post_2 + weeks_post_3+ weeks_post_4 +weeks_post_5 +weeks_post_6+ weeks_post_7+weeks_post_8 + factor(cal_week) + factor(cities) + mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model5))
stargazer(mod_e4wk, apply.coef=exp, type="text", title="IRRs event model")
rse_mod_e4wk<-exp(coeftest(mod_e4wk, vcov=vcovCL(mod_e4wk,type="HC1",cluster=~FIPS+Province_State)))
rci_mod_e4wk<-exp(coefci(mod_e4wk, vcov=vcovCL(mod_e4wk,type="HC1",cluster=~FIPS+Province_State)))
rse_mod_e4wk
rci_mod_e4wk


##############################################################################
#Event Study Model: Appendix FIgure D3

#load dataset w/ output from event study model
dta<-read_excel("data/tabled3.xlsx")

models<-colnames(dta)[-1]
dta<-dta %>% rename(week=1) %>% 
  gather(type, value, -week) %>% 
  mutate(value=gsub("\\*", "", value),
         coef=substr(value, 1, regexpr("\\(", value)-1),
         lci=substr(value, regexpr("\\(", value)+1, regexpr("\\,", value)-1),
         uci=substr(value, regexpr("\\,", value)+1, regexpr("\\)", value)-1),
         coef=ifelse(value=="Ref", 1, coef),
         lci=ifelse(value=="Ref", 1, lci),
         uci=ifelse(value=="Ref", 1, uci),
         coef=as.numeric(coef),
         lci=as.numeric(lci),
         uci=as.numeric(uci),
         week=ifelse(week<0, week+1, week),
         type=factor(type, levels=models))
fontsize<-20
ggplot(data=dta, aes(x=week, y=coef, group=type))+
  #geom_ribbon(aes(ymin=lci, ymax=uci, group=type), linetype=2, fill="gray80")+
  geom_hline(yintercept = 1, lty=2)+
  geom_linerange(aes(ymin=lci, ymax=uci, group=type), position=position_dodge(width=0.3))+
  geom_line(aes(lty=type), position=position_dodge(width=0.3))+
  geom_point(aes(shape=type), color="black", fill="gray", position=position_dodge(width=0.3)) + 
  geom_vline(aes(xintercept=0), color="black")+
  scale_y_continuous(trans="log", breaks=pretty_breaks())+
  labs(title = "",
       y = "IRR",
       x = "Weeks since dining allowed to re-open") +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=16),
        axis.title=element_text(color="black", size=16, face="bold"))


ggplot(data=dta, aes(x=week, y=coef, group=type))+
  geom_ribbon(aes(ymin=lci, ymax=uci, group=type, fill=type), alpha=0.2)+
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = 0, lty=2)+
  geom_line(aes(color=type))+
  geom_point(aes(shape=type,fill=type), color="black") + 
  scale_y_continuous(trans="log", breaks=2^c(-1:5))+
  scale_x_continuous(breaks=seq(-4, 12, by=2))+
  scale_shape_manual(values=c(21, 22, 23), name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  labs(title = "",
       y = "IRR (95% CI)",
       x = "Weeks since indoor dining allowed to re-open") +
  theme_bw()+
  theme(legend.text=element_text(color="black", size=fontsize),
        legend.background = element_blank(),
        #legend.position = "bottom",
        legend.position=c(0.2,.8),
        axis.text=element_text(color="black", size=fontsize),
        axis.title=element_text(color="black", size=fontsize, face="bold"),
        plot.title = element_text(color="black", size=fontsize, face="bold"),
        strip.text = element_text(color="black", size=fontsize, face="bold"),
        strip.background = element_blank())
ggsave("results/ES_Figure_D3.pdf", width=10, height=7)
ggsave("results/ES_Figure_D3.png", width=10, height=7)
ggsave("results/ES_Figure_D3.jpg", width=10, height=7)

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

##################################################################################
### TABLE 1

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
  female<-paste0(format(median(.x$pct_female), nsmall=1, digits=1), "[",
                 format(min(.x$pct_female), nsmall=1, digits=1), "-",
                 format(max(.x$pct_female), nsmall=1, digits=1),"]")
  nhw<-paste0(format(median(.x$pct_nhwhite), nsmall=1, digits=1), "[",
              format(min(.x$pct_nhwhite), nsmall=1, digits=1), "-",
              format(max(.x$pct_nhwhite), nsmall=1, digits=1),"]")
  nhb<-paste0(format(median(.x$pct_black), nsmall=1, digits=1), "[",
              format(min(.x$pct_black), nsmall=1, digits=1), "-",
              format(max(.x$pct_black), nsmall=1, digits=1),"]")
  hispanic<-paste0(format(median(.x$pct_hisp), nsmall=1, digits=1), "[",
                   format(min(.x$pct_hisp), nsmall=1, digits=1), "-",
                   format(max(.x$pct_hisp), nsmall=1, digits=1),"]")
  noncit<- paste0(format(median(.x$pct_noncitizen), nsmall=1, digits=1), "[",
                  format(min(.x$pct_noncitizen), nsmall=1, digits=1), "-",
                  format(max(.x$pct_noncitizen), nsmall=1, digits=1),"]")
  foreignborn<-paste0(format(median(.x$pct_foreignborn), nsmall=1, digits=1), "[",
                      format(min(.x$pct_foreignborn), nsmall=1, digits=1), "-",
                      format(max(.x$pct_foreignborn), nsmall=1, digits=1),"]")
  overcrowded1<-paste0(format(median(.x$pct_overcrowded1), nsmall=1, digits=1), "[",
                       format(min(.x$pct_overcrowded1), nsmall=1, digits=1), "-",
                       format(max(.x$pct_overcrowded1), nsmall=1, digits=1),"]")
  transit<-paste0(format(median(.x$pct_transit), nsmall=1, digits=1), "[",
                  format(min(.x$pct_transit), nsmall=1, digits=1), "-",
                  format(max(.x$pct_transit), nsmall=1, digits=1),"]")
  service<-paste0(format(median(.x$pct_service), nsmall=1, digits=1), "[",
                  format(min(.x$pct_service), nsmall=1, digits=1), "-",
                  format(max(.x$pct_service), nsmall=1, digits=1),"]")
  data.frame(n=n, total_pop=total_pop,
             agebelow18=age0017, age1864=age1864, age65plus=age65plus,
             poverty=poverty, college=college, female=female, nhw=nhw, nhb=nhb, hispanic=hispanic, noncit=noncit, foreignborn=foreignborn, overcrowded1=overcrowded1, transit=transit, service=service)
}) %>% 
  gather(variable, value, -group) %>%  
  spread(group, value) %>% 
  mutate(variable=factor(variable, levels=c("n", "total_pop", "agebelow18", "age1864", "age65plus",
                                            "poverty", "college", "female", "nhw", "nhb", "hispanic", "noncit", "foreignborn","overcrowded1", "transit", "service"))) %>% 
  arrange(variable)
fwrite(table1, file="results/table1.csv")


