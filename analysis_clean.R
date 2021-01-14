###***********************Data Analysis_ indoor dining & preemption**************************

#*Version 1 started Nov 30, 2020 by Alina Schnake-Mahl
#*Descriptive and regression analysis 

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
#import data files
load("daily_count.Rdata")
load("event_model1.Rdata")
load("roll_avg.Rdata")
load("NewCasesTidy.Rdata")
load("roll_avg_death.Rdata")
load("daily_count_deaths.Rdata")

####################################################
#Appendix Figure 1
####################################################
#figures of each city w/ event + cases 
#PHILADELPHIA 
philly<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==42101) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,500)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-07-03"), xend=ymd("2020-07-03"),
           yend=150, y=440, arrow=arrow(), size=2)+
  annotate("segment", x=ymd("2020-09-08"), xend=ymd("2020-09-08"),
           yend=150, y=460, arrow=arrow(), size=2, color="red")+
  annotate("text", label="PA Reopens", x=ymd("2020-07-03"), y=440,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  annotate("text", label="Philly Reopens", x=ymd("2020-09-08"), y=460,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date")  +
  theme_bw()

philly
#Mineapolis

indianapolis<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==18097) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,500)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-05-11"), xend=ymd("2020-05-11"),
           yend=150, y=440, arrow=arrow(), size=2)+
  annotate("segment", x=ymd("2020-06-01"), xend=ymd("2020-06-01"),
           yend=120, y=480, arrow=arrow(), size=2, color="red")+
  annotate("text", label="IN Reopens", x=ymd("2020-05-11"), y=440,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  annotate("text", label="Indianapolis Reopens", x=ymd("2020-06-01"), y=480,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date")  +
  theme_bw()
indianapolis

#PORTLAND
portland<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==41051) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,500)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-05-15"), xend=ymd("2020-05-15"),
           yend=75, y=440, arrow=arrow(), size=2)+
  annotate("segment", x=ymd("2020-06-19"), xend=ymd("2020-06-19"),
           yend=75, y=480, arrow=arrow(), size=2, color="red")+
  annotate("text", label="OR Reopens", x=ymd("2020-05-15"), y=440,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  annotate("text", label="Portland Reopens", x=ymd("2020-06-19"), y=480,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date") +
  theme_bw()
portland
#Las Vegas
lasvegas<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==32003) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,500)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-05-09"), xend=ymd("2020-05-09"),
           yend=130, y=440, arrow=arrow(), size=2)+
  annotate("segment", x=ymd("2020-06-04"), xend=ymd("2020-06-04"),
           yend=150, y=480, arrow=arrow(), size=2, color="red")+
  annotate("text", label="NV reopens", x=ymd("2020-05-09"), y=440,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  annotate("text", label="Las Vegas Reopens", x=ymd("2020-06-04"), y=480,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date") +
  theme_bw()

lasvegas
##### CONTROLS 
###PHEONIX

#may want to limit to 500, to make the same size as others 
phoenix<-
  fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==4013) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,3000)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-05-11"), xend=ymd("2020-05-11"),
           yend=150, y=2000, arrow=arrow(), size=2)+
  annotate("text", label="Arizona & Pheonix Reopens", x=ymd("2020-05-11"), y=2000,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date") +
  theme_bw()
phoenix

#AUSTIN
austin<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==48453) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,600)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-05-01"), xend=ymd("2020-05-01"),
           yend=150, y=480, arrow=arrow(), size=2)+
  annotate("text", label="Austin & TX Reopen", x=ymd("2020-05-01"), y=480,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date") +
  theme_bw()
austin
#DALLAS
dallas<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==48113) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,1750)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-05-01"), xend=ymd("2020-05-01"),
           yend=200, y=1500, arrow=arrow(), size=2)+
  annotate("text", label="Dallas & TX Reopen", x=ymd("2020-05-01"), y=1500,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date") +
  theme_bw()
dallas
#San Antonio
sanantonio<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==48029) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,1750)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-05-01"), xend=ymd("2020-05-01"),
           yend=150, y=1500, arrow=arrow(), size=2)+
  annotate("text", label="San Antonio & TX Reopen", x=ymd("2020-05-01"), y=1500,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date") +
  theme_bw()
sanantonio
#Houston
houston<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==48201) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,1750)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-05-01"), xend=ymd("2020-05-01"),
           yend=200, y=1500, arrow=arrow(), size=2)+
  annotate("text", label="Houston & TX Reopen", x=ymd("2020-05-01"), y=1500,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date") +
  theme_bw()
houston
#ATLANTA
atlanta<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==13121) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,500)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-04-27"), xend=ymd("2020-04-27"),
           yend=110, y=400, arrow=arrow(), size=2)+
  annotate("segment", x=ymd("2020-06-16"), xend=ymd("2020-06-16"),
           yend=110, y=450, arrow=arrow(), size=2, color="blue")+
  annotate("text", label="Atlanta & Georgia Reopen", x=ymd("2020-04-27"), y=400,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  annotate("text", label="All dining restrictions removed", x=ymd("2020-6-16"), y=450,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date") +
  theme_bw()
atlanta
#CHARLESTON
charleston<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(fips==45019) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
  mutate(cases2=cases2-lag(cases2)) %>% 
  ggplot(aes(x=date, y=cases2)) + 
  geom_line()+
  ylim(0,500)+
  scale_x_date(breaks="1 month", date_labels = "%b") +
  annotate("segment", x=ymd("2020-04-27"), xend=ymd("2020-05-11"),
           yend=50, y=400, arrow=arrow(), size=2)+
  annotate("text", label="Charleston and SC Reopen", x=ymd("2020-05-11"), y=400,
           color="black", size=4,hjust=0.5, vjust=-0.1)+
  labs(title = "Rolling 7-day averages of new COVID cases", 
       y = "New Cases",
       x = "Date") +
  theme_bw()
charleston

#print all to PDF
multi.page <- ggarrange(philly,
                        indianapolis, 
                        portland, 
                        lasvegas, 
                        sanantonio, 
                        dallas,
                        houston, 
                        austin,
                        phoenix,
                        atlanta,
                        charleston,
                        nrow = 2, ncol = 2)
multi.page[[1]]
ggexport(multi.page, filename = "figure_1.pdf")

####################################################
#Figure 1:Parallel trends assumption 
####################################################

#find means by treat and week
means <- roll_avg %>% 
  group_by(treat1, time) %>% 
  summarise(casemean = mean(daily_count), 
            mean7da = mean(case_07da), 
            ratemean=mean(caserate_07da),
            logcasemean=log(casemean))
means

rate_mean1 <- ggplot(roll_avg, aes(x = time, y = caserate_07da, group =treat1)) +
  geom_line(data=means, aes(x=time, y=ratemean, color=factor(treat1, labels = c("Control", "Treatment"))))+
  geom_vline(xintercept = 14)+
  labs(title = " Rolling 7 day average rate new COVID cases", 
       color="Treat v Control",
       y = "New case rate per 100,000",
       x = "Days Since Delayed Reopening ") +
  stat_smooth(method="loess", se=FALSE, color="black" , aes=(color=as.factor(cities))) +
  coord_cartesian(ylim = c(0, 15))

rate_mean1

#final figure for deaths 
means_death <- roll_avg_death %>% 
  group_by(treat1, time) %>% 
  summarise(deathmean = mean(daily_deaths), 
            mean7da = mean(deaths_07da), 
            ratemean=mean(deathrate_07da))
means_death

#####NEED TO FIX THIS..... 
#Likely something going on here-- or else really not parallel trends 
deaths_mean1 <- ggplot(roll_avg_death, aes(x = time, y = deathrate_07da, group =treat1)) +
  geom_line(data=means_death, aes(x=time, y=ratemean, color=factor(treat1, labels = c("Control", "Treatment"))))+
  geom_vline(xintercept = 21)+
  labs(title = " Rolling 7 day average rate new COVID cases", 
       color="Treat v Control",
       y = "New case rate per 100,000",
       x = "Days Since Delayed Reopening ") +
  stat_smooth(method="loess", se=FALSE, color="black" , aes=(color=as.factor(cities))) 

deaths_mean1


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
rse_mod_nb1<-exp(coeftest(mod_nb1, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb1<-exp(coefci(mod_nb1, vcov = vcovHC,  cluster= ~cities))

#add offset (could also consider test offset)
##we add the per 100000 to make a per 100,000 rate offset
summary(mod_nb1off<-glm.nb(daily_count~pre_post + offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb1off, apply.coef = exp, type='text')
AIC(mod_nb1off)
BIC(mod_nb1off)
##add heteroskedasticity robust standard errors and cluster at city 
#controls for mild violation of distribution assumption that var=mean
#estimate robust standard errors (can sub in type="HC0" to change from default HC1)
#repeated for each model 
rse_mod_nb1off<-exp(coeftest(mod_nb1off, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb1off<-exp(coefci(mod_nb1off, vcov = vcovHC,  cluster= ~cities))
#offset better fit for model- include from here on. 

#add treat 
summary(mod_nb2off<-glm.nb(daily_count~pre_post +treat1+ offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb2off, apply.coef = exp, type='text')
rse_nb2off<-exp(coeftest(mod_nb2off, vcov = vcovHC,  cluster= ~cities))
rci_nb2off<-exp(coefci(mod_nb2off, vcov = vcovHC,  cluster= ~cities))
#add interaction 
summary(mod_nb3<-glm.nb(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb3, apply.coef = exp, type='text')
rse_mod_nb3<-exp(coeftest(mod_nb3, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3<-exp(coefci(mod_nb3, vcov = vcovHC,  cluster= ~cities))

########add other policies########
#stay at home order
summary(mod_nb3a<-glm.nb(daily_count~treat1*pre_post + at_home + offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb3a, apply.coef = exp, type='text')
anova(mod_nb3, mod_nb3a,  test="Chisq")
rse_mod_nb3a<-exp(coeftest(mod_nb3a, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3a<-exp(coefci(mod_nb3a, vcov = vcovHC,  cluster= ~cities))

summary(county_cases2$date)
#mask mandate
summary(mod_nb3b<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + offset(log(pop/100000)), data=county_cases2))
stargazer(mod_nb3b, apply.coef = exp, type='text')
anova(mod_nb3a, mod_nb3b,  test="Chisq")
rse_mod_nb3b<-exp(coeftest(mod_nb3b, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3b<-exp(coefci(mod_nb3b, vcov = vcovHC,  cluster= ~cities))
#eviction ban 
summary(mod_nb3c<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_end + offset(log(pop/100000)),  data=county_cases2))
stargazer(mod_nb3c, apply.coef = exp, type='text')
anova(mod_nb3b, mod_nb3c,  test="Chisq")
rse_mod_nb3c<-exp(coeftest(mod_nb3c, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3c<-exp(coefci(mod_nb3c, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3c

county_cases2_LV<-county_cases2%>%
  filter(cities!="Las Vegas")

#compare models 
models <- list(mod_nb1, mod_nb1off, mod_nb2off, mod_nb3)
models1<-list(mod_nb3a, mod_nb3b, mod_nb3c)

robust_se_1<-list(rse_mod_nb1, rse_mod_nb1off, rse_mod_nb2off, rse_mod_nb3)
robust_se_2<-list(rse_mod_nb3a, rse_mod_nb3b, rse_mod_nb3c)
robust_ci_1<-list(rci_mod_nb1, rci_mod_nb1off, rci_mod_nb2off, rci_mod_nb3)
robust_ci_2<-list(rci_mod_nb3a, rci_mod_nb3b, rci_mod_nb3c)

stargazer(models, apply.coef=exp, type = "text", ci = TRUE, title="Base Models", out="results/table1.txt")
stargazer(models1, apply.coef=exp, type="text", ci = TRUE, title="Models w/ NPIs", out="results/table1a.txt")

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
#Sensivity 1:no lag for other NPIs
#only testing the interaction and full model 
summary(mod_s2a1<-glm.nb(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2a))
rse_mod_s2a1<-exp(coeftest(mod_s2a1, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2a1<-exp(coefci(mod_s2a1, vcov = vcovHC,  cluster= ~cities))

summary(mod_s2a2<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2a))
rse_mod_s2a2<-exp(coeftest(mod_s2a2, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2a2<-exp(coefci(mod_s2a2, vcov = vcovHC,  cluster= ~cities))

#**********************************
###Sensivity 2: 9 day lag
county_cases2b<-county_cases2b%>%
  filter(cities!="Las Vegas")
summary(mod_s2b1<-glm.nb(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2b))
rse_mod_s2b1<-exp(coeftest(mod_s2b1, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2b1<-exp(coefci(mod_s2b1, vcov = vcovHC,  cluster= ~cities))

summary(mod_s2b2<-glm.nb(daily_count~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_cases2b))
rse_mod_s2b2<-exp(coeftest(mod_s2b2, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2b2<-exp(coefci(mod_s2b2, vcov = vcovHC,  cluster= ~cities))
exp(-0.88385)

#**********************************
###Sensitivity 3: increase lag to 3 weeks (21 days)
summary(mod_s2c1<-glm.nb(daily_deaths~treat1*pre_post + offset(log(pop/100000)), data=county_cases2c))
rse_mod_s2c1<-exp(coeftest(mod_s1c1, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2c1<-exp(coefci(mod_s1c1, vcov = vcovHC,  cluster= ~cities))

summary(mod_s2c2<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2c))
rse_mod_s2c2<-exp(coeftest(mod_s1c2, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2c2<-exp(coefci(mod_s1c2, vcov = vcovHC,  cluster= ~cities))

###Sensivity 4: increase lag to 4 weeks (28 days)
summary(mod_s2d1<-glm.nb(daily_deaths~treat1*pre_post + offset(log(pop/100000)), data=county_cases2d))
rse_mod_s2d1<-exp(coeftest(mod_s1d1, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2d1<-exp(coefci(mod_s1d1, vcov = vcovHC,  cluster= ~cities))

summary(mod_s2d2<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2d))
rse_mod_s2d2<-exp(coeftest(mod_s1d2, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2d2<-exp(coefci(mod_s1d2, vcov = vcovHC,  cluster= ~cities))

#sensitivity 5
#Increase study period to 12 weeks 

summary(mod_s2d1<-glm.nb(daily_deaths~treat1*pre_post + offset(log(pop/100000)), data=county_cases2e))
rse_mod_s2e1<-exp(coeftest(mod_s1e1, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2e1<-exp(coefci(mod_s1e1, vcov = vcovHC,  cluster= ~cities))

summary(mod_s2e2<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2e))
rse_mod_s2e2<-exp(coeftest(mod_s1e2, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2e2<-exp(coefci(mod_s1e2, vcov = vcovHC,  cluster= ~cities))

#Sensitivity 6: limit to BCHC cities 
summary(mod_s2f1<-glm.nb(daily_deaths~treat1*pre_post + offset(log(pop/100000)), data=county_cases2f))
rse_mod_s2f1<-exp(coeftest(mod_s1f1, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2f1<-exp(coefci(mod_s1f1, vcov = vcovHC,  cluster= ~cities))

summary(mod_s2f2<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2f))
rse_mod_s2f2<-exp(coeftest(mod_s1f2, vcov = vcovHC,  cluster= ~cities))
rci_mod_s2f2<-exp(coefci(mod_s1f2, vcov = vcovHC,  cluster= ~cities))


##model output model outpumodel output model output model output model output t 
#changes in lag periods  
models_sen <- list(mod_s2a1, mod_s2a2, mod_s2b1,mod_s2b2, mod_s2c1,mod_s2c2, mod_s2d1,mod_s2d2)
robust_se_2<-list(rse_mod_s2a1, rse_mod_s2a2, rse_mod_s2b1, rse_mod_s2b2, rse_mod_s2c1, rse_mod_s2c2, rse_mod_s2d1, rse_mod_s2d2)
robust_ci_2<-list(rci_mod_s2a1, rci_mod_s2a2, rci_mod_s2b1, rci_mod_s2b2, rci_mod_s2c1, rci_mod_s2c2, rci_mod_s2d1, rci_mod_s2d2)

stargazer(models, apply.coef=exp, type = "text", ci = TRUE, title="Sensitivity_change in lags", out="results/table_1sa_cases.txt")
#model output additional sensitivity analysis 
## BCHC &  
models_sen2 <- list(mod_s2e1, mod_s2e2, mod_s2f1,mod_s2f2)
robust_se_2b<-list(rse_mod_s2e1, rse_mod_s2e2, rse_mod_s2f1, rse_mod_s2f2)
robust_ci_2b<-list(rci_mod_s2e1, rci_mod_s2e2, rci_mod_s2f1, rci_mod_s2f2)

stargazer(models, apply.coef=exp, type = "text", ci = TRUE, title="Sensitivity_period, BCHC", out="results/table_1sb_cases.txt")

####################################################
# Models for Deaths 
####################################################

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
rci_mod_nb3<-exp(coefci(mod_nb3, vcov = vcovHC,  cluster= ~cities))

########add other policies########
#stay at home order
summary(mod_nb3a<-glm.nb(daily_deaths~treat1*pre_post + at_home + offset(log(pop/100000)), data=county_deaths2))
stargazer(mod_nb3a, apply.coef = exp, type='text')
anova(mod_nb3, mod_nb3a,  test="Chisq")
rse_mod_nb3a<-exp(coeftest(mod_nb3a, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3a<-exp(coefci(mod_nb3a, vcov = vcovHC,  cluster= ~cities))

summary(county_cases2$date)
#mask mandate
summary(mod_nb3b<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + offset(log(pop/100000)), data=county_deaths2))
stargazer(mod_nb3b, apply.coef = exp, type='text')
anova(mod_nb3a, mod_nb3b,  test="Chisq")
rse_mod_nb3b<-exp(coeftest(mod_nb3b, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3b<-exp(coefci(mod_nb3b, vcov = vcovHC,  cluster= ~cities))

#eviction ban 
summary(mod_nb3c<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2))
stargazer(mod_nb3c, apply.coef = exp, type='text')
anova(mod_nb3b, mod_nb3c,  test="Chisq")
rse_mod_nb3c<-exp(coeftest(mod_nb3c, vcov = vcovHC,  cluster= ~cities))
rci_mod_nb3c<-exp(coefci(mod_nb3c, vcov = vcovHC,  cluster= ~cities))

#compare models 
modelsdeath <- list(mod_nb1, mod_nb1off, mod_nb2off, mod_nb3)
modelsdeath1<-list(mod_nb3a, mod_nb3b, mod_nb3c)

robust_se_death<-list(rse_mod_nb1, rse_mod_nb1off, rse_mod_nb2off, rse_mod_nb3)
robust_ci_death<-list(rci_mod_nb1, rci_mod_nb1off, rci_mod_nb2off, rci_mod_nb3)
robust_se_death<-list(rse_mod_nb3a, rse_mod_nb3b, rse_mod_nb3c)
robust_ci_death<-list(rci_mod_nb3a, rci_mod_nb3b, rci_mod_nb3c)

stargazer(modelsdeath, apply.coef=exp, type = "text", title="Base Models", out="results/table1deaths.txt")
stargazer(modelsdeath1, apply.coef=exp, type="text", title="Models w/ NPIs", out="results/table1adeaths.txt")

#################################################################################
#Sensitivity Analysis
#repeat above models for different treatment period 

#sensitivity 1:create pre post with 42 days after
# 14 days for cases, + 28 for death lag  
summary(mod_ds1b<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2b))
rse_mod_ds1b<-exp(coeftest(mod_ds1b, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1b<-exp(coefci(mod_ds1b, vcov = vcovHC,  cluster= ~cities))

#sensitivity analysis 2: 
#remove lag for other NPIs
summary(mod_ds1c<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2c))
rse_mod_ds1c<-exp(coeftest(mod_ds1c, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1c<-exp(coefci(mod_ds1c, vcov = vcovHC,  cluster= ~cities))

# sensitivity analysis 3
#only 7 day case lag + 21 day death 
summary(mod_ds1d<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2d))
rse_mod_ds1d<-exp(coeftest(mod_ds1d, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1d<-exp(coefci(mod_ds1d, vcov = vcovHC,  cluster= ~cities))

#Sensitivity 4
#remove the non bchc cities 
summary(mod_ds1e<-glm.nb(daily_deaths~treat1*pre_post + at_home + mask_mandate + evict_ban + offset(log(pop/100000)),  data=county_deaths2e))
rse_mod_ds1e<-exp(coeftest(mod_ds1e, vcov = vcovHC,  cluster= ~cities))
rci_mod_ds1e<-exp(coefci(mod_ds1e, vcov = vcovHC,  cluster= ~cities))


model_deaths_s1<-list(mod_ds1b, mod_ds1c, mod_ds1d, mod_ds1e)
stargazer(model_deaths_s1, apply.coef=exp, type="text", title="sensitivity death 1", out="results/table1a_deathsS4.txt")

robust_se_death<-list(rse_mod_ds1b, rse_mod_ds1c, rse_mod_ds1d, rse_mod_ds1e)
robust_ci_death<-list(rci_mod_ds1b, rci_mod_ds1c, rci_mod_ds1d, rci_mod_ds1e)

#######################################################
#Event Model 
#######################################################
#base model 
#just weeks

summary(mod_e1<-glm.nb(daily_count~weeks_prior+ weeks_post + offset(log(pop/100000)),  data=event_model1))
stargazer(mod_e1, mod_e1p, apply.coef = exp, type='text')

#adding city fixed effects (as dummies)
summary(mod_e2<-glm.nb(daily_count~weeks_prior +weeks_post + factor(cities) + offset(log(pop/100000)),  data=event_model1))

#adding calendar week fixed effects (as dummies)
summary(mod_e3<-glm.nb(daily_count~weeks_prior +weeks_post + factor(cal_week) + factor(cities) + offset(log(pop/100000)),  data=event_model1))

#adding mask mandate
summary(mod_e4a<-glm.nb(daily_count~weeks_prior +weeks_post+ factor(cal_week) + factor(cities) +mask_week + offset(log(pop/100000)),  data=event_model1))

#adding stay at home order 
summary(mod_e4b<-glm.nb(daily_count~weeks_prior + weeks_post + factor(cal_week) + factor(cities) +mask_week + stay_week+ offset(log(pop/100000)),  data=event_model1))

#adding eviction ban
summary(mod_e4c<-glm.nb(daily_count~ weeks_prior + weeks_post + factor(cal_week) + factor(cities) +mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model1))

event_models<-list(mod_e1, mod_e2, mod_e3, mod_e4a, mod_e4b, mod_e4c)
stargazer(event_models, apply.coef=exp, type="text", title="IRRs event model", out="results/table2_event.txt")

#cluster standard errors -pick type
coeftest(mod_e1, vcov = vcovHC(mod_e1, type="HC1", cluster="group"))
coeftest(mod_e1c, vcov = vcovHC(mod_e1c, type="HC1", cluster="group"))


###SENSITIVITY 1 
#repeat model using 4+ for other NPIs
summary(mod_e1s<-glm.nb(daily_count~weeks_prior+ weeks_post + offset(log(pop/100000)),  data=event_model2))
stargazer(mod_e1s, mod_e1p, apply.coef = exp, type='text')

#adding city fixed effects (as dummies)
summary(mod_e2s<-glm.nb(daily_count~weeks_prior +weeks_post + factor(cities) + offset(log(pop/100000)),  data=event_model2))

#adding calendar week fixed effects 
summary(mod_e3s<-glm.nb(daily_count~weeks_prior +weeks_post + factor(cal_week) + factor(cities) + offset(log(pop/100000)),  data=event_model2))

#adding mask mandate
summary(mod_e4as<-glm.nb(daily_count~weeks_prior +weeks_post+ factor(cal_week) + factor(cities) +mask_week + offset(log(pop/100000)),  data=event_model2))

#adding stay at home order 
summary(mod_e4bs<-glm.nb(daily_count~weeks_prior + weeks_post + factor(cal_week) + factor(cities) +mask_week + stay_week+ offset(log(pop/100000)),  data=event_model2))

#adding eviction ban
summary(mod_e4cs<-glm.nb(daily_count~ weeks_prior + weeks_post + factor(cal_week) + factor(cities) +mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model2))

event_models<-list(mod_e1s, mod_e2s, mod_e3s, mod_e4as, mod_e4bs, mod_e4cs)
stargazer(event_models, apply.coef=exp, type="text", title="IRRs event model_4+", out="results/table2_event_s1.txt")

##Standard errors for panel data 
vcovPL(mod_e1s, cluster = "cities", order.by = NULL, kernel = "Bartlett",
       sandwich = TRUE, fix = FALSE)


###############################################################################
#Event Models-Deaths
###############################################################################

summary(mod_ed1<-glm.nb(daily_deaths~weeks_prior+ weeks_post + offset(log(pop/100000)),  data=event_model_death1))
stargazer(mod_ed1, mod_e1p, apply.coef = exp, type='text')

#adding city fixed effects (as dummies)
summary(mod_ed2<-glm.nb(daily_deaths~weeks_prior +weeks_post + factor(cities) + offset(log(pop/100000)),  data=event_model_death1))

#adding calendar week fixed effects 
summary(mod_ed3<-glm.nb(daily_deaths~weeks_prior +weeks_post + factor(cal_week) + factor(cities) + offset(log(pop/100000)),  data=event_model_death1))

#adding mask mandate
summary(mod_ed4a<-glm.nb(daily_deaths~weeks_prior +weeks_post+ factor(cal_week) + factor(cities) +mask_week + offset(log(pop/100000)),  data=event_model_death1))

#adding stay at home order 
summary(mod_ed4b<-glm.nb(daily_deaths~weeks_prior + weeks_post + factor(cal_week) + factor(cities) +mask_week + stay_week+ offset(log(pop/100000)),  data=event_model_death1))

#adding eviction ban
summary(mod_ed4c<-glm.nb(daily_deaths~ weeks_prior + weeks_post + factor(cal_week) + factor(cities) +mask_week + stay_week+ evict_week + (log(pop/100000)),  data=event_model_death1))

event_models_d<-list(mod_ed1, mod_ed2, mod_ed3, mod_ed4a, mod_ed4b, mod_ed4c)
stargazer(event_models_d, apply.coef=exp, type="text", title="IRRs event model death_4+", out="results/table2_event_death.txt")

#check this it looks to similar to the case model.... 


#estimate marginal effects 
#can't use margins bc of the offset. 
negbinmfx(formula, data, atmean = TRUE, robust = FALSE, clustervar1 = NULL, 
          clustervar2 = NULL, start = NULL, control = glm.control())


#descriptive data
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
  summarize(mean=mean(case_rate))


