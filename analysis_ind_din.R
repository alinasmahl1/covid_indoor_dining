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

#Plot cases over time by city 
t<-county_cases2%>%
  filter(treat1==1)%>%
  ggplot(aes(x=time, y=case_rate, group=cities)) +
  geom_line(aes(color=cities))
t

c<-county_cases2%>%
  filter(treat1==0)%>%
  ggplot(aes(x=time, y=case_rate, group=cities)) +
  geom_line(aes(color=cities))
c

#visualize moving averages 
p<- ggplot(data=county_cases2, aes(x = time, y = daily_count)) +
  geom_col(alpha = 3/10, linetype = 0)  + 
  geom_line(data = NewCasesTidy, 
            mapping = aes(x = time, 
                          y = new_conf_av_value, 
                          color = new_conf_av_key))+   
  facet_wrap(~cities)+
  labs(title = "Rolling 3 and 7-day averages of new COVID cases", 
       y = "New Cases",
       color = "Metric:", 
       x = "Time") + 
  hrbrthemes::theme_ipsum_tw() + 
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank() + 
          theme(legend.position = "top")
        p
        
        save(p, file="figure1.pdf")
        
        # 7 day ave case_07day averages for all, on one graph 
        r<- ggplot() +
          geom_line(data=roll_avg, aes(x=time, y= case_07da, colour=cities)) +
          labs(title = "Rolling 7-day averages of new COVID cases", 
               y = "New Cases",
               color = "cities", 
               x = "Time")  +
          facet_wrap(~treat1) 
        hrbrthemes::theme_ipsum_tw() + 
          theme(axis.title.x = element_blank(),
                axis.ticks.x = element_blank() + 
                  ggplot2::theme(legend.position = "top"))
        r
        save(r, file="figure_1a.pdf")        
        
        #repeat but indexed at day 1. 
        
        r<- ggplot()+
          geom_line(data=roll_avg, aes(x=time, y= case_07da, colour=cities)) +
          labs(title = "Rolling 7-day averages of new COVID cases", 
               y = "New Cases",
               color = "cities", 
               x = "Time")  
        hrbrthemes::theme_ipsum_tw() + 
          theme(axis.title.x = element_blank(),
                axis.ticks.x = element_blank() + 
                  ggplot2::theme(legend.position = "top"))
        r
        save(r, file="figure_1a.pdf")        
        
        #repeat figure but index at time zero 
        
        case_timezero<-roll_avg%>%
          group_by(cities)%>%
          filter(time==0)%>%
          select(cities, case_07da)%>%
          rename(case_zero=case_07da)
        
        index_at_zero<-roll_avg%>%
          left_join(case_timezero)%>%
          mutate(ratio=case_07da/case_zero)
        
        #plot this over time
        g<- ggplot(data=index_at_zero, aes(x=time, y= ratio, colour=cities))+
          geom_line() +
          labs(title = "Ratio of daily cases to cases at time 0", 
               y = "new cases/cases at time zero",
               color = "cities", 
               x = "Time")  +
          hrbrthemes::theme_ipsum_tw() + 
          theme(axis.title.x = element_blank(),
                axis.ticks.x = element_blank() + 
                  ggplot2::theme(legend.position = "top"))
        g
        g+ geom_smooth(method="loess")
        g + scale_y_continuous(trans='log') 
        g + scale_y_continuous(trans='log') +geom_smooth(method="loess")
        g + scale_y_continuous(trans='log') +geom_smooth(method="loess")+
          facet_wrap(~treat1)
        
        m<-ggplot(data=roll_avg, aes(x=date, y=daily_count)) +
          geom_line()
        m
        
        
        ### figure 1 # plotting cases by day, w/ arrows for state vs city reopening
        
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
        
        #San Francisco
        SanFrancisco<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
          filter(fips==6075) %>% 
          mutate(date=ymd(date)) %>% 
          mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
          mutate(cases2=cases2-lag(cases2)) %>% 
          ggplot(aes(x=date, y=cases2)) + 
          geom_line()+
          ylim(0,500)+
          scale_x_date(breaks="1 month", date_labels = "%b") +
          annotate("segment", x=ymd("2020-08-31"), xend=ymd("2020-08-31"),
                   yend=75, y=440, arrow=arrow(), size=2)+
          annotate("segment", x=ymd("2020-09-30"), xend=ymd("2020-09-30"),
                   yend=75, y=480, arrow=arrow(), size=2, color="red")+
          annotate("text", label="SF allowed to Reopen", x=ymd("2020-08-30"), y=440,
                   color="black", size=4,hjust=0.5, vjust=-0.1)+
          annotate("text", label="SF Reopens", x=ymd("2020-09-30"), y=480,
                   color="black", size=4,hjust=0.5, vjust=-0.1)+
          labs(title = "Rolling 7-day averages of new COVID cases", 
               y = "New Cases",
               x = "Date") +
          theme_bw()
SanFrancisco       

#Milwaukee
        Milwaukee<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
          filter(fips==32003) %>% 
          mutate(date=ymd(date)) %>% 
          mutate(cases2=rollmean(cases, k=7, align="center",na.pad=T)) %>% 
          mutate(cases2=cases2-lag(cases2)) %>% 
          ggplot(aes(x=date, y=cases2)) + 
          geom_line()+
          ylim(0,500)+
          scale_x_date(breaks="1 month", date_labels = "%b") +
          annotate("segment", x=ymd("2020-05-14"), xend=ymd("2020-05-14"),
                   yend=130, y=440, arrow=arrow(), size=2)+
          annotate("segment", x=ymd("2020-06-05"), xend=ymd("2020-06-05"),
                   yend=150, y=480, arrow=arrow(), size=2, color="red")+
          annotate("text", label="NV reopens", x=ymd("2020-05-14"), y=440,
                   color="black", size=4,hjust=0.5, vjust=-0.1)+
          annotate("text", label="Las Vegas Reopens", x=ymd("2020-06-05"), y=480,
                   color="black", size=4,hjust=0.5, vjust=-0.1)+
          labs(title = "Rolling 7-day averages of new COVID cases", 
               y = "New Cases",
               x = "Date") +
          theme_bw()
        
        Milwaukee
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
        
        ######################parallel trends assumption ##################
        #can try to do parallel trends assumption, adjusted confounders
        
        #find means by treat and week
        means <- roll_avg %>% 
          group_by(treat1, time) %>% 
          summarise(casemean = mean(daily_count), 
                    mean7da = mean(case_07da), 
                    ratemean=mean(caserate_07da),
                    logcasemean=log(casemean))
        means
        
        #filter to only pre period
        roll_avg1<-roll_avg %>%
          filter(time<14)
        
        means1<-roll_avg1 %>% 
          group_by(treat1, time) %>% 
          summarise(casemean = mean(daily_count), 
                    mean7da = mean(case_07da), 
                    ratemean=mean(caserate_07da), 
                    log_mean7da=log(mean7da), 
                    log_meanrate=log(ratemean))
        means1
        
        ####plot rolling average over time, daily cases, rate 
        ########daily case
        ##full period (pre & post)
        
        daily<- ggplot() +
          geom_line(data=roll_avg, aes(x=time, y= daily_count, colour=cities)) +
          geom_line(data=means, aes(x=time, y=casemean))+
          labs(title = " daily case rate new COVID cases", 
               y = "New Cases",
               color = "cities", 
               x = "Time")  +
          #add vertial line at 14 days post policy date 
          geom_vline(xintercept = 14) +
          facet_wrap(~treat1) 
        daily
        
        ##Only pre period --plot rolling average over time 
        daily_pre<- ggplot() +
          geom_line(data=roll_avg1, aes(x=time, y= daily_count, colour=cities)) +
          geom_line(data=means1, aes(x=time, y=casemean))+
          labs(title = " daily new COVID cases", 
               y = "New Cases",
               color = "cities", 
               x = "Time")  +
          facet_wrap(~treat1) 
        daily_pre
        ######## 7 day average
        
        avg7da<- ggplot() +
          geom_line(data=roll_avg, aes(x=time, y=case_07da , colour=cities)) +
          geom_line(data=means, aes(x=time, y=mean7da))+
          labs(title = "  rolling 7 day average case rate new COVID cases", 
               y = "New Cases",
               color = "cities", 
               x = "Time")  +
          #add vertial line at 14 days post policy date 
          geom_vline(xintercept = 14) +
          facet_wrap(~treat1) 
        avg7da
        
        ##Only pre period --plot rolling average over time 
        avg7da_pre<- ggplot() +
          geom_line(data=roll_avg1, aes(x=time, y=case_07da, colour=cities)) +
          geom_line(data=means1, aes(x=time, y=mean7da))+
          labs(title = "log rolling 7 day average case rate new COVID cases", 
               y = "New Cases",
               color = "cities", 
               x = "Time")  +
          facet_wrap(~treat1) 
        avg7da_pre
        
        ######Rate 7 day average 
        rate<- ggplot() +
          geom_line(data=roll_avg, aes(x=time, y=caserate_07da, colour=cities)) +
          geom_line(data=means, aes(x=time, y=ratemean))+
          labs(title = " rolling 7 day average case rate new COVID cases", 
               y = "New Cases",
               color = "cities", 
               x = "Time")  +
          #add vertial line at 14 days post policy date 
          geom_vline(xintercept = 14) +
          facet_wrap(~treat1) 
        rate
        
        #Only pre period --plot rolling average over time 
        rate_pre<- ggplot() +
          geom_line(data=roll_avg1, aes(x=time, y=caserate_07da, colour=cities)) +
          geom_line(data=means1, aes(x=time, y=ratemean))+
          labs(title = " rolling 7 day average case rate new COVID cases", 
               y = "New Cases",
               color = "cities", 
               x = "Time")  +
          facet_wrap(~treat1) 
        
        rate_pre
        
        ####repeat above but just means, on one graph 
        
        #7 day average Rate (THIS IS OUR FINAL FIGURE)
        
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
        
        #Likely something going on here-- or else really not parallel trends 
        deaths_mean1 <- ggplot(roll_avg_death, aes(x = time, y = deathrate_07da, group =treat1)) +
          geom_line(data=means_death, aes(x=time, y=ratemean, color=factor(treat1, labels = c("Control", "Treatment"))))+
          geom_vline(xintercept = 14)+
          labs(title = " Rolling 7 day average rate new COVID cases", 
               color="Treat v Control",
               y = "New case rate per 100,000",
               x = "Days Since Delayed Reopening ") +
          stat_smooth(method="loess", se=FALSE, color="black" , aes=(color=as.factor(cities))) 
        
        deaths_mean1
        
        #plot deaths by 7 day avg 
        
        avg7da_death<- ggplot() +
          geom_line(data=roll_avg_death, aes(x=time, y=deaths_07da , colour=cities)) +
          geom_line(data=means_death, aes(x=time, y=mean7da))+
          labs(title = "  Rolling 7 day average deaths", 
               y = "Daily Deaths",
               color = "cities", 
               x = "Time")  +
          #add vertial line at 14 days post policy date 
          geom_vline(xintercept = 34) +
          facet_wrap(~treat1) 
        avg7da_death
        
        save(avg7da_death, file="Death_par_trend.pdf")
        #repeat for pre period
        rate_meanpre<- ggplot(roll_avg1, aes(x = time, y = caserate_07da, group =treat1)) +
          geom_line(data=means1, aes(x=time, y=ratemean, color=factor(treat1, labels = c("Control", "Treatment"))))+
          labs(title = " Rolling 7 day average rate new COVID cases", 
               color="Treat v Control",
               y = "New case rate per 100,000",
               x = "Days Since Delayed Reopening ") +
          stat_smooth(method="loess", se=FALSE, color="black" , aes=(color=as.factor(cities))) +
          coord_cartesian(ylim = c(0, 15))
        rate_meanpre
        
        #graphing by week (event study)
        
        means2<-event_model2m%>%
          group_by(treat1, weeks)%>%
          summarize(ratemean=mean(weekly_rate), 
                    casemean=mean(weekly_newcases))%>%
          ungroup()
        means2
        
    
        e1<-ggplot(event_model2m, aes(x = weeks, y = weekly_rate, group =treat1)) +
          geom_line(data=means2, aes(x=weeks, y=ratemean, color=factor(treat1, labels = c("Control", "Treatment"))))+
          labs(title = " Rolling 7 day average rate new COVID cases", 
               color="Treat v Control",
               y = "New case rate per 100,000",
               x = "Days Since Delayed Reopening ") +
          stat_smooth(method="loess", se=FALSE, color="black" , aes=(color=as.factor(cities))) 
        
        e1
        
        means2d<-event_model_death2m%>%
          group_by(treat1, weeks)%>%
          summarize(ratemean=mean(weekly_rate), 
                    deathmean=mean(weekly_newdeaths))%>%
          ungroup()
        means2d
        
        e1<-ggplot(event_model_death2m, aes(x = weeks, y = weekly_rate, group =treat1)) +
          geom_line(data=means2d, aes(x=weeks, y=ratemean, color=factor(treat1, labels = c("Control", "Treatment"))))+
          labs(title = " Rolling 7 day average rate new COVID cases", 
               color="Treat v Control",
               y = "New case rate per 100,000",
               x = "Days Since Delayed Reopening ") +
          stat_smooth(method="loess", se=FALSE, color="black" , aes=(color=as.factor(cities))) 
        
        e1
        
        rate_mean1 <- ggplot(roll_avg, aes(x = time, y = caserate_07da, group =treat1)) +
          geom_line(data=means, aes(x=time, y=ratemean, color=factor(treat1, labels = c("Control", "Treatment"))))+
          geom_vline(xintercept = 14)+
          labs(title = " Rolling 7 day average rate new COVID cases", 
               color="Treat v Control",
               y = "New case rate per 100,000",
               x = "Days Since Delayed Reopening ") +
          stat_smooth(method="loess", se=FALSE, color="black" , aes=(color=as.factor(cities))) +
          coord_cartesian(ylim = c(0, 15))
        
        
        e2<-
          ggplot(data=means2, aes(x=weeks, y=ratemean, group=treat1, color=treat1)) +
          geom_line()+
          #x=13 is equivalent to 0 
          geom_vline(xintercept = 13) +
          labs(title = "Weekly Average rate", 
               y = "average per week",
               color = "Treament and Control (treat=1)", 
               x = "weeks since indoor dining stayed closed")  
        e2 
        
        
        
        #repeated for logs 
        e3<-
          ggplot(data=means2, aes(x=weeks, y=log(casemean), group=treat1, color=treat1)) +
          geom_line()+
          #x=13 is equivalent to 0 
          geom_vline(xintercept = 13) +
          labs(title = "Weekly log Average cases", 
               y = "log average per week",
               color = "Treament and Control", 
               x = "weeks since indoor dining stayed closed")  
        e3 
        
        e4<-
          ggplot(data=means2, aes(x=weeks, y=log(ratemean), group=treat1, color=treat1)) +
          geom_line()+
          #x=13 is equivalent to 0 
          geom_vline(xintercept = 13) +
          labs(title = "Weekly log Average rate", 
               y = "log average per week",
               color = "Treament and Control (treat=1)", 
               x = "weeks since indoor dining stayed closed")  
        e4 
        
        #save figures to pdf
        multi.page <- ggarrange(r,  r1, 
                                d, d1,
                                c, c1, 
                                l, l1, 
                                lb, lb1,
                                nrow = 2, ncol = 2)
        multi.page[[1]]
        ggexport(multi.page, filename = "parallel_trend1.pdf")
        
        multi.page <- ggarrange(m1, m1a,
                                m2, m2a,
                                m3, m3a,
                                m4, m4a,
                                nrow = 2, ncol = 2)
        multi.page[[1]]
        ggexport(multi.page, filename = "parallel_trend2_meanonly.pdf")
        
        multi.page <- ggarrange(e1, e2,e3, 34, 
                                nrow = 2, ncol = 2)
        multi.page[[1]]
        ggexport(multi.page, filename = "event_studytrend.pdf")
        
        
        #descriptives 
        with(county_cases2, tapply(daily_count, treat1, function(x) {
          sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
        }))
        
        mean(county_cases2$daily_count)
        var(county_cases2$daily_count) #evidence of overdispersion
        
        ###########################################################################
        ####################BUILDING THE MODEL ######################################
        
        #poisson (see here https://stats.idre.ucla.edu/r/dae/poisson-regression/)
        #start w/ just pre/post
        county_cases2$pre_post<-as.factor(county_cases2$pre_post)
        
        summary(mod_p1<-glm(daily_count~pre_post, data=county_cases2, family="poisson"(link = "log")))
        stargazer(mod_p1, apply.coef = exp, type='text')
        AIC(mod_p1)
        BIC(mod_p1)
        #add offset (could also consider test offset)
        ##we add the per 100000 to make a per 100,000 rate offset
        summary(mod_1off<-glm(daily_count~pre_post + offset(log(pop/100000)), family="poisson", data=county_cases2))
        stargazer(mod_1off, apply.coef = exp, type='text')
        AIC(mod_1off)
        BIC(mod_1off)
        #offset better fit for model- include from here on. 
        
        #add treat 
        summary(mod_2off<-glm(daily_count~pre_post +treat1+ offset(log(pop/100000)), family="poisson", data=county_cases2))
        stargazer(mod_2off, apply.coef = exp, type='text')
        
        #add interaction 
        summary(mod_3<-glm(daily_count~treat1*pre_post + offset(log(pop/100000)), data=county_cases2, family="poisson"(link = "log")))
        stargazer(mod_3, apply.coef = exp, type='text')
        
        ########add other policies########
        #stay at home order
        summary(mod_3a<-glm(daily_count~treat1*pre_post + at_home + offset(log(pop/100000)), family="poisson", data=county_cases2))
        stargazer(mod_3a, apply.coef = exp, type='text')
        anova(mod_3, mod_3a,  test="Chisq")
        
        summary(county_cases2$date)
        #mask mandate
        summary(mod_3b<-glm(daily_count~treat1*pre_post + at_home + mask_mandate + offset(log(pop/100000)), family="poisson", data=county_cases2))
        stargazer(mod_3b, apply.coef = exp, type='text')
        anova(mod_3a, mod_3b,  test="Chisq")
        
        #eviction ban 
        summary(mod_3c<-glm(daily_count~treat1*pre_post + at_home + mask_mandate + end_evict + offset(log(pop/100000)), family="poisson", data=county_cases2))
        stargazer(mod_3c, apply.coef = exp, type='text')
        anova(mod_3b, mod_3c,  test="Chisq")
        
        #compare models 
        models <- list(mod_p1, mod_1off, mod_2off, mod_3)
        models1<-list(mod_3a, mod_3b, mod_3c)
        
        stargazer(models, apply.coef=exp, type = "text", title="Base Models", out="results/table1.txt")
        stargazer(models1, apply.coef=exp, type="text", title="Models w/ NPIs", out="results/table1a.txt")
        
        
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
        
        
        #add city fixed effects
        #change reference to a control austin? 
        county_cases2$cities = relevel(county_cases2$cities, ref="Austin")
        
        summary(mod_offset<-glm(daily_count~pre_post+ cities+ offset(log(pop/100000)), family="poisson", data=county_cases2))
        table(county_cases2$pre_post, county_cases2$cities)
        
        summary(mod_offset<-glm(daily_count~treat1*factor(pre_post) + Admin2+ offset(log(pop/100000)), family="poisson", data=county_cases2))
        
        ## FOR SOME REASON DROPPING OUT PORTLAND... (some sort of collinearity issue)
        
        #see here for intreprtation of poisson w/ offset http://people.virginia.edu/~am3xa/BiostatII/notes/notes15.pdf
        
        #add heteroskedasticity robust standard errors and cluster at city 
        #controls for mild violation of distribution assumption that var=mean
        #substitute final model in here for M1
        
        cov.m1 <- vcovHC(m1, type="HC0")
        std.err <- sqrt(diag(cov.m1))
        r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
                       "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
                       LL = coef(m1) - 1.96 * std.err,
                       UL = coef(m1) + 1.96 * std.err)
        
        r.est
        
        
        #event model 
        event_model1$pre_post<-as.factor(event_model1$pre_post)
        
        summary(event1<-glm(daily_count~ week + offset(log(pop/100000)), family="poisson", data=event_model1))
        
        str(county_cases2)
        
        
        