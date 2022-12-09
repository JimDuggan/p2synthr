library(p2synthr)
library(lubridate)
library(dplyr)
library(ggpubr)
library(ggplot2)

irl <- owid %>%
        select(date,location,new_cases) %>%
        mutate(year=year(date),
               week=week(date)) %>%
        filter(location=="Ireland") %>%
        group_by(year,week) %>%
        summarise(date=first(date),cases=sum(new_cases,na.rm=T))

ps <- synth_ps(pull(irl,cases),
               dates=pull(irl,date),
               population_size = 5000000,
               reporting_fraction = 0.70,
               proportion_enrolled = 0.005,
               leading_time = 2,
               false_positive_fraction = 0.05,
               set_seed = F,
               SEED=100,
               rbinomial_size = 2,
               active_users_lower = 0.65,
               active_users_upper = 0.80)


p1 <- ggplot(irl,aes(x=date,y=cases))+geom_col(colour="steelblue")+
  xlab("Date")+ylab("Reported Cases")

p2 <- ggplot(ps,aes(x=Date,y=PSSyntheticReported))+geom_line()+geom_point()+
      xlab("Date")+ylab("Synthetic Reported Infections (PS)")

ps_cc <- ps %>%
          filter(complete.cases(ps))

cors <- ccf(pull(ps_cc,WeeklyCases),
            pull(ps_cc,PSSyntheticReported))

cors <- ccf(pull(ps_cc,PSSyntheticReported),
            pull(ps_cc,WeeklyCases),
            plot=F)

p3 <- ggplot(cors_t,aes(x=Lag,y=ACF))+geom_col(width = .1)


p4 <- ggarrange(p1,p2,p3,nrow=3)

