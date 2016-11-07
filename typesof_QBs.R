#######################
# Types of Quarterbacks
# Author: Kevin Soo
#######################

# load stuff
library(tidyverse)
theme_set(theme_bw())
load("QBStats.Rda")

# periods of interest
QBStats$Period <- ifelse(as.numeric(as.Date(QBStats$Date)) < as.numeric(as.Date("2006-09-01")), "Pre",
                         ifelse(as.numeric(as.Date(QBStats$Date)) < as.numeric(as.Date("2015-09-01")), "During", "Current"))
# isolate QBs of interest
QBStats$QB <- as.factor(ifelse(QBStats$Player=="Aaron Rodgers", "Aaron Rodgers", 
                               ifelse(QBStats$Player=="Tom Brady", "Tom Brady", "Other")))

# isolate time period
df <- QBStats %>% filter(Year>=2014, Year<2016)

ggplot(df, aes(x=Date, group=QB, y=QBRating)) + 
    geom_point(aes(color=QB), alpha=0.2) + 
    geom_path(data=filter(df, QB %in% c("Tom Brady", "Aaron Rodgers")), aes(color=QB, x=Date, y=QBRating)) +
    geom_vline(xintercept = as.numeric(as.Date(c("2006-09-01", "2015-09-01"))), linetype="dashed") +
    stat_smooth(method="lm", aes(color=QB)) +
    scale_x_date()

# get average stats for QBs other than Tom Brady by game number (only regular season)
PassStats <- QBStats %>% filter(Player!="Tom Brady", Type=="RegularSeason", Year>=2004) %>% 
    group_by(Year, GameNum) %>%
    summarise(AverageDate=as.Date(mean(as.numeric(as.Date(Date))), origin="1970-01-01"), 
              Attempts.M=mean(PassAttempt), Attempts.SD=sd(PassAttempt),
              Completed.M=mean(PassComplete), Completed.SD=sd(PassComplete),
              Accuracy.M=mean(PassAccuracy), Accuracy.SD=sd(PassAccuracy))

ggplot(PassStats, aes(x=AverageDate, y=Accuracy.M)) + geom_smooth()
