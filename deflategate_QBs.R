#######################################
# Visualizes QB passing stats over time
# Author: Kevin Soo
#######################################

# load stuff
library(tidyverse)
library(ggthemes)
load("QBStats.Rda")

# periods of interest
QBStats$Period <- ifelse(as.numeric(as.Date(QBStats$Date)) < as.numeric(as.Date("2006-09-01")), "Pre",
                         ifelse(as.numeric(as.Date(QBStats$Date)) < as.numeric(as.Date("2015-09-01")), "During", "Current"))

# get stats for QBs other than Tom Brady by game number (only regular season)
PassStats <- QBStats %>% filter(Player!="Tom Brady", Type=="RegularSeason") %>% 
                    group_by(Year, GameNum) %>%
                    summarise(AverageDate=as.Date(mean(as.numeric(as.Date(Date))), origin="1970-01-01"), 
                              Attempts.M=mean(PassAttempt), Attempts.SD=sd(PassAttempt),
                              Completed.M=mean(PassComplete), Completed.SD=sd(PassComplete),
                              Accuracy.M=mean(PassAccuracy), Accuracy.SD=sd(PassAccuracy))

PassStats$GameWeek <- as.Date(paste(PassStats$Year, "-", PassStats$GameNum, sep="")) # label time
ggplot(PassStats, aes(x=GameWeek, y=Accuracy.M)) + geom_path()

ggplot(QBStats, aes(x=Date, group=Player, y=PassAccuracy)) + 
    geom_point(aes(color=Player), alpha=0.2) + 
    geom_vline(xintercept = as.numeric(as.Date(c("2006-09-01", "2015-09-01"))), linetype="dashed") +
    # stat_smooth(aes(color=Player), method="lm") +
    scale_x_date() +
    theme_minimal()
