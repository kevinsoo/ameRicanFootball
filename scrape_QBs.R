######################################################################
# This scrapes all QB data from http://www.pro-football-reference.com/
# Author: Kevin Soo
######################################################################

# load packages
library(tidyverse)
library(rvest)
library(stringr)

# get index of QBs
qbindex <- read_html("http://www.pro-football-reference.com/players/qbindex.htm")
qbtable <- qbindex %>% html_table(fill = TRUE)
qbtable <- data.frame(qbtable[1])
qbtable$Seasons <- qbtable$To - qbtable$From + 1

# get links of QBs
qblinks <- qbindex %>% html_nodes('#players a') %>% html_attr('href')
qbtable <- data.frame(qbtable, qblinks)

# filter QBs based on years active
qbtable <- qbtable %>% filter(To>=2012, Seasons>=3)

# get url for player gamelogs
qbtable$url <- rep(NA, nrow(qbtable))
for (n in 1:nrow(qbtable)) {
    qbtable$url[n] <- paste("http://www.pro-football-reference.com", 
                            substr(qbtable$qblinks[n], 1, str_locate(qbtable$qblinks[n], 'htm')[1]-2),
                            "/gamelog/", sep="")
}

# scraping
for (q in 1:nrow(qbtable)) {
    # scrape tables
    player <- read_html(qbtable$url[q])
    stats <- player %>% html_table(fill = TRUE)
    regularSeason <- data.frame(stats[1])
    
    # rename columns
    colnames(regularSeason) <- regularSeason[1,]
    
    # eliminate filler rows
    regularSeason$Rk <- as.numeric(regularSeason$Rk)
    regularSeason <- regularSeason %>% na.omit()
    
    # select and name columns
    regularSeason <- regularSeason[,2:21]
    colnames(regularSeason) <- c("Year", "Date", "GameNum", "Age", "Team", "HomeAway", "Opponent", "Result", "Start",
                                 "PassComplete", "PassAttempt", "PassAccuracy", "PassYards", "PassTD", "PassIntercepted", 
                                 "QBRating", "Sacked", "YardsLost", "YardsPerPassAttempt", "AdjustedYardsPerPassAttempt")
    
    # parse poorly formatted columns
    regularSeason$WinLoss <- NA
    regularSeason$ScoreTeam <- NA
    regularSeason$ScoreOpp <- NA
    regularSeason$AgeYears <- NA
    regularSeason$AgeDays <- NA
    for (i in 1:nrow(regularSeason)) {
        # parse win-loss columns
        regularSeason$WinLoss[i] <- strsplit(regularSeason$Result[i], " ")[[1]][1]
        score <- strsplit(regularSeason$Result[i], " ")[[1]][2]
        regularSeason$ScoreTeam[i] <- strsplit(score, "-")[[1]][1]
        regularSeason$ScoreOpp[i] <- strsplit(score, "-")[[1]][2]
        
        # parse age columns
        regularSeason$AgeYears[i] <- strsplit(regularSeason$Age[i], "-")[[1]][1]
        regularSeason$AgeDays[i] <- strsplit(regularSeason$Age[i], "-")[[1]][2]
    }
    
    # recode columns
    regularSeason$HomeAway <- ifelse(regularSeason$HomeAway=="@", "A", ifelse(regularSeason$HomeAway=="", "H", "N"))
    regularSeason$Start <- ifelse(regularSeason$Start=="", 0, 1)
    regularSeason$AgeDays <- as.numeric(regularSeason$AgeDays)
    regularSeason$Type <- "RegularSeason"
    
    # select columns
    regularSeason <- regularSeason %>% select(Type, Year:GameNum, 
                                              AgeYears, AgeDays, 
                                              Team:Opponent, 
                                              WinLoss:ScoreOpp, 
                                              Start:AdjustedYardsPerPassAttempt)
    
    # for players with playoff stats
    if (length(player)==2) {
        # get table
        playoffs <- data.frame(stats[2])
        
        # rename columns
        colnames(playoffs) <- playoffs[1,]
        
        # eliminate filler rows
        playoffs$Rk <- as.numeric(playoffs$Rk)
        playoffs <- playoffs %>% na.omit()
        
        # select and name columns
        playoffs <- playoffs[,2:21]
        colnames(playoffs) <- c("Year", "Date", "GameNum", "Age", "Team", "HomeAway", "Opponent", "Result", "Start",
                                "PassComplete", "PassAttempt", "PassAccuracy", "PassYards", "PassTD", "PassIntercepted", 
                                "QBRating", "Sacked", "YardsLost", "YardsPerPassAttempt", "AdjustedYardsPerPassAttempt")
        
        # parse poorly formatted columns
        playoffs$WinLoss <- NA
        playoffs$ScoreTeam <- NA
        playoffs$ScoreOpp <- NA
        playoffs$AgeYears <- NA
        playoffs$AgeDays <- NA
        for (i in 1:nrow(playoffs)) {
            # parse win-loss columns
            playoffs$WinLoss[i] <- strsplit(playoffs$Result[i], " ")[[1]][1]
            score <- strsplit(playoffs$Result[i], " ")[[1]][2]
            playoffs$ScoreTeam[i] <- strsplit(score, "-")[[1]][1]
            playoffs$ScoreOpp[i] <- strsplit(score, "-")[[1]][2]
            
            # parse age columns
            playoffs$AgeYears[i] <- strsplit(playoffs$Age[i], "-")[[1]][1]
            playoffs$AgeDays[i] <- strsplit(playoffs$Age[i], "-")[[1]][2]
        }
        
        # recode columns
        playoffs$HomeAway <- ifelse(playoffs$HomeAway=="@", "A", ifelse(playoffs$HomeAway=="", "H", "N"))
        playoffs$Start <- ifelse(playoffs$Start=="", 0, 1)
        playoffs$AgeDays <- as.numeric(playoffs$AgeDays)
        playoffs$Type <- "Playoffs"
        
        # select columns
        playoffs <- playoffs %>% select(Type, Year:GameNum, 
                                        AgeYears, AgeDays, 
                                        Team:Opponent, 
                                        WinLoss:ScoreOpp, 
                                        Start:AdjustedYardsPerPassAttempt)
        
        # combine data frames
        allStats <- rbind(regularSeason, playoffs) %>% arrange(Date)
        Game <- 1:nrow(allStats)
        Player <- rep(qbtable$Player[q], nrow(allStats))
        allStats <- data.frame(Player, Game, allStats)
    }
    
    # if no playoff stats
    if (length(player)==1) {
        allStats <- regularSeason %>% arrange(Date)
        Game <- 1:nrow(allStats)
        Player <- rep(qbtable$Player[q], nrow(allStats))
        allStats <- data.frame(Player, Game, allStats)   
    }
    
    # combine players
    if (q==1) { QBStats <- allStats }
    else { QBStats <- rbind(QBStats, allStats) }
}

# set NA entries to 0
QBStats$PassComplete <- ifelse(is.na(QBStats$PassComplete)==TRUE, 0, QBStats$PassComplete)
QBStats$PassAttempt <- ifelse(is.na(QBStats$PassAttempt)==TRUE, 0, QBStats$PassAttempt)
QBStats$PassYards <- ifelse(is.na(QBStats$PassYards)==TRUE, 0, QBStats$PassYards)
QBStats$PassTD <- ifelse(is.na(QBStats$PassTD)==TRUE, 0, QBStats$PassTD)
QBStats$PassIntercepted <- ifelse(is.na(QBStats$PassIntercepted)==TRUE, 0, QBStats$PassIntercepted)

# set data types
QBStats$Player <- as.factor(QBStats$Player)
QBStats$Game <- as.numeric(QBStats$Game)
QBStats$Year <- as.numeric(QBStats$Year)
QBStats$Date <- as.Date(QBStats$Date)
QBStats$GameNum <- as.numeric(QBStats$GameNum)
QBStats$AgeYears <- as.numeric(QBStats$AgeYears)
QBStats$AgeDays <- as.numeric(QBStats$AgeDays)
QBStats$Team <- as.factor(QBStats$Team)
QBStats$HomeAway <- as.factor(QBStats$HomeAway)
QBStats$Opponent <- as.factor(QBStats$Opponent)
QBStats$WinLoss <- as.factor(QBStats$WinLoss)
QBStats$ScoreTeam <- as.numeric(QBStats$ScoreTeam)
QBStats$ScoreOpp <- as.numeric(QBStats$ScoreOpp)
QBStats$Start <- as.factor(QBStats$Start)
QBStats$PassComplete <- as.numeric(QBStats$PassComplete)
QBStats$PassAttempt <- as.numeric(QBStats$PassAttempt)
QBStats$PassAccuracy <- as.numeric(QBStats$PassAccuracy)
QBStats$PassYards <- as.numeric(QBStats$PassYards)
QBStats$PassTD <- as.numeric(QBStats$PassTD)
QBStats$PassIntercepted <- as.numeric(QBStats$PassIntercepted)
QBStats$QBRating <- as.numeric(QBStats$QBRating)
QBStats$Sacked <- as.numeric(QBStats$Sacked)
QBStats$YardsLost <- as.numeric(QBStats$YardsLost)
QBStats$YardsPerPassAttempt <- as.numeric(QBStats$YardsPerPassAttempt)
QBStats$AdjustedYardsPerPassAttempt <- as.numeric(QBStats$AdjustedYardsPerPassAttempt)

# save data
save(QBStats, file="QBStats.Rda")
