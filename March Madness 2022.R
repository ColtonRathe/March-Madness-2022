#March Madness Prediction

######### Load Libraries ######################

#install.packages("rvest")
library(rvest)
#install.packages("pylr")
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("conflicted")
library(conflicted)
#install.packages("XML")
library(XML)

######### Pull Box Score Data ###########################

#Train 2011-2019 test 2021 (before selection sunday), no tourny in 2020 
#then use 2022

### Box score data 
#Date combinations for the March Madness tournament 
months <- c("3", "4")
days   <- c("1", "2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13","14", "15", "16","17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")
years  <- c("2011","2012", "2013", "2014", "2015", "2016", "2017", "2018","2019", "2021","2022")

master_boxscore = data.frame()

#if timeout error, break up query by year
for (day in days) {
  for (month in months) {
    for (year in years) {
      
      #pull the boxscores for the day 
      url <- paste("https://www.sports-reference.com/cbb/boxscores/index.cgi?month=", month, "&day=", day, "&year=", year, sep="")
      pg <- read_html(url)
      tables <- html_table(pg, fill = TRUE)
      
      #check to see make sure url has boxscores to append
      if (length(tables) > 0 ) {
        
        len_tables <- seq(1:length(tables))
        
        #append the games from the day to the master box score dataset 
        for (table in len_tables) {
          tb_boxscore<- tables[[table]]
          tb_boxscore$Day <- day
          tb_boxscore$Month <- month
          tb_boxscore$Year <- year
          master_boxscore <- rbind(master_boxscore, tb_boxscore)
        }
      }
    }
  }
}

######### Subset to only March Madness games ######################
conflict_prefer("lag", "dplyr")

#lead 1 row to obtain team 2 in the match up 
master_boxscore$lead1 <- lead(master_boxscore$X1)

#lead 2 row to obtain tournament type (first round, second round, etc)
master_boxscore$lead2 <- lead(master_boxscore$X1, 2)

#lead 1 row to obtain team 2 scored points 
master_boxscore$scorelead1 <- lead(master_boxscore$X2)

#subset to only NCAA tournament games 
index_pos <- which(grepl("NCAA", master_boxscore$lead2, fixed = TRUE))
index_master_boxscore <- master_boxscore[index_pos,]

######### Clean up variable names, make scores numeric, and take only needed columns ######################
#rename variables to more descriptive 
conflict_prefer("rename", "dplyr")
index_master_boxscore_rename <- index_master_boxscore %>% 
  rename(
    Tournament_Game = lead2,
    Team1_NameSeed = X1,
    Team1_Score = X2,
    Team2_NameSeed = lead1,
    Team2_Score = scorelead1
  )

#make scores numeric
index_master_boxscore_rename$Team1_Score <- as.numeric(index_master_boxscore_rename$Team1_Score)
index_master_boxscore_rename$Team2_Score <- as.numeric(index_master_boxscore_rename$Team2_Score)

str(index_master_boxscore_rename)

master_boxscore_final <- index_master_boxscore_rename %>% select (Tournament_Game, Year, Month, Day, 
                                                                  Team1_NameSeed, Team1_Score, Team2_NameSeed, Team2_Score)

master_boxscore_final_2022 <- subset(master_boxscore_final, master_boxscore_final$Year==2022)

head(master_boxscore_final)

conflict_prefer("arrange", "dplyr")
master_boxscore_final<- arrange(master_boxscore_final, Year, Month, Day)

######### Extract team and seed to create two differents columns ######################
#Extract team and seed to create two different columns
master_boxscore_final$Team1_SeedA <-  gsub("[[:punct:]]", "", master_boxscore_final$Team1_NameSeed)
tail(master_boxscore_final$Team1_SeedA)
master_boxscore_final$Team1_SeedB <- gsub("[[:alpha:]]", "", master_boxscore_final$Team1_SeedA)
tail(master_boxscore_final$Team1_SeedB)
master_boxscore_final$Team1_Seed <- as.numeric(gsub("[[:blank:]]", "", master_boxscore_final$Team1_SeedB))
master_boxscore_final$Team1_Seed
master_boxscore_final$Team1 <- gsub("[[:digit:]]", "", master_boxscore_final$Team1_SeedA)

#trim leading and trailing white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
master_boxscore_final$Team1 <- trim(master_boxscore_final$Team1)

master_boxscore_final$Team1_SeedA  <- NULL
master_boxscore_final$Team1_SeedB  <- NULL
master_boxscore_final$Team1_NameSeed <- NULL

master_boxscore_final$Team2_SeedA <-  gsub("[[:punct:]]", "", master_boxscore_final$Team2_NameSeed)
master_boxscore_final$Team2_SeedA
master_boxscore_final$Team2_SeedB <- gsub("[[:alpha:]]", "", master_boxscore_final$Team2_SeedA)
master_boxscore_final$Team2_SeedB
master_boxscore_final$Team2_Seed <- as.numeric(gsub("[[:blank:]]", "", master_boxscore_final$Team2_SeedB))
master_boxscore_final$Team2_Seed
master_boxscore_final$Team2 <- gsub("[[:digit:]]", "", master_boxscore_final$Team2_SeedA)
#trim leading and trailing white space
master_boxscore_final$Team2 <- trim(master_boxscore_final$Team2)

master_boxscore_final$Team2_SeedA  <- NULL
master_boxscore_final$Team2_SeedB  <- NULL
master_boxscore_final$Team2_NameSeed <- NULL
write.csv(master_boxscore_final, "march_madness_2011_2021_with_play_in_games.csv")

######### Take out play-in games (comment out if you want to keep play-in games) ######################
master_boxscore_final <-subset(master_boxscore_final, !(  (master_boxscore_final$Tournament_Game== 'NCAA, East - First Round' & master_boxscore_final$Year == '2011') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, Southeast - First Round' & master_boxscore_final$Year == '2011') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, Southwest - First Round' & master_boxscore_final$Year == '2011') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, Midwest - First Round' & master_boxscore_final$Year == '2012') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, West - First Round' & master_boxscore_final$Year == '2012') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, South - First Round' & master_boxscore_final$Year == '2012') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, East - First Round' & master_boxscore_final$Year == '2013') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, Midwest - First Round' & master_boxscore_final$Year == '2013') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, West - First Round' & master_boxscore_final$Year == '2013') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, South - First Round' & master_boxscore_final$Year == '2014') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, Midwest - First Round' & master_boxscore_final$Year == '2014') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, East - First Round' & master_boxscore_final$Year == '2015') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, South - First Round' & master_boxscore_final$Year == '2015') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, Midwest - First Round' & master_boxscore_final$Year == '2015') |
                                                            (master_boxscore_final$Tournament_Game== 'NCAA, West - First Round' & master_boxscore_final$Year == '2015') |
                                                            master_boxscore_final$Tournament_Game== 'NCAA, South - First Four' | 
                                                            master_boxscore_final$Tournament_Game== 'NCAA, East - First Four' |
                                                            master_boxscore_final$Tournament_Game== 'NCAA, West - First Four' |
                                                            master_boxscore_final$Tournament_Game== 'NCAA, Midwest - First Four'))

#630
master_boxscore_final <- master_boxscore_final %>% select (Tournament_Game, Year, Month, Day, 
                                                           Team1, Team1_Seed, Team1_Score, Team2, Team2_Seed, Team2_Score)
write.csv(master_boxscore_final, "march_madness_2011_2021.csv")

