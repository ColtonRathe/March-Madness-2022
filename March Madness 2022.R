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
days   <- c("1", "2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13","14", "15", "16", 
            "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")
years  <- c("2011","2012", "2013", "2014", "2015", "2016", "2017", "2018","2019", "2021","2022")

master_boxscore = data.frame()

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