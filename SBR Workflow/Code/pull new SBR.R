###############################################################################
###
###   PURPOSE: 
###
###############################################################################


### Set Environment -------------------------

# Clear the environment 
rm(list=ls())

# Set working directory
path <- "C:\\Users\\nated\\Documents\\Documents_NB\\Projects\\Betting\\SBR Workflow"
options(stringsAsFactors = F)
setwd(path)

library(nbastatR)
library(tidyverse)
library(janitor)
library(visdat)
library(tidylog)


# Check which dates are needed to pull from SBR ---------------------------------
load("Intermediate/Game Logs/game_logs_20.Rda")
#load("Output/game_logs_10_20.Rda")


last_pull<-max(twenty_updated$dateGame)
#If you want to do a manual pull change last pull manually
#last_pull<-"2019-12-04"

# Get Logs using nbastatR -----------------------------------------------------

twenty_updated<-game_logs(seasons = 2020, 
                  league = "NBA", 
                  season_types = c("Regular Season"),
                  result_types = "team")


new_dates<-twenty_updated%>%
  filter(dateGame>last_pull)%>%
  mutate(dateGame = gsub("-","",as.character(dateGame)))%>%
  distinct(dateGame)%>%pull(dateGame)

if (length(new_dates)!=0) {
  #Pull SBR Data  --------------------------------------------------------------
  
  source_python("Code/Functions/sbrscraper_functions.py")
  
  SBRPull<- function(i){
    print(i)
    DoAnalysis(i)
  }
  
  #RUN LAPPLY ----
  data<- lapply(new_dates, SBRPull)%>%bind_rows()
  #---------------
  
  
  #Save data -------------------------------------------------------------------
  
  #Save NBAStatR Log 2020 updated
  save(twenty_updated, file = "Intermediate/Game Logs/game_logs_20.Rda")
  
  #Save SBR Spreads updated through new dates
  sbr_name = paste0("SBR_", gsub("-","",new_dates[1]), "-", gsub("-","",tail(new_dates, n = 1)), ".csv")
  write_csv(data,paste0("Intermediate/Downloaded Books/",sbr_name))
}
