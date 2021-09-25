###############################################################################
###
###   PURPOSE: 
###
###############################################################################


### Set Environment -------------------------

# Clear the environment 
rm(list=ls())

# Set working directory
path <- "C:\\Users\\nated\\Documents\\Documents_NB\\Projects\\Betting"
options(stringsAsFactors = F)
setwd(path)

# Load packages
library(tidyverse)
library(janitor)
library(plotly)
library(tidylog)
library(nbastatR)

# Get Logs using nbastatR -----------------------------------------------------


logs<-game_logs(seasons = seq(2010,2019, by = 1), 
                league = "NBA", 
                season_types = c("Regular Season", "Playoffs"),
                result_types = "team")

twenty<-game_logs(seasons = 2020, 
                  league = "NBA", 
                  season_types = c("Regular Season"),
                  result_types = "team")

#CLean data -------------------------------------------------------------------

dates<-logs%>%
  bind_rows(twenty)%>%
  distinct(dateGame)%>%
  mutate(formatted_date=gsub("-","", as.character(dateGame)))

logs_twenty<-
  logs%>%bind_rows(twenty)

#Save -------------------------------------------------------------------------
save(logs_twenty, file  = "Intermediate/game_logs_10_20.Rda")
write_csv(dates,"Intermediate/games_to_pull_10_20.csv")
#----
