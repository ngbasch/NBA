###############################################################################
###
###   PURPOSE: 
###
###############################################################################

### Pull recent SBR Data -----------------------

source("Code/update and clean sbr.R")


### Set Environment -------------------------

# Clear the environment 
rm(list=ls())

# Set working directory
path <- "C:\\Users\\nated\\Documents\\Documents_NB\\Projects\\Betting\\SBR Workflow"
options(stringsAsFactors = F)
setwd(path)

# Load packages
library(tidyverse)
library(janitor)
library(plotly)
library(tidylog)
library(forcats)
library(broom)
library(reticulate)

# Set constants



### Import Data -------------------------------------------------------------------------------------------------

#load cleaned logs
load("Intermediate/Game Logs/logs_clean.Rda")

#Load sbr betting data
load ("Output/sbr_long_pin.Rda")

#Get nba schedule
load ("Intermediate/schedule_2020.Rda")

#Read crosswalks
crosswalk<-read.csv("Input/team_crosswalk.csv")%>%
  mutate(idTeam = as.numeric(idTeam),
         nameTeam = as.character(nameTeam),
         sbrTeam = as.character(sbrTeam))


# Join Data ------------------------------------------------------------------------------------------------------

combined<-
  logs_clean%>%
  left_join(sbr_long_pin%>%
              filter(bet == "ml")%>%
              select(idGame, idTeam,sportsbook, bet, line, odds, consensus, ratio, betting_on), by = c("idGame", "idTeam"))%>%
  #Training data from 2015 to 2018
  filter(yearSeason>=2015, !is.na(consensus), !is.na(ratio),typeSeason == "Regular Season", odds!=0)%>%
  group_by(idGame)%>%
  mutate(favs = ifelse(ratio == max(ratio), "Underdog", "Favorite"))%>%
  ungroup()%>%
  mutate(VegasCorrect = ifelse((favs == "Favorite" & isWin) | favs == "Underdog" & isWin == FALSE, 1, 0),
         isB2B= ifelse(isB2B, 1, 0),
         win = ifelse(outcomeGame == "W", 1, 0))
  #Remove where lag is missing (first game of season)

reg_data<-
  combined%>%
  group_by(idGame)%>%
  filter(!is.na(net_rating_cum_lag))%>%
  filter(n() == 2)%>%
  select(idTeam, yearSeason, dateGame, idGame,isB2B, home, numberGameTeamSeason, contains("_lag"), -net_rating_cum_lag, -ts_percent_cum_lag, -contains("possession"), 
         consensus, ratio, VegasCorrect, win)%>%
  mutate_at(vars(-idTeam, -yearSeason, -idGame, -dateGame, -home, -ratio, - consensus, -VegasCorrect, -win,-numberGameTeamSeason),
            .funs = list(opp = ~ifelse(home == 0, 
                                       .[home == 1],
                                       .[home == 0])))%>%
  ungroup()

### Analyze Data ---------------------------------------------------------------------------------------------------


#Predict if Vegas is correct?
first_model<- 
  reg_data%>%
  filter(yearSeason==2020)%>%
#  select(-VegasCorrect)%>%
#  select(-dateGame, -idTeam, -yearSeason)%>%
  #glm(VegasCorrect ~  . -dateGame - idTeam - yearSeason -win, data = .,family = "binomial")
  glm(win ~  . -dateGame - idTeam - yearSeason , data = .,family = "binomial")


summary(first_model)

#How is fti
prediction = augment(first_model, reg_data%>%filter(yearSeason == 2020), type.predict = "response")%>%
  left_join(crosswalk%>%distinct(idTeam, .keep_all=T))




ggplot(prediction)+
  #geom_violin(aes(x = as.factor(VegasCorrect), y = .fitted), alpha = 0.01, color = "blue")+
  geom_boxplot(aes(x = as.factor(win), y = .fitted), color = "blue")+
  facet_wrap(~yearSeason)



glance(first_model, model)


#Predict future games -------------------------------------
#Clean future schedule file
source_python("Code/Functions/sbrscraper_functions.py")

SEARCH_DATE<-gsub("-","",as.character(Sys.Date()))

today_sbr<-DoAnalysis(SEARCH_DATE)
today_sbr<-today_sbr%>%
  mutate(dateGame = as.Date(key, format = "%Y%m%d"),
            sbrTeam = team,
            consensus = as.numeric(ml_consensus)/100,
            ml_PIN = as.numeric(ml_PIN),
            ratio = ifelse(ml_PIN<0, -100/ml_PIN, ml_PIN/100)
            )%>%
  select(dateGame, sbrTeam, ratio, consensus)
  

todays_info<-combined%>%
  select(idTeam, yearSeason, dateGame, idGame, home, numberGameTeamSeason, contains("_cum"),-contains("_lag"), -net_rating_cum, -ts_percent_cum, -contains("possession"))%>%
  rename_at(vars(contains("_cum")), .funs = ~paste0(., "_lag"))%>%
  group_by(idTeam)%>%
  #filter(dateGame < "2019-12-15")%>%
  filter(dateGame == max(dateGame))%>%
  select(-dateGame, -idGame, -home, -numberGameTeamSeason)%>%
  ungroup()%>%
  #Take today's slate of games
  right_join(sched_2020%>%
               filter(!idGame %in%combined$idGame)%>%
               filter(dateGame == min(dateGame)) 
               #filter(dateGame == "2019-12-15" )
             )%>%
  group_by(idGame)%>%
  mutate_at(vars(-idTeam, -yearSeason, -idGame, -dateGame, -home),
            .funs = list(opp = ~ifelse(home == 0, 
                                       .[home == 1],
                                       .[home == 0])))%>%
  left_join(crosswalk%>%distinct(idTeam, .keep_all=T))%>%
  #Join sbr data from today
  left_join (today_sbr)%>%
  select(-nameTeam, -sbrTeam)%>%
  ungroup()

#Predict probability of whether Today's Vegas predictions are wrong
predicted<-todays_info%>%
  mutate(prediction = predict(first_model, ., type = "response"))%>%
  left_join(crosswalk%>%distinct(idTeam, .keep_all=T))




#---------------------------------------------------------

#Category 1: Predict probability of a given bet. Should work for all three bet types
#Win/Lose = TeamAHome[various team stats,  strength of schedule, negative_win_shares_injury] + 
#          TeamBAway[various team stats, strength of schedule, negative_win_shares_injury] +
#          Odds[Team A (or Team B)] +
#          PercentageBettors[Team A (or Team B)] +
#          Some time fixed effects (since uncertainty is bound to change throughout the season? See if this is significant)




#Category 2: Predict spread given performance stats, independent of odds. Compare to vegas predicted spreads

#Category 3: Training and test dataset, binary classification?

#Category 4: Use nearest neighbor approach to find how previous games went

#Category 5: Simpler approach of score differential throughout game (this is probably more useful for real time betting)




#Helpful links:
##https://towardsdatascience.com/using-linear-regression-to-model-point-spreads-in-college-basketball-f7da5811c3da
##https://sports.sites.yale.edu/nba-model-math
