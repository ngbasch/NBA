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

# Load packages
library(tidyverse)
library(janitor)
library(plotly)
library(tidylog)
library(forcats)

# Set constants



### Import Data -------------------------------------------------------------------------------------------------

#read logs
load("Intermediate/Game Logs/logs_clean.Rda")


#Visualize Data ------------------------------------------------------------------------------------------------------

#How does offensive efficiency change game to game?
logs_clean%>%
  #select(nameTeam, idGame, dateGame,offensive_efficiency, offensive_efficiency_cum)%>%
 # select(nameTeam, idGame, dateGame,defensive_efficiency, defensive_efficiency_cum)%>%
  select(nameTeam, idGame, dateGame,offensive_efficiency_cum, defensive_efficiency_cum)%>%
  #select(nameTeam, idGame, dateGame,net_rating, net_rating_cum)%>%
  gather(var, val, 4:5)%>%
  ggplot(aes(x = dateGame, y = val, colour = var))+
  geom_line()+
  geom_hline(yintercept = 100)+
  facet_wrap(~nameTeam)+
  theme_bw()

t<-logs_clean%>%
  select(locationGame,nameTeam, idGame, dateGame,outcomeGame, net_rating_cum)%>%
  spread(locationGame, net_rating_cum)
  ggplot(aes(x = lag_net_rating_cum))

reg_data%>%
  filter(yearSeason == "2020")%>%
  filter(nameTeam == "Indiana Pacers")%>%
  ggplot(aes(x = dateGame, y = plusminusTeam, color = net_rating_cum_lag_opp))+
  geom_hline(yintercept = 0)+
  geom_point()+
  #scale_colour_gradient(limits = c(-5,5))+
  scale_colour_viridis_c()+
  theme_bw()+
  facet_wrap(~nameTeam)

## Show ggplot for each tema showing certain efficiencies for each game, and then 
### do another graph showing the running efficiencies to each (what vegas would be looking at)
#Can we weight it by strength of opponent? 

  
