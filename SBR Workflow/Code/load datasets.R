###############################################################################
###
###   PURPOSE: 
###
###############################################################################

### Pull recent SBR Data -----------------------

#source("Code/update and clean sbr.R")


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
crosswalk<-read_excel("Input/team_crosswalk.xlsx")%>%
  mutate(idTeam = as.numeric(idTeam),
         nameTeam = as.character(nameTeam),
         sbrTeam = as.character(sbrTeam))

