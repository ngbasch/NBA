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
library(reticulate)
library(rsample)    # for creating validation splits
library(recipes)    # for feature engineering
library(caret)       # for fitting KNN models

### 1. Pull recent SBR and NBA Log Data ------------------------------------------------

#Update odds databases with new data
#Update NBA game logs and cleaned log dataset
source("Code/update and clean sbr.R")

### 2. Load and Prep data ---------------------------------------------------------
#Load datasets
source("Code/Load datasets.R")


#Prepare and clean historical Data for modeling
source("Code/regression prep.R")

#Pull today's games
source_python("Code/Functions/sbrscraper_functions.py")
source("Code/todays lines.R")
todays_info<-GetTodaysPicks()


### 3. Regression Modeling Approach ------------------------------------------------------------------
#Load functions
source("../Analysis/regression models.R")

#Define which variables to use as predictors
PERFORMANCE_STATS = c("isB2B", "offensive_efficiency_cum_lag", "defensive_efficiency_cum_lag", "efg_percent_cum_lag", 
                      "tov_percent_cum_lag", "ft_rate_cum_lag", "orb_percent_cum_lag", "drb_percent_cum_lag")
#DEPENDENT<- "win"
INDEPENDENTS<- c("home", "numberGameTeamSeason", 
                 PERFORMANCE_STATS, paste0(PERFORMANCE_STATS, "_opp"))



#MoneyLine -------------------

#Likelihood of winning giving stats
ml_logit<-LinearModel("win", c(INDEPENDENTS, "consensus_ml"), type = "Logit")
#Likelihood of Vegas being correct (in choosing underdog and favorite) given historical Vegas data
ml_logit2<-LinearModel("VegasCorrectML", c(INDEPENDENTS, "consensus_ml"), type = "Logit")



#Spreads ----------------------

#Predict actual spread based on performance only
rl_lm<-LinearModel("plusminusTeam", INDEPENDENTS, type = "LM")
#Predict actual spread based on performance plus vegas sprad
rl_lm_v2<-LinearModel("plusminusTeam", c(INDEPENDENTS, "sbr_spread", "consensus_rl"), type = "LM")

#Predict difference between Vegas and actual
rl_lm2<-LinearModel("dif_spread", c(INDEPENDENTS, "sbr_spread", "consensus_rl"),type = "LM")

#Predict whether SBR is correct or not
rl_logit<-LinearModel("VegasCorrectRL", c(INDEPENDENTS, "consensus_rl"), type = "Logit")

#Function and input equation given historical DF, today's DF, equation



#Totals -----------------------

#Predict actual total
to_lm<-LinearModel("ptsTotal", INDEPENDENTS, type = "LM")
#Function and input equation given historical DF, today's DF, equation
to_lm_v2<-LinearModel("ptsTotal", c(INDEPENDENTS, "sbr_total", "consensus_to"), type = "LM")

#Predict whether SBR is correct or not
to_logit<-LinearModel("VegasCorrectTO", c(INDEPENDENTS, "consensus_to"), type = "Logit")

#Predict difference between Vegas and actual
to_dif<-LinearModel("dif_total", c(INDEPENDENTS, "sbr_total", "consensus_to"), type = "LM")

### 4. Machine Learning Approach ------------------------------------------------------------------

#To fill in

### 5. Calculate Expected Returns --------------------------------------------------------
#For Logit regression, it's the probability of winning * bet.


ml_today<-LinearToday("VegasCorrectML", c(INDEPENDENTS, "consensus_ml"), type = "Logit")
ml_today_win<-LinearToday("win",INDEPENDENTS, type = "Logit")
rl_today<-LinearToday("plusminusTeam", c(INDEPENDENTS, "sbr_spread", "consensus_rl"),type = "LM")
to_today<-LinearToday("VegasCorrectTO", c(INDEPENDENTS, "consensus_to"), type = "Logit")
to_today_total<-LinearToday("ptsTotal", c(INDEPENDENTS), type = "LM")
to_today_dif<-LinearToday("dif_total", c(INDEPENDENTS, "sbr_total", "consensus_to"), type = "LM")
