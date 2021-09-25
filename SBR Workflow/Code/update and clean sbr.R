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

#Read all csvs ---------------------------------------------------------

source("Code/pull new SBR.R")

files<-list.files("Intermediate/Downloaded Books", pattern = "\\.csv$")

sbr<-lapply(files, function(x) read_csv(paste0("Intermediate/Downloaded Books/", x), col_types = cols(.default = "c")))%>%bind_rows()

#Read Logs
load("Intermediate/Game Logs/game_logs_10_19.Rda")
load("Intermediate/Game Logs/game_logs_20.Rda")
logs<-bind_rows(logs_10_19, twenty_updated)

#Read crosswalks
crosswalk<-read_excel("Input/team_crosswalk.xlsx")%>%
  mutate(idTeam = as.numeric(idTeam),
         nameTeam = as.character(nameTeam),
         sbrTeam = as.character(sbrTeam))

#Clean SBR data ------------------------------------------------------------
clean_sbr<-sbr%>%
  filter(team !="MISSING", opp_team != "MISSING")%>%
  select(-ml_time, -rl_time,-tot_time)%>%
  mutate(date = as.Date(key, format = "%Y%m%d"))

sbr_wide<-logs%>%
  left_join(crosswalk)%>%
  select(yearSeason, typeSeason,idTeam, idGame, locationGame, dateGame, sbrTeam, outcomeGame, ptsTeam, plusminusTeam)%>%
  group_by(idGame)%>%
  mutate(ptsTotal = sum(ptsTeam, na.rm=T),
         plusminusTeam = -plusminusTeam)%>%
  left_join(clean_sbr, by = c("dateGame" = "date", "sbrTeam" = "team"))%>%
  ungroup()

consensus<-
  sbr_wide%>%
  select(idGame, sbrTeam, contains("consensus"))%>%
  gather(bet, consensus, 3:5)%>%
  mutate(bet = substr(bet, 1, 2),
         consensus = as.numeric(consensus)/100)%>%
  group_by(idGame, bet)%>%
  mutate(keep = ifelse(sum(consensus,na.rm=T)!=1,
                            F,
                            T))%>%
  ungroup()%>%
  mutate(consensus = ifelse(keep, consensus, NA))%>%
  select(-keep)

sbr_long<-
  sbr_wide%>%
  select(-contains("consensus"))%>%
  gather(var, val, ml_PIN:tot_BOL_odds)%>%
  #filter(grepl("PIN",var))%>%
  mutate(type = ifelse(grepl("ml|odds",var), "odds", "line"),
         sportsbook = trimws(gsub("[[:alpha:]][a-z]+|[a-z][[:alpha:]]+|_", "", var)),
         bet = substr(var, 1, 2))%>%
  select(-var)%>%
  spread(type, val)%>%
  left_join(consensus)%>%
  mutate(line = ifelse(grepl("PK", line), 0, line),
         odds = gsub("PK","", odds))%>%
  filter(!is.na(odds))%>%
  mutate(line = as.numeric(line),
         odds = as.numeric(odds),
         ratio = ifelse(odds<0, -100/odds, odds/100),
         betting_on = ifelse(bet != "to", sbrTeam, 
                             ifelse(locationGame == "A", "Over", "Under")))

sbr_long_pin <-sbr_long%>%filter(sportsbook == "PIN")


#Clean Logs Data ------------------------------------------------------------

#Calculate offensive efficiency, defensive efficiency, etc.
logs_clean<-
  logs%>%
  #CALCULATE STATISTICS FROM GAME LOG DATA
  mutate(possession = fgaTeam + tovTeam + 0.4*ftaTeam - orebTeam,
         offensive_efficiency = 100*ptsTeam/possession,
         ts_percent = ptsTeam / (2*(fgaTeam + 0.44 * ftaTeam)),
         efg_percent = (fgmTeam + 0.5*fg3mTeam)/fgaTeam,
         tov_percent = tovTeam / (fgaTeam+0.44*ftaTeam + tovTeam),
         ft_rate = ftaTeam / fgaTeam,
         home = ifelse(locationGame == "H", 1, 0)
  )%>%
  #Calculate some opponent stats
  group_by(idGame)%>%
  mutate_at(vars(c("nameTeam", "orebTeam", "drebTeam")),
            .funs = list(opp = ~ifelse(locationGame == "A", 
                                       .[locationGame == "H"],
                                       .[locationGame == "A"])))%>%
  mutate(orb_percent = orebTeam / (orebTeam + drebTeam_opp),
         drb_percent = drebTeam / (drebTeam + orebTeam_opp)
  )%>%
  ungroup()%>%
  arrange(yearSeason, dateGame, idGame)%>%
  #CALCULATE CUMULATIVE STATS AS THE YEAR GOES ON (BY TEAM)
  group_by(yearSeason,nameTeam)%>%
  mutate(possession_cum = cumsum(fgaTeam) + cumsum(tovTeam) + 0.4*cumsum(ftaTeam) - cumsum(orebTeam),
         offensive_efficiency_cum = 100*cumsum(ptsTeam)/possession_cum,
         ts_percent_cum = cumsum(ptsTeam)/ (2*(cumsum(fgaTeam)+ 0.44*cumsum(ftaTeam))),
         efg_percent_cum = (cumsum(fgmTeam) + 0.5*cumsum(fg3mTeam))/cumsum(fgaTeam),
         tov_percent_cum = cumsum(tovTeam) / (cumsum(fgaTeam)+0.44*cumsum(ftaTeam) + cumsum(tovTeam)),
         ft_rate_cum = cumsum(ftaTeam) / cumsum(fgaTeam),
         orb_percent_cum = cumsum(orebTeam) / (cumsum(orebTeam) + cumsum(drebTeam_opp)),
         drb_percent_cum = cumsum(drebTeam) / (cumsum(drebTeam) + cumsum(orebTeam_opp))
  )%>%
  group_by(yearSeason,nameTeam_opp)%>%
  mutate(possession_cum_defense = cumsum(fgaTeam) + cumsum(tovTeam) + 0.4*cumsum(ftaTeam) - cumsum(orebTeam),
         defensive_efficiency_cum = 100*cumsum(ptsTeam)/possession_cum_defense)%>%
  #Set opponent offensive efficiency to team defensive efficiency.
  group_by(yearSeason, idGame)%>%
  mutate(defensive_efficiency = ifelse(locationGame == "A", 
                                       offensive_efficiency[locationGame == "H"],
                                       offensive_efficiency[locationGame == "A"]),
         defensive_efficiency_cum = ifelse(locationGame == "A",
                                           defensive_efficiency_cum[locationGame == "H"],
                                           defensive_efficiency_cum[locationGame == "A"]
         ))%>%
  ungroup()%>%
  mutate(net_rating = offensive_efficiency - defensive_efficiency,
         net_rating_cum = offensive_efficiency_cum - defensive_efficiency_cum)%>%
  #What we want is the statistics ENTERING the game (prior to a bet being made).
  ###So, let's take the aggregat statistics up to the previous gaem
  group_by(yearSeason, nameTeam)%>%
  mutate_at(vars(contains("_cum")), .funs = list(lag = ~lag(.)))%>%
  #filter(yearSeason == 2020)%>%
  #filter(nameTeam == "Atlanta Hawks")%>%
  ungroup()

#Basic Possession Formula=0.96*[(Field Goal Attempts)+(Turnovers)+0.44*(Free Throw Attempts)-(Offensive Rebounds)]


#Save data ------------------------------------------------------------

save(sbr_wide, file = "Output/sbr_wide.Rda")
save(sbr_long, file = "Output/sbr_long.Rda") 
save(sbr_long_pin, file = "Output/sbr_long_pin.Rda")

save(logs_clean, file = "Intermediate/Game Logs/logs_clean.Rda")


#Checks to see how much data is missing --------------------------------

# sbr_wide %>%
#   select(dateGame, sbrTeam, ml_consensus:tot_BOL_odds)%>%
#   is.na() %>%
#   reshape2::melt() %>%
#   ggplot(aes(Var2, Var1, fill=value)) +
#   geom_raster() +
#   coord_flip() +
#   scale_y_continuous(NULL, expand = c(0, 0)) +
#   scale_fill_grey(name = "",
#                   labels = c("Present",
#                              "Missing")) +
#   xlab("Observation") +
#   theme(axis.text.y  = element_text(size = 4))
# 
# ggplotly(sbr_wide%>%
#            filter(is.na(ml_PIN))%>%
#            group_by(dateGame)%>%
#            summarise(n = n())%>%
#            ggplot(aes(x = dateGame,y = n))+
#            geom_col())
# #Looks like there is some data missing around December 2014 on the classic site that is on the non-classic site. Not sure what's going on..
# 
# 
# # How much do the lines differ? --------------------------------------
# sbr_long_pin%>%
#   filter(bet == "rl", locationGame == "H")%>%
#   group_by(idGame, sportsbook)%>%
#   summarise(line = mean(line))%>%
#   group_by(idGame)%>%
#   summarise(sd = sd(line))%>%
#   ggplot(aes(sd))+
#   geom_histogram(bins = 150)
# 
