library(ballr)
library(nbastatR)
library(shiny)
library(ggplot2)
library(hexbin)
library(dplyr)
library(httr)
library(jsonlite)
library(rjson)
library(plotly)
#https://toddwschneider.com/posts/ballr-interactive-nba-shot-charts-with-r-and-shiny/

t<-play_by_play(game_ids = c(21700002), nest_data = F, return_message = T)

#Think about using synergy data at some point:
#synergy = synergy(seasons = 2018)


test<-
  t%>%
  filter(!is.na(slugScore))%>%
  select(minuteGame, slugScore, scoreAway, scoreHome)%>%
  gather(team, score, c(scoreAway,scoreHome))%>%
  group_by(minuteGame, team)%>%
  filter(score == max(score))%>%
  filter(!duplicated(score))%>%
  ungroup()%>%
  arrange(minuteGame)
ggplotly(test%>%
    ggplot(aes(x = minuteGame, y = score, colour = team))+
      geom_line())
      #geom_bar(stat = "identity"))
  

#Run some nbastatR functions that look useful
test<-NBAPerGameAdvStatistics(season = 2018)
test2<-NBAPlayerPerGameStats("/players/d/davisan02.html")

#run this guy's github which allow me to make those kirk goldsberry figures (not working)
shiny::runGitHub("ballr", "toddwschneider")

#Traditional box score from NBA.com (not working [NOW it is!!!])
boxTraditional <- box_scores(game_ids = c(21700002, 21700003), 
                             box_score_types = c("Traditional", "Advanced", "Scoring", "Misc", "Usage", "Four Factors", "Tracking"), 
                             result_types = c("player", "team"), 
                             join_data = TRUE, 
                             assign_to_environment = TRUE, 
                             return_message = TRUE)


test<- game_logs(game_ids = 21900862)

#Win Probabilities for a given game
t<-win_probability(game_ids = c(21700002, 21700003,20200211),
                nest_data = F)

t%>%
  mutate(likely = ifelse(pctWinProbAway>=0.5, "A", "H"))%>%
ggplot( aes(x = minuteGame, pctWinProbAway))+
  #geom_line(aes(colour = likely))+
  geom_line(aes(color = as.character(idGame)), alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 48, 12))+
  geom_hline(yintercept  = 0.5, linetype = "dashed")


