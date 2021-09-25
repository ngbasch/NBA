library(nbastatR)
library(shiny)
library(ggplot2)
library(hexbin)
library(dplyr)
library(httr)
library(jsonlite)
library(rjson)
library(plotly)



logs<-game_logs(seasons = 2020, league = "NBA", result_types = "team", nest_data = T)
t<-play_by_play(game_ids = c(21900305), nest_data = F, return_message = T)

ScoreTracker<- function(gameid){
  t<-play_by_play(game_ids = c(gameid), nest_data = F, return_message = T)
  
  title<-
    logs%>%filter(idGame == gameid, locationGame == "A")%>%pull(slugMatchup)
  date<-
    logs%>%filter(idGame == gameid, locationGame == "A")%>%pull(dateGame)
  
  
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
             ggplot(aes(x = minuteGame, y = score, fill = team))+
             geom_bar(stat = "identity")+
             scale_x_continuous(breaks = seq(0, 48, 12))+
             ggtitle(paste0(title,": ", date))+
             theme_bw()
  )
}

ScoreTracker(21900305)


#Look how shapes change between teams. 
#Try to take into account garbage time potentially?
### Teams is up by a solid amount of points and almost no starters are in the game. How does this team play into the game?
#Compare to spreads
