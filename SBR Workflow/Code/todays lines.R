#Predict future games -------------------------------------
GetTodaysPicks<- function(d_input = "Today"){
#Clean future schedule file
  
  d_input<-ifelse(d_input == "Today", 
                  as.character(Sys.Date()), 
                  d_input)
  
  SEARCH_DATE<-gsub("-","",as.character(d_input))
  
  today_sbr<-DoAnalysis(SEARCH_DATE)
  today_sbr<-today_sbr%>%
    mutate(dateGame = as.Date(key, format = "%Y%m%d"),
           sbrTeam = team,
           #MONEYLINE
           consensus_ml = as.numeric(ml_consensus)/100,
           ml_PIN = as.numeric(ml_PIN),
           ratio_ml = ifelse(ml_PIN<0, -100/ml_PIN, ml_PIN/100),
           #SPREAD
           consensus_rl = as.numeric(rl_consensus)/100,
           rl_PIN_odds = as.numeric(rl_PIN_odds),
           ratio_rl = ifelse(rl_PIN_odds<0, -100/rl_PIN_odds, rl_PIN_odds/100),
           sbr_spread = as.numeric(rl_PIN_line),
           #TOTAL
           consensus_to = as.numeric(tot_consensus)/100,
           tot_PIN_odds = as.numeric(tot_PIN_odds),
           ratio_to = ifelse(tot_PIN_odds<0, -100/tot_PIN_odds, tot_PIN_odds/100),
           sbr_total = as.numeric(tot_PIN_line),
           
    )%>%
    select(dateGame, sbrTeam, contains("ratio"), consensus_to, consensus_rl, consensus_ml, sbr_spread, sbr_total)
  

  
  todays_info<-combined%>%
    select(idTeam, yearSeason, dateGame, idGame, home, numberGameTeamSeason, contains("_cum"),-contains("_lag"), 
           -net_rating_cum, -ts_percent_cum, -contains("possession"))%>%
    rename_at(vars(contains("_cum")), .funs = ~paste0(., "_lag"))%>%
    group_by(idTeam)%>%
    #Filter out to last game for each time prior to this matchup
    filter(dateGame < d_input)%>%
    filter(dateGame == max(dateGame))%>%
    select(-dateGame, -idGame, -home, -numberGameTeamSeason)%>%
    ungroup()%>%
    #Take today's slate of games
    right_join(sched_2020%>%
                 filter(dateGame == d_input)
    )%>%
    group_by(idGame)%>%
    mutate_at(vars(-idTeam, -yearSeason, -idGame, -dateGame, -home),
              .funs = list(opp = ~if_else(home == "Away", 
                                         .[home == "Home"],
                                         .[home == "Away"])))%>%
    left_join(crosswalk%>%distinct(idTeam, .keep_all=T))%>%
    #Join sbr data from today
    left_join (today_sbr)%>%
    select(-nameTeam)%>%
    select(sbrTeam, everything())%>%
    ungroup()
  
return(todays_info)
}


