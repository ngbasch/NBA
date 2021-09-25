library(nbastatR)

sched_2020<- current_schedule()%>%
  filter(dateGame >= "2019-10-22")%>%
  select(dateGame,idGame, idTeamHome, idTeamAway)%>%
  gather(var, idTeam, idTeamHome:idTeamAway)%>%
  mutate(home = ifelse(var == "idTeamHome", "Home", "Away")%>%as.factor())%>%
  group_by(idTeam)%>%
  arrange(dateGame)%>%
  mutate(days_between = difftime(dateGame, lag(dateGame)),
         isB2B = ifelse(is.na(days_between)| days_between != 1, "No", "Yes")%>%as.factor(),
         numberGameTeamSeason = 1:n())%>%
  ungroup()%>%
  select(-var, -days_between)%>%
  arrange(dateGame, idGame)
  


save(sched_2020, file = "Intermediate/schedule_2020.Rda")
