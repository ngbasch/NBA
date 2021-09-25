###############################################################################
###
###   PURPOSE: 
###
###############################################################################

#Clean data ------------------------------------------------------------------------------
combined<-
  logs_clean%>%
  left_join(sbr_long_pin%>%
              filter(bet == "ml")%>%
              select(idGame, idTeam,"consensus_ml" = consensus, "ratio_ml" = ratio), by = c("idGame", "idTeam"))%>%
  left_join(sbr_long_pin%>%
              filter(bet == "rl")%>%
              select(idGame, idTeam, "consensus_rl" = consensus, "sbr_spread" = line, "ratio_rl" = ratio), by = c("idGame", "idTeam"))%>%
  left_join(sbr_long_pin%>%
              filter(bet == "to")%>%
              select(idGame, idTeam, "sbr_total" = line, "consensus_to" = consensus, "ratio_to" = ratio, ptsTotal), by = c("idGame", "idTeam"))%>%
  #Training data from 2015 to 2018
  filter(yearSeason>=2015, typeSeason == "Regular Season")%>%
  group_by(idGame)%>%
  mutate(favs_ml = if_else(ratio_ml == max(ratio_ml), "Underdog", "Favorite"),
         favs_rl = if_else(sbr_spread > 0, "Underdog", "Favorite"),
         plusminusTeam = -plusminusTeam)%>%
  ungroup()%>%
  mutate(VegasCorrectML = if_else((favs_ml == "Favorite" & isWin) | favs_ml == "Underdog" & isWin == FALSE, "Yes", "No")%>%as.factor(),
         VegasCorrectRL = if_else(plusminusTeam<=sbr_spread, "Yes", "No")%>%as.factor(),
         VegasCorrectTO = if_else((ptsTotal<sbr_total & home == 1) | (ptsTotal>sbr_total & home == 0), "Yes", "No")%>%as.factor(),
         isB2B= as.factor(if_else(isB2B, "Yes", "No")),
         win = as.factor(outcomeGame),
         home = if_else(home==1, "Home", "Away")%>%as.factor(),
         dif_total = ptsTotal - sbr_total,
         dif_spread = plusminusTeam - sbr_spread
           
  )%>%
  left_join(crosswalk)
#Remove where lag is missing (first game of season)

# test<-combined%>%filter(dateGame == "2019-12-16")%>%select(nameTeam, home,sbr_spread, plusminusTeam, sbr_total, ptsTotal,favs_rl, 
#                                                            VegasCorrectML, VegasCorrectRL, VegasCorrectTO, dif_total, dif_spread,
#                                                            contains("ratio"), contains ("consensus"))

reg_data<-
  combined%>%
  group_by(idGame)%>%
  filter(!is.na(net_rating_cum_lag))%>%
  filter(n() == 2)%>%
  select(idTeam, nameTeam, yearSeason, dateGame, idGame,isB2B, home, numberGameTeamSeason, 
         contains("_lag"), -ts_percent_cum_lag, -contains("possession"), 
         plusminusTeam, ptsTotal, win, sbr_total, sbr_spread, dif_total, dif_spread,
         contains("consensus"), contains("ratio"), contains("VegasCorrect"))%>%
  #Choose which statistics get put as "opponent statistics
  mutate_at(vars(contains("_lag"), isB2B, -contains("possession")),
            .funs = list(opp = ~if_else(home == "Away", 
                                       .[home == "Home"],
                                       .[home == "Away"])))

