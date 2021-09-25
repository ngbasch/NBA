
library(nbastatR)
#Get team data for each game using nbastatR

df_dict_nba_teams<-nba_teams()
teams_tables(teams = "Brooklyn Nets",
             #all_active_teams = T,
             seasons = 2019, 
             tables = "splits" , 
             modes = "Totals",
             measures = "Advanced")

??get_team_table_data

#This isn't working right now... need to figure out later.
teams_tables(teams = c("Brooklyn Nets"),
             seasons = 2017, tables = c("splits"), 
             measures = "Base", 
             modes = c("PerGame", "Totals"))
