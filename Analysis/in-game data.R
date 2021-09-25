### Set Environment -------------------------

# Clear the environment 
#rm(list=ls())

#Links
## https://dreamrs-vic.shinyapps.io/shinyWidgets/

# Set working directory
path <- "C:\\Users\\nated\\Documents\\Documents_NB\\Projects\\Betting\\SBR Workflow"
options(stringsAsFactors = F)
setwd(path)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               lubridate,
               dplyr,
               tidyr,
               scales,
               ggplot2,
               readxl,
               janitor,
               shiny,
               shinyWidgets,
               ggthemes,
               plotly,
               ggimage,
               magick,
               DT,
               nbastatR,
               reticulate
)

#Load data ------------------------------------------
source("Code/Load datasets.R")
#Prepare and clean historical Data for modeling
source("Code/regression prep.R")

time_margin_input<-
  read_excel("../Analysis/Input/time margin crosswalk.xlsx")

source("Code/Functions/plotting functions.R")

DISTINCT_COLORS<-c("#d4502d", "#7166d8","#5cc24d","#bf55c4","#9db935", "#d5408e","#4dc482","#d23f56","#459133", "#90539e","#caa637","#4f68ab","#dd872f", "#8796e1","#7b8929","#d088ca","#3b793c",
                   "#e37ba3", "#8fb869","#9c4564","#51c5b5","#a0522e", "#48b1da","#8a6b2b","#2b7f67","#d87a76", "#5aa97d","#da9667","#616c2c","#b7ae64")

#Download in-game data for 2019-2020 season --------

#choose relevant game_ids to download
# relevant_ids <- sched_2020%>%
#   mutate(month = month(dateGame))%>%
#   filter(dateGame<Sys.Date())
# 
# PlayFunction<-function(x){
#   df<-play_by_play(game_ids = x, nest_data = F, return_message = T)
#   Sys.sleep(1)
#   
#   return(df)
# }

#Get Data by Month
# for (i in unique(relevant_ids$month)){
#   print(paste0("Month: ", i))
#   
#   list<-
#     relevant_ids%>%
#       filter(month == i)%>%
#       distinct(idGame)%>%
#       pull(idGame)
#   
#   pbp <- lapply(list, PlayFunction)
#   pbp_df<-bind_rows(pbp)
#   
#   save(pbp_df, file = paste0("Intermediate/Play By Play/2019-20/Month",i,".Rda"))
# }

#pbp1 <-play_by_play(game_ids = test, nest_data = F, return_message = T)

#Load datasets-----------------------------------------------------------------

files<-list.files("Intermediate/Play By Play/2019-20")
pbp<-lapply(files, function(x) {
  load(paste0("Intermediate/Play By Play/2019-20/", x)) 
  return(pbp_df)
})%>%bind_rows()

clean<-
  pbp%>%
  filter(!is.na(slugScore))%>%
  #filter(idGame == 21900278)%>%
  select(idGame, minuteGame, slugScore, scoreAway, scoreHome)%>%
  gather(home, score, c(scoreAway,scoreHome))%>%
  group_by(idGame, minuteGame, home)%>%
  filter(score == max(score))%>%
  filter(!duplicated(score))%>%
  group_by(idGame, minuteGame)%>%
  mutate(slugScore = paste0(score[home == "scoreAway"]," - ",score[home == "scoreHome"]))%>%
  ungroup()%>%
  mutate(home = ifelse(grepl("Home", home),"Home","Away"),
         q = ifelse(minuteGame >48, "OT", paste0("Q",ceiling(minuteGame/12)))
         )%>%
  #Join schedule information
  left_join(sched_2020%>%select(idGame,home,dateGame,idTeam), by = c("idGame","home"))%>%
  #Join team abbreviation
  left_join(crosswalk%>%distinct(idTeam, abbrev), by = c("idTeam"))%>%
  #Join final spread for the game
  left_join(combined%>%select(idGame,idTeam, sbr_spread,plusminusTeam, logos_local), by = c("idGame", "idTeam"))%>%
  arrange(idGame,minuteGame)%>%
  group_by(idGame)%>%
  #Clean label for Facet wrap
  mutate(label = paste0(abbrev[home == "Away"][1], " @ ", abbrev[home=="Home"][1], " (",sbr_spread,")" ))%>%
  group_by(idGame, minuteGame)%>%
  #Calculate margin at every point in the game
  mutate(margin = ifelse(home == "Home", 
                         score[home == "Home"] - score[home == "Away"],
                         score[home == "Away"] - score[home == "Home"]
                         ),
         #Adding, since sbr_spread is in negatives
         dif_margin_spread = margin + sbr_spread
         )%>%
  group_by(idGame, home)%>%
  mutate(result = ifelse(margin[minuteGame == max(minuteGame)] > 0, "W", "L"),
         minute = floor(minuteGame))%>%
  ungroup()%>%
  select(dateGame,idGame, idTeam,abbrev, logos_local, label, home, sbr_spread, plusminusTeam, result, minuteGame, q, minute, everything())

ggplotly(clean%>%
           ggplot(aes(x = minuteGame, y = score, colour = abbrev, linetype = home))+
           geom_line(alpha = 0.5)+
           scale_x_continuous(breaks = seq(0, 48, 12))+
           facet_wrap(~label)
          )
#geom_bar(stat = "identity"))

#Version 1
clean%>%
  filter(minuteGame<=48)%>%
  #filter(result == "W")%>%
  ggplot(aes(x = minuteGame, y = margin))+
  geom_line(colour = "grey70",alpha = 0.1, aes(group = idGame))+
  geom_smooth(size = 1.1,span = .2, method = "loess", se = FALSE)+
  scale_x_continuous(breaks = seq(0, 48, 12))+
  lims( y = c(-30, 30))+
  geom_hline(yintercept  = 0, linetype = "dashed", size = 0.1, alpha = 0.9)+
  facet_wrap(~abbrev)+
  theme_bw()

#Version 2 (shows W vs L)
clean%>%
   #filter(abbrev == "HOU" | abbrev == "BOS")%>%
   filter(minuteGame<=48)%>%
   #filter(result == "W")%>%
   ggplot(aes(x = minuteGame, y = margin))+
   geom_line(alpha = 0.1, aes(group = idGame, colour = result))+
  #geom_smooth(colour = "grey70",size = 1.1, method = "loess", se = FALSE)+
   geom_smooth(size = 1.1, method = "loess", se = FALSE, span = .15, aes(colour = result))+
   scale_x_continuous(breaks = seq(0, 48, 12))+
   lims( y = c(-30, 30))+
   geom_hline(yintercept  = 0, linetype = "dashed", size = 0.1, alpha = 0.9)+
   facet_wrap(~abbrev)+
   theme_bw()


ggplotly(
  clean%>%
    filter(abbrev == "HOU")%>%
    filter(minuteGame<=48)%>%
    filter(result == "W")%>%
    #filter(month(dateGame) == 2)%>%
           ggplot()+
           geom_line(aes(x = minuteGame, y = margin, linetype = home, colour = as.character(idGame)),alpha = 0.1, show.legend = FALSE)+
           #geom_point(size = 1, alpha = 0.5)+
           scale_x_continuous(breaks = c(seq(0, 48, 12), 55,60, 67,72))+
           geom_hline(yintercept  = 0, linetype = "dashed", size = 0.1, alpha = 0.9)+
          #stat_smooth(aes(x = minuteGame, y = margin, colour = result),method = loess,alpha=1.5, se=FALSE, span = 0.000075,show.legend=FALSE)+
           facet_wrap(~abbrev)+
            theme_bw()
)

  
  ggplotly(
    clean%>%
    filter(minute<=48)%>%
    group_by(abbrev, minute)%>%
    #group_by(abbrev, minute, result)%>%
    summarise_at(vars(score, margin, dif_margin_spread), ~mean(.,na.rm=T))%>%
    ggplot(aes(x = minute, y = margin, colour = abbrev))+
    geom_line()+
    geom_point()+
      theme_bw()+
    scale_x_continuous(breaks = c(seq(0, 48, 12), 55,60, 67,72))
      
)
  
  ggplotly(
    clean%>%
      filter(minute<=48)%>%
      group_by(abbrev, minute, result)%>%
      summarise_at(vars(score, margin, dif_margin_spread), ~mean(.,na.rm=T))%>%
      ggplot(aes(x = minute, y = margin, colour = abbrev, linetype = result))+
      geom_line()+
      geom_point()+
      theme_bw()+
      scale_x_continuous(breaks = c(seq(0, 48, 12), 55,60, 67,72))
    
  )
  
  
  ggplotly(
    clean%>%
      filter(minute<=48)%>%
      ggplot(aes(x = minute, y = margin, colour = abbrev))+
      geom_boxplot()+
      theme_bw()+
      scale_x_continuous(breaks = c(seq(0, 48, 12), 55,60, 67,72))
    
  )
  
  ggplotly(
    clean%>%
      filter(minute >= 12, minute<=48)%>%
      ggplot(aes(x = as.factor(abbrev), y = dif_margin_spread))+
      geom_boxplot()+
      theme_bw() +
      coord_flip()+
      geom_hline(yintercept  = 0, linetype = "dashed", size = 0.1, alpha = 0.9)
      
  )
  
  
  
  


#What team spends the most time behind a game before coming back?  -------------------------------

#What team has the closest margin away from spread?
  
#What team has the most extreme fluctuatoins in wins or losses
  
fluct<-
    clean%>%
    filter(minute>=5)%>%
    group_by(abbrev, idGame)%>%
    summarise(max = max (dif_margin_spread),
              min = min (dif_margin_spread),
              median = median(dif_margin_spread),
              mean = mean(dif_margin_spread),
              sd = sd(dif_margin_spread))%>%
    group_by(abbrev)%>%
    summarise_at(vars(c(max, min, median,mean, sd)), ~mean(.,na.rm=T))
    
    
  
#What does the fluctuatoin look like at each spread
clean%>%
  #filter(abbrev == "HOU")%>%
  filter(minuteGame>=5)%>%
  group_by(abbrev,sbr_spread, idGame)%>%
  summarise(max = max (dif_margin_spread),
            min = min (dif_margin_spread))%>%
  gather(var, val, max:min)%>%
  ggplot(aes(x = as.factor(-sbr_spread), y = val, colour = var))+
  coord_flip()+
  #geom_violin(alpha = 0.5)
  geom_jitter(alpha = 0.2, width = 0.1)

#What percentage of games that the rockets have been up 6 have they won? 
#What Percentae of games where the rockets have been 10 away from the original spread in the 2nd quarter on have they covered in the end?
## Is it significantly different for home vs away?
t<-clean%>%
  filter(minuteGame>=12)%>%
  group_by(abbrev, result, idGame)%>%
  summarise(min = min(margin),
            max = max(margin),
            final_margin = margin[minuteGame == max(minuteGame)])%>%
  group_by(abbrev)%>%
  summarise(games = n_distinct(idGame),
            less_six_w = sum(min<=-6 & result == "W"),
            less_six_avg_margin = mean(final_margin[min<=-6]),
            less_six_med_margin = median(final_margin[min<=-6]),
            more_six_l = sum(max>=6 & result == "L"),
            less_ten_w = sum(min<=-10 & result == "W"),
            more_ten_l = sum(max>=10 & result == "L")
            )%>%
  gather(var, val, less_six_w:length(.))%>%
  mutate(perc_win = val/games)



TestFunction<- function(df, target){
  
  col<- ifelse(target<0, 
               paste0("down_", gsub("-","", target), "_"),
               paste0("up_", gsub("-","", target),"_"))
  
  df<-
    df%>%
    {if (target<0) filter(., min<=target) else filter(., max>=target)} %>%
    summarise(margin_target = target,
              games = n(),
              w = sum(result == "W",na.rm=T),
              w_avg = mean(plusminusTeam[result == "W"],na.rm=T),
              w_med = median(plusminusTeam[result == "W"],na.rm=T),
              l = sum(result == "L", na.rm=T),
              l_avg = mean(plusminusTeam[result == "L"],na.rm=T),
              l_med = median(plusminusTeam[result == "L"],na.rm=T),
              over = sum(over_under == "Over"),
              under = sum(over_under == "Under")
              )
              
}

test<-
  clean%>%
  left_join(combined%>%select(abbrev, idGame,dif_spread,plusminusTeam))%>%
  mutate(dif_spread = -dif_spread,
         plusminusTeam = -plusminusTeam,
         over_under = ifelse( dif_spread>0, "Over", "Under"))%>%
  #distinct(slugScore, abbrev, idGame, .keep_all=TRUE)%>%
  filter(minuteGame<=12)%>%
  #TEMP: LOOK AT JUST THE FIRST QUARTER
  #filter(minuteGame<=12)%>%
  group_by(dateGame,abbrev, result, idGame)%>%
  mutate(lead_changes = ifelse(lag(sign(margin)) != 0, 
                               lag(sign(margin)) == -sign(margin),
                               lag(sign(margin), n = 2) == -sign(margin)),
         ties = margin == 0)%>%
  summarise(min = min(margin),
            max = max(margin),
            range = max-min,
            sd = sd(margin),
            lead_changes = sum(lead_changes, na.rm=T),
            ties = sum(ties, na.rm=T),
            over_under = over_under[1],
            plusminusTeam = plusminusTeam[1]
            )%>%
  group_by(abbrev)%>%
  lapply(c(seq(-25, -6, by = 1), seq(6, 25, by = 1)), 
         TestFunction, df = .)%>%
  bind_rows()

# W vs Lose for different amounts Up/Down by
test%>%
  select(abbrev, margin_target, w, l)%>%
  #filter(abbrev == "BOS")%>%
  #filter(margin_target>0)%>%
  gather(var, val, 3:4)%>%
  mutate(val = ifelse(margin_target<0, -val, val),
         margin_target = abs(margin_target))%>%
  ggplot(aes(x = margin_target, y = val, fill = var))+
  geom_bar(stat = "identity")+
  #stat_smooth(geom = 'area', method = 'loess', span = 1/3,alpha = 1/2) +
  labs(title  = "Outcomes when teams are Up/Down Big", x = "Points Down/Up By", y = "Games Won/Loss when \nDown By (Left) or Up By (Right) Points")+
  geom_hline(yintercept  = 0, linetype = "dashed", size = 0.1, alpha = 0.9)+
  facet_wrap(~abbrev)+
  scale_y_continuous(labels=abs)+
  coord_flip()+
  theme_bw()
  
# Over vs Under for different amounts Up/Down by
test%>%
  select(abbrev, margin_target, over, under)%>%
  #filter(abbrev == "BOS")%>%
  #filter(margin_target>0)%>%
  gather(var, val, 3:4)%>%
  mutate(val = ifelse(margin_target<0, -val, val),
         margin_target = abs(margin_target))%>%
  ggplot(aes(x = margin_target, y = val, fill = var))+
  geom_bar(stat = "identity")+
  #stat_smooth(geom = 'area', method = 'loess', span = 1/3,alpha = 1/2) +
  labs(title  = "Over/Under when teams are Up/Down Big", x = "Points Down/Up By", y = "Over/Under when \nDown By (Left) or Up By (Right) Points")+
  geom_hline(yintercept  = 0, linetype = "dashed", size = 0.1, alpha = 0.9)+
  facet_wrap(~abbrev)+
  scale_y_continuous(labels=abs)+
  coord_flip()+
  theme_bw()



test%>%
  select(abbrev, margin_target, w, l)%>%
  filter(margin_target == 10)%>%
  gather(var, val, 3:4)%>%
  ggplot(aes(x = abbrev, y = val, fill = var))+
  geom_bar(stat = "identity")+
  #stat_smooth(geom = 'area', method = 'loess', span = 1/3,alpha = 1/2) +
  labs(title  = "Outcomes when teams are Up 10", x="",y= "Games Won/Loss when Up By 10")+
  coord_flip()+
  theme_bw()

#what do the average margins of victories look like at different deficits/leads
test%>%
  select(abbrev, margin_target, w,l, w_avg, l_avg)%>% 
  filter(abbrev == "BOS")%>%
  gather(var, avg_margin, 5:6)%>%
  mutate(games = ifelse(var == "w_avg", w, l),
         win = ifelse(var == "w_avg", "W", "L"))%>%
  ggplot(aes(x = avg_margin, y = margin_target, size = games/10, colour = win))+
    geom_point(alpha = 0.1)+
  geom_hline(yintercept  = 0, linetype = "dashed", size = 0.1, alpha = 0.9)+
  geom_vline(xintercept  = 0, linetype = "dashed", size = 0.1, alpha = 0.9)+
  facet_wrap(~abbrev)+
  theme_bw()
  


#gRAPH VARIANCE of margin and results over time
clean%>%
  left_join(combined%>%select(abbrev, idGame,dif_spread,plusminusTeam))%>%
  mutate(dif_spread = -dif_spread,
         plusminusTeam = -plusminusTeam,
         over_under = ifelse( dif_spread>0, "Over", "Under"))%>%
  filter(minuteGame>=6)%>%
  group_by(abbrev, result, idGame)%>%
  summarise(min = min(margin),
            max = max(margin),
            range = max-min,
            sd = sd(margin),
            over_under = over_under[1],
            plusminusTeam = plusminusTeam[1]
  )%>%
  ggplot(aes(x = abbrev, y = sd))+
  geom_boxplot()+
  geom_point(aes(colour = result), alpha = 0.2)+
  coord_flip()+
  theme_bw()


#Can we categorize (classify) losses or wins solely based on point differential as game goes on:
### For example: i) late came come back, ii) very close all game and team pulls away in end, iii) 2nd quarter flip the switch


#How much time in each game (and throughout the year) do teams spend up or down a certain amount? Let's find out. -----------
margin_time<-
  clean%>%
  #filter(idGame == 21900251)%>%
  group_by(idGame, abbrev)%>%
  mutate(time_lag = ifelse(is.na(lag(minuteGame)), 0, lag(minuteGame)))%>%
  group_by(dateGame, idGame, abbrev, logos_local, margin, result, sbr_spread, plusminusTeam )%>%
  summarise(time = sum(minuteGame - time_lag,na.rm=T))%>%
  ungroup()%>%
  left_join(time_margin_input)%>%
  group_by(dateGame,idGame, abbrev, logos_local, result, sbr_spread, plusminusTeam, margin_label )%>%
  summarise(time = sum(time, na.rm=T))%>%
  ungroup()%>%
  mutate(margin_label = fct_relevel(margin_label, unique(time_margin_input$margin_label)))


up_down<-margin_time %>%
  #group_by(abbrev, result)%>%
  group_by(abbrev)%>%
  mutate(n_games = n_distinct(idGame))%>%
  #group_by(abbrev, logos_local, margin_label, result)%>%
  group_by(abbrev, logos_local, margin_label)%>%
  #filter(result == "W")%>%
  summarise(time = sum(time) / mean(n_games))

  up_down %>%
  ggplot()+
  geom_image(aes(x = margin_label, y = time, image = logos_local), 
             position = position_jitter(width = 0.25, height = 0),  
             size = 0.03,
             image_fun = function(.) TransparentImage(., 0.5))+
  labs(x = "", y = "Average Minutes per Game", title = "How much are teams Up/Down During a Game?")+
    theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #coord_flip()
  #facet_wrap(~result)

  ggplot(up_down, aes(x = time, y = abbrev, fill = margin_label))+
    geom_col(position = "dodge")+
    scale_fill_manual(values = DISTINCT_COLORS)+
    theme_minimal()+
    coord_flip()


#If the rockets are -9 and they were up by 9+ points 45% of the time in the first 3 quarters, how often do they win?


ggplotly(
  clean%>%
  filter(abbrev == "HOU")%>%
  filter(minuteGame %in% c(12,24,36, 48))%>%
  #filter(result == "W")%>%
  ggplot(aes(x = sbr_spread, y = dif_margin_spread))+
  geom_point(size = 2, aes(colour = q, label = q))+
  geom_hline(yintercept = 0, linetype = "dashed"))
  geom_line(colour = "grey60",alpha = 0.1, aes(group = idGame))+
  #geom_smooth(colour = "grey70",size = 1.1, method = "loess", se = FALSE)+
  geom_smooth(size = 1.1, method = "loess", se = FALSE, span = .5, aes(colour = as.character(sbr_spread)))+
  scale_x_continuous(breaks = seq(0, 48, 12))+
  lims( y = c(-30, 30))+
  geom_hline(yintercept  = 0, linetype = "dashed", size = 0.1, alpha = 0.9)+
  facet_wrap(~abbrev)+
  theme_bw()
)

