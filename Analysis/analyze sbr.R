###############################################################################
###
###   PURPOSE: 
###
###############################################################################

### Pull recent SBR Data -----------------------

#source("Code/update and clean sbr.R")

teamcrosswalk<-read_excel("../Input/team crosswalk.xlsx")

### Set Environment -------------------------

# Clear the environment 
rm(list=ls())

# Set working directory
path <- "Documents/Github/NBA/SBR Workflow"
options(stringsAsFactors = F)
setwd(path)

# Load packages
library(tidyverse)
library(janitor)
library(plotly)
library(tidylog)
library(forcats)
library(ggimage)
library(readxl)
library(vip)

# Set constants
DISTINCT_COLORS<-c("#d4502d", "#7166d8","#5cc24d","#bf55c4","#9db935", "#d5408e","#4dc482","#d23f56","#459133", "#90539e","#caa637","#4f68ab","#dd872f", "#8796e1","#7b8929","#d088ca","#3b793c",
   "#e37ba3", "#8fb869","#9c4564","#51c5b5","#a0522e", "#48b1da","#8a6b2b","#2b7f67","#d87a76", "#5aa97d","#da9667","#616c2c","#b7ae64")

### Import Data -------------------------------------------------------------------------------------------------

#Load datasets/
source("Code/Load datasets.R")
source("Code/Functions/plotting functions.R")


#Prepare and clean historical Data for modeling
source("Code/regression prep.R")


### Visualize data ------------------------------------------------------------------------------------------------------

#Team summaries -----------------------------------------------------------------------------------------------
# How has vegas adapted it's lines and spreads over the 2020 season?
### Are they favoring teams they didn't before?

#Moneyline

sbr_long_pin%>%
  filter(bet == "ml", 
         yearSeason == 2020
         )%>%
  ggplot(aes(x = dateGame, y = ratio, colour = outcomeGame))+
    geom_point(size = 1.2)+
    scale_y_continuous(breaks = seq(0, 100, 2))+
    geom_hline(yintercept = 1)+
    theme_bw()+
  #  facet_wrap(~sbrTeam, scales = "free")
    facet_wrap(~sbrTeam)
  

#How does vegas do over time with favorites?
test<-sbr_long_pin%>%
  filter(bet == "ml",typeSeason == "Regular Season")%>%
  group_by(idGame)%>%
  mutate(favs = ifelse(ratio == max(ratio), "Underdog", "Favorite"))%>%
  ungroup()%>%
  mutate(w_l = ifelse(outcomeGame == "W", 1, 0))%>%
  group_by(yearSeason,dateGame, favs)%>%
  summarise(wins = sum(w_l),
            payout = sum(ratio*w_l))%>%
  group_by(yearSeason, favs)%>%
  mutate(n = 1:n())%>%
  group_by(dateGame)%>%
  mutate(payout_total = ifelse(favs == "Favorite", 
                               payout-wins[favs =="Underdog"], 
                               payout - wins[favs == "Favorite"]))%>%
  group_by(yearSeason,favs)%>%
  mutate(cum_payout = cumsum(payout_total))
  
test%>%
  #filter(yearSeason == 2020)%>%
  ggplot(aes(x = n, y = wins, fill = favs))+
    geom_bar(stat = "identity")+
    scale_y_continuous(breaks = seq(0, 100, 1))+
    theme_bw()+
    facet_wrap(~yearSeason)
    NULL

#Bet all favorites vs all underdogs
test%>%
  filter(yearSeason ==2020)%>%
    ggplot()+
      geom_bar(stat = "identity",
               position = "dodge",aes(x = dateGame, y = payout_total, fill = favs),alpha = 0.2)+
      geom_line(size = 1,aes(x = dateGame, y = cum_payout, colour = favs))+
      geom_text(nudge_y = ifelse(test$favs == "Favorite", 5, -5),
                aes(x = dateGame, y = cum_payout, colour = favs, 
                    label=ifelse(dateGame == "2019-12-23", favs,"")),
                size = 4)+
      #scale_y_continuous(breaks = seq(-100, 100, 1))+
      theme_bw()+
      labs(x = "", y = "Payout ($)", title = "What would happen if you bet $1\non each favorite (or each underdog) every night")+
      geom_hline(yintercept = 0, linetype = "dashed")+
      #ylim(-10,10)+
      scale_y_continuous(breaks = seq(-100, 100, 10))+
      #facet_wrap(~yearSeason)+
      guides(color = F, fill = F)
      NULL
#Version 2:

  
#RUN REGRESSION with time effects of betting favorite or underdog and how this changes over time

  



#How they do over time with underdogs

# Do team summaries of Totals


#Get a sense of the correlation between the amount bet in the ML and the weighted amount bet in the spread

#Which games were tossups according to ML?
ml_tossups<-sbr_long_pin%>%
  filter(bet == "ml", typeSeason == "Regular Season")%>%
  select(idGame,locationGame, ratio)%>%
  spread(locationGame, ratio)%>%
  filter(A == H)


ml_spread_compare<-sbr_long_pin%>%
  filter(bet == "ml" | bet == "rl", typeSeason == "Regular Season")%>%
  group_by(idGame)%>%
  mutate(favs =  ifelse(bet == "ml" & idGame %in% ml_tossups$idGame, "Pickem",
                  ifelse(ratio == max(ratio) & bet == "ml", "Underdog", 
                       ifelse(ratio != max(ratio) &  bet == "ml", "Favorite",
                                  ifelse(line<0, "Favorite",
                                         ifelse(line>0, "Underdog", 
                                                "Pickem"))))))%>%
  group_by(idGame, locationGame)%>%
  mutate(n = 1:n(),
         ratio_ml = ratio[n == 1],
         favs_ml = favs[n ==1])%>%
  filter(bet != "ml")
         
disagreement<-
  ml_spread_compare%>%
  filter(favs!=favs_ml)

#Note that underdogs are on top and favorites are below.
ggplotly(
  ml_spread_compare%>%
  filter(!(idGame%in%disagreement$idGame))%>%
  filter(favs!= "Pickem")%>%
  filter(yearSeason == 2020)%>%
  #ggplot(aes(x = ratio_ml, y = line, color = ratio, shape = outcomeGame ))+
  ggplot(aes(x = ratio_ml, y = line, color = outcomeGame))+
  #ggplot(aes(x = ratio_ml, y = ratio, color = line, shape = outcomeGame ))+
    geom_point(alpha = .2)+
    facet_wrap(~yearSeason)+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept  = 1)+
    scale_y_continuous(breaks = seq(-100, 100, 0.5))+
    theme_bw()
   # scale_color_viridis_c()
)

#See all years in one
  ml_spread_compare%>%
    filter(!(idGame%in%disagreement$idGame))%>%
    filter(favs!= "Pickem")%>%
    filter(ratio_ml <=20)%>%
    #ggplot(aes(x = ratio_ml, y = line, color = ratio, shape = outcomeGame ))+
    ggplot()+
    #geom_rect(aes(xmin=0,xmax=1,ymin=-Inf,ymax=0),alpha=0.002,fill="orange")+
    #geom_rect(aes(xmin=1,xmax=Inf,ymin=0,ymax=Inf),alpha=0.002,fill="purple")+
    #ggplot(aes(x = ratio_ml, y = ratio, color = line, shape = outcomeGame ))+
    geom_point(aes(x = ratio_ml, y = line, color = outcomeGame), alpha = .01)+
    geom_hline(yintercept = 0, linetype = "dotted")+
    geom_vline(xintercept  = 1, linetype = "dotted")+
    scale_y_continuous(breaks = seq(-100, 100, 2))+
    scale_x_continuous(breaks = c(0, 1, seq(5, 100, 5)))+
    # geom_smooth(method = "nls",
    #             formula = y ~ p1 + p2*log(x),
    #             start = list(a = 0, b = 5),
    #             se = FALSE,
    #             control = list(maxiter = 100),
    #             colour = "black",
    #             linetype = "dashed",
    #             alpha = 0.3)+
    labs(x = "Payout if $1 Bet on Moneyline", y = "Spread", title = "Spread vs MoneyLine (2010 - 2020)" )+
    theme_bw()
  # scale_color_viridis_c()


#What does the distribution of ML ratios look like for favorites?
  ml_spread_compare%>%
    filter(!(idGame%in%disagreement$idGame))%>%
    filter(favs!= "Pickem")%>%
    filter(ratio_ml <=20, yearSeason == 2020)%>%
    filter(ratio_ml<1)%>%
    #mutate(american_ratio = ifelse(ratio_ml<1, 100/ratio_ml, -100*ratio_ml))%>%
    ggplot(aes(x = ratio_ml))+
    geom_density(aes(y = (..count..)/sum(..count..)),bins = 10, alpha = 0.5)+
    geom_vline(xintercept = 1, linetype = "dashed")+
    scale_x_continuous(breaks = seq(0, 100, 0.1))+
    theme_bw()

  ml_spread_compare%>%
    filter(!(idGame%in%disagreement$idGame))%>%
    filter(favs!= "Pickem")%>%
    filter(yearSeason == 2020)%>%
    #ggplot(aes(x = ratio_ml, y = line, color = ratio, shape = outcomeGame ))+
    ggplot(aes(x = ratio_ml, y = line, color = outcomeGame))+
    #ggplot(aes(x = ratio_ml, y = ratio, color = line, shape = outcomeGame ))+
    geom_point(alpha = .2)+
    facet_wrap(~sbrTeam)+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept  = 1)+
    #scale_y_continuous(breaks = seq(-100, 100, 0.5))+
    theme_bw()
  # scale_color_viridis_c()





#Top upsets (spread)-------------------------------------------------------------------------------------------
upsets<-sbr_long_pin%>%
  filter(bet == "rl")%>%
  mutate(difference = abs(plusminusTeam - line),
         label = paste0(sbrTeam, " W vs ", opp_team, " (", as.character(dateGame),")"),
         label = fct_reorder(label, line)
         )%>%
  filter(yearSeason==2020, line>0, outcomeGame == "W")%>%
  arrange(-line)%>%
  top_n(25)

ggplot()+
  geom_point(data = upsets, aes(x = label, y = -line, color = ratio))+
  geom_point(data = upsets, aes(x = label, y = -plusminusTeam), color = "red")+
  geom_hline(yintercept = 0)+
  coord_flip()+
  labs(x = "", y = "")+
  theme_bw()

#Odds versus plus minus

sbr_long_pin%>%
  filter(bet == "ml", outcomeGame == "W",sbrTeam == betting_on)%>%
  mutate(label = paste0(sbrTeam, " W vs ", opp_team, " (", as.character(dateGame),")"),
         label = fct_reorder(label, ratio),
         plusminusTeam = -plusminusTeam)%>%
  arrange(-ratio)%>%
  filter(yearSeason == 2020)%>%
  top_n(25)%>%
  gather(var, val, c(plusminusTeam, ratio))%>%
  ggplot(data = .)+
    geom_col(aes(x = label, y = val))+
    facet_wrap(~var, scales = "free")+
    coord_flip()


#Totals

totals_upsets<-sbr_long_pin%>%
  filter(bet == "to")%>%
  mutate(label = paste0(sbrTeam, " W vs ", opp_team, " (", as.character(dateGame),")"),
         difference = abs(ptsTotal-line),
         label = fct_reorder(label, difference)
  )%>%
  arrange(-difference)%>%
  filter(!duplicated(idGame), yearSeason == 2020)%>%
  top_n(25)

ggplot()+
  geom_point(data = totals_upsets, aes(x = label, y = line, color = ratio))+
  geom_point(data = totals_upsets, aes(x = label, y = ptsTotal), color = "red")+
  coord_flip()+
  labs(x = "", y = "")+
  scale_color_viridis_c()+
  theme_bw()


#Totals results 2019 (over vs under based on where line is set)
combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  ggplot(aes(x = sbr_total, fill = result, colour = result))+
  geom_density(alpha = 0.2)+
  theme_bw()

combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  ggplot(aes(y = sbr_total, x = result, fill = result))+
  geom_violin(width = 0.5)+
  geom_boxplot(width = 0.1)+
  theme_bw()


#geom_point()

combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
    ggplot()+
    geom_boxplot(notch = FALSE, aes(x = dateGame, y = sbr_total), fill = "grey", alpha =0.05)+
   geom_point(aes(x = dateGame, y = sbr_total, colour = result), alpha = 0.15)+
  labs(x = "", y = "Vegas Total") + 
    #stat_smooth(geom = "line")+
    theme_bw()

combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  ggplot()+
  geom_boxplot(notch = FALSE, aes(x = dateGame, y = ptsTotal), fill = "grey", alpha =0.05)+
  geom_point(aes(x = dateGame, y = ptsTotal, colour = result), alpha = 0.15)+
  labs(x = "", y = "Actual Total") + 
  #stat_smooth(geom = "line")+
  theme_bw()


combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  ggplot(aes(y = dif_total, x = dateGame, colour = result))+
  geom_point(alpha = 0.1)+
  stat_smooth(geom = "line")+
  theme_bw()

combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  ggplot(aes(y = dif_total, x = sbr_total))+
  geom_point(aes(colour = result),alpha = 0.1)+
  geom_smooth(color = "black")+
  theme_bw()

#**THIS ONE IS VERY GOOD AT SHOWING THE RANGE OF TOTALS FOR EACH PREDICTED TOTAL
combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  ggplot()+
  geom_bar(stat = "identity", position = "stack", orientation = "x", aes(x =  sbr_total, y = result_count,  fill = result, group = sbr_total), 
           alpha = 0.4)+
  geom_point(aes(x = sbr_total, y = dif_total, fill = result, colour = result), shape = 21, colour = "black", alpha = 0.1)+
  #stat_smooth(aes(x = sbr_total, y = dif_total, colour = result),geom='line', alpha=0.5, se=FALSE)+
  labs(x = "Vegas Point Total Prediction", y = "Dots: Difference (Predicted - Actual), Bars: # of Over/Unders")+
  #ylim(-50, 50)+
  scale_y_continuous(breaks = seq(-50,50, 10), minor_breaks = seq(-50,50,10), limits = c(-50,50))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  #coord_flip()+
  theme_bw()


p2<-combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  ggplot()+
  #geom_bar(stat = "identity", position = "stack", orientation = "x", aes(x =  sbr_total, y = result_count,  fill = result, group = sbr_total), 
  #         alpha = 0.4)+
  geom_point(aes(x = sbr_total, y = dif_total, fill = result, colour = result), shape = 21, colour = "black", alpha = 0.1)+
  #stat_smooth(aes(x = sbr_total, y = dif_total, colour = result),geom='line', alpha=0.5, se=FALSE)+
  labs(x = "Vegas Point Total Prediction", y = "Dots: Difference (Predicted - Actual), Bars: # of Over/Unders")+
  #ylim(-50, 50)+
  scale_y_continuous(breaks = seq(-50,50, 10), minor_breaks = seq(-50,50,10), limits = c(-50,50))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  #coord_flip()+
  theme_bw()

p2
animate(p2+transition_time(dateGame)+
          shadow_mark(), fps = 250, renderer = gifski_renderer("../Plots/totals.gif"))


p3<-combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  group_by(result, sbr_total)%>%
  summarise(result_count = sum(result_count))%>%
  ggplot()+
  geom_bar(stat = "identity", position = "stack", orientation = "x", aes(x =  sbr_total, y = result_count,  fill = result, group = sbr_total), 
           alpha = 0.4)+
  #geom_point(aes(x = sbr_total, y = dif_total, fill = result, colour = result), shape = 21, colour = "black", alpha = 0.1)+
  #stat_smooth(aes(x = sbr_total, y = dif_total, colour = result),geom='line', alpha=0.5, se=FALSE)+
  labs(x = "Vegas Point Total Prediction", y = "Dots: Difference (Predicted - Actual), Bars: # of Over/Unders")+
  #ylim(-50, 50)+
  scale_y_continuous(breaks = seq(-50,50, 10), minor_breaks = seq(-50,50,10))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  #coord_flip()+
  theme_bw()

p3_b<-
  combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  group_by(result, sbr_total)%>%
  summarise(result_count = sum(result_count))%>%
  ggplot()+
  geom_area(position = "identity",aes(x =  sbr_total, y = result_count, fill = result), 
           alpha = 0.4)+
  #geom_point(aes(x = sbr_total, y = dif_total, fill = result, colour = result), shape = 21, colour = "black", alpha = 0.1)+
  #stat_smooth(aes(x = sbr_total, y = dif_total, colour = result),geom='line', alpha=0.5, se=FALSE)+
  labs(x = "Vegas Point Total Prediction", y = "Dots: Difference (Predicted - Actual), Bars: # of Over/Unders")+
  #ylim(-50, 50)+
  scale_y_continuous(breaks = seq(-50,50, 10), minor_breaks = seq(-50,50,10))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  #coord_flip()+
  theme_bw()

p3_c<-
  combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  group_by(result, sbr_total)%>%
  summarise(result_count = sum(result_count))%>%
  ggplot()+
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/3,
    alpha = 1/2, aes(x = sbr_total, y = result_count, fill = result)) +
  #geom_line(aes(x = sbr_total, y = sum(..result_count..)))+
  #stat_summary(aes(x = sbr_total, y = result_count), fun.y = sum(..y..), na.rm = TRUE, color = 'black', geom ='line')+
  #geom_point(aes(x = sbr_total, y = dif_total, fill = result, colour = result), shape = 21, colour = "black", alpha = 0.1)+
  #stat_smooth(aes(x = sbr_total, y = dif_total, colour = result),geom='line', alpha=0.5, se=FALSE)+
  labs(x = "Vegas Point Total Prediction", y = "Dots: Difference (Predicted - Actual), Bars: # of Over/Unders")+
  #ylim(-50, 50)+
  scale_y_continuous(breaks = seq(-50,50, 10), minor_breaks = seq(-50,50,10))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  #coord_flip()+
  theme_bw()


  
  
grid.arrange(p3, p3_b,p2,  p3_c)

combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  mutate(result_count = ifelse(result == "Over", 1,
                               ifelse(result == "Under",-1,NA)),
         sbr_total_bins = case_when(
           sbr_total <=210 ~ 210,
           sbr_total >210 & sbr_total <=215 ~ 215,
           sbr_total >215 & sbr_total <220 ~ 220,
           sbr_total >220 & sbr_total <=225 ~ 225,
           sbr_total >225 & sbr_total <230 ~ 230, 
           sbr_total >=230 ~ 235
         )
         
         )%>%
  group_by(sbr_total)%>%
  summarise(n = n(), 
            perc_over = sum(result == "Over")/n,
            avg_over_deviation = mean (dif_total [result == "Over"]),
            avg_under_deviation =  -mean(dif_total [result == "Under"]))%>%
  gather(var, val, n:length(.))%>%
  ggplot(aes(x = sbr_total, y = val))+
  geom_col(alpha = 0.5)+
  facet_wrap(~var, scales = "free", ncol= 1)+
  theme_bw()


combined%>%
  filter(yearSeason == 2020)%>%
  CleanTotals()%>%
  ggplot(aes(dif_total,ptsTotal, colour = result))+
  geom_point()+
  facet_grid(.~nameTeam)+
  #geom_violin()+
  #geom_vline(xintercept = 0, linetype = "dashed", size = 0.1)+
  theme_bw()

combined%>%
  filter(yearSeason == 2020)%>%
  select(idGame, nameTeam,home, dateGame, yearSeason, dif_total, sbr_total, ptsTotal)%>%
  mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
  mutate(result_count = ifelse(result == "Over", 1, -1), 
         home = as.character(home),
         idGame = as.character(idGame))%>%
  group_by(idGame)%>%
  mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]))%>%
  #filter(nameTeam == "Boston Celtics")%>%
  ggplot(aes(ptsTotal,nameTeam2, colour = result, alpha = abs(dif_total)))+
  geom_point()+
  #geom_text(aes(label = sbr_total), size = 3)+
  facet_grid(.~nameTeam)+
  scale_x_continuous(limits = c(150, 300), breaks = c(225))+
  theme_bw()

combined%>%
  filter(yearSeason == 2020)%>%
  select(idGame, nameTeam,home, dateGame, yearSeason, dif_total, sbr_total, ptsTotal)%>%
  mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
  mutate(result_count = ifelse(result == "Over", 1, -1), 
         home = as.character(home),
         idGame = as.character(idGame))%>%
  group_by(idGame)%>%
  mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]))%>%
  filter(nameTeam == "Boston Celtics")%>%
  ggplot(aes(ptsTotal,nameTeam2, colour = result, alpha = abs(dif_total)))+
  #geom_point()+
  geom_text(aes(label = sbr_total), size = 3)+
  facet_grid(.~nameTeam)+
  #scale_x_continuous(limits = c(150, 300), breaks = c(225))+
  theme_bw()

#Calculate Average point total for each team
avg_total<-
  combined%>%
  filter(yearSeason == 2020)%>%
  group_by(abbrev)%>%
  summarise(avg_total = mean(ptsTotal),
            avg_spread = mean(plusminusTeam))

#I really like this one. Consider removing the shape (home vs away) stuff if it looks like it's too much to look at
combined%>%
  filter(yearSeason == 2020)%>%
  #select(idGame, nameTeam,home, dateGame, yearSeason, dif_total, sbr_total, ptsTotal)%>%
  mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
  mutate(result_count = ifelse(result == "Over", 1, -1), 
         home = as.character(home),
         idGame = as.character(idGame))%>%
  group_by(idGame)%>%
  mutate(abbrev2 = if_else(home == "Away", abbrev[home == "Home"], abbrev[home == "Away"]))%>%
  filter(abbrev == "BOS")%>%
  ggplot()+
  geom_point(aes(ptsTotal,abbrev2, colour = result, size = abs(dif_total),shape = home), alpha = 0.5)+
  #This tells you the average total of each time. Feel free to remove if too much.
  geom_point(data = avg_total%>%rename(abbrev2 = "abbrev"), aes(x = avg_total, y = abbrev2), alpha = 0.1, shape = 8,colour = "black")+
  #geom_text(aes(label = sbr_total), size = 3)+
  facet_grid(.~abbrev)+
  #scale_x_continuous(limits = c(150, 300), breaks = c(225))+
  labs(x = "Actual Total Points", y = "")+
  guides(size = guide_legend(title = "How many points\n off was Vegas?",order = 3),
         colour =guide_legend(title=NULL,  order = 1),
         shape = guide_legend(title=NULL, order = 2)
  )+
  theme_bw()

# WITH LOGOS
combined%>%
  filter(yearSeason == 2020)%>%
  mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
  mutate(result_count = ifelse(result == "Over", 1, -1), 
         home = as.character(home),
         idGame = as.character(idGame))%>%
  group_by(idGame)%>%
  mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]),
         nameTeam2_logo = if_else(home == "Away", logos_local[home == "Home"], logos_local[home == "Away"]))%>%
  filter(nameTeam == "Boston Celtics")%>%
  ggplot(aes(sbr_total,dif_total))+#colour = result, size = abs(dif_total),shape = home))+
  geom_image(aes(image = nameTeam2_logo),  size = 0.04)+
  #geom_point(aes(colour = result), size = 2)+
  #geom_text(aes(label = sbr_total), size = 3)+
  facet_grid(.~nameTeam)+
  #scale_x_continuous(limits = c(150, 300), breaks = c(225))+
  labs(x = "Vegas Predicted Total", y = "How Many Points\nVegas was off on Total")+
  guides(size = guide_legend(title = "How many points\n off was Vegas?",order = 3),
         colour =guide_legend(title=NULL,  order = 1),
         shape = guide_legend(title=NULL, order = 2)
  )+
  theme_bw()

combined%>%
  filter(yearSeason == 2020)%>%
  mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
  mutate(result_count = ifelse(result == "Over", 1, -1), 
         home = as.character(home),
         idGame = as.character(idGame))%>%
  group_by(idGame)%>%
  mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]),
         nameTeam2_logo = if_else(home == "Away", logos_local[home == "Home"], logos_local[home == "Away"]))%>%
  filter(nameTeam == "Charlotte Hornets")%>%
  ggplot(aes(dateGame,ptsTotal, alpha =))+#colour = result, size = abs(dif_total),shape = home))+
  geom_image(aes(image = nameTeam2_logo), size = 0.04)+
  #geom_text(aes(label = sbr_total), size = 3)+
  facet_grid(.~nameTeam)+
  #scale_x_continuous(limits = c(150, 300), breaks = c(225))+
  labs(x = "", y = "Total Point Scored")+
  guides(size = guide_legend(title = "How many points\n off was Vegas?",order = 3),
         colour =guide_legend(title=NULL,  order = 1),
         shape = guide_legend(title=NULL, order = 2)
  )+
  theme_bw()



combined%>%
  filter(yearSeason == 2020)%>%
  select(idGame, nameTeam,home, dateGame, yearSeason, dif_total, sbr_total, ptsTotal)%>%
  mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
  mutate(result_count = ifelse(result == "Over", 1, -1), 
         home = as.character(home),
         idGame = as.character(idGame))%>%
  group_by(idGame)%>%
  mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]))%>%
  filter(nameTeam == "Boston Celtics")%>%
  ggplot(aes(dateGame,nameTeam2, colour = result),size = abs(dif_total),shape = home)+
  geom_point(alpha = 0.5)+
  #geom_text(aes(label = sbr_total), size = 3)+
  facet_grid(.~nameTeam)+
  #scale_x_continuous(limits = c(150, 300), breaks = c(225))+
  labs(x = "", y = "")+
  guides(size = guide_legend(title = "How many points\n off was Vegas?",order = 3),
         colour =guide_legend(title=NULL,  order = 1),
         shape = guide_legend(title=NULL, order = 2)
  )+
  theme_bw()


#SPREAD
labels <- combined%>%
  filter(yearSeason == 2020)%>%
  distinct(nameTeam, .keep_all = T)%>%
  mutate(logos_local = paste0("<img src='", logos_local,"' width='25' />"))%>%
  select(nameTeam, logos_local) %$%
  split(logos_local,nameTeam)%>%
  unlist()

combined%>%
filter(yearSeason == 2020)%>%
  #select(idGame, nameTeam, home, dateGame, plusminusTeam,yearSeason, dif_spread, sbr_spread,VegasCorrectRL)%>%
  mutate(result = ifelse(dif_spread<0, "Over", "Under"))%>%
  mutate(home = as.character(home),
         idGame = as.character(idGame))%>%
  group_by(idGame)%>%
  mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]),
         nameTeam2_logo = if_else(home == "Away", logos_local[home == "Home"], logos_local[home == "Away"]))%>%
  filter(nameTeam == "Boston Celtics")%>%
  ggplot(aes(-plusminusTeam,nameTeam2, colour = result, size = abs(dif_spread),shape = home))+
  geom_point(alpha = 0.5)+
  facet_grid(.~nameTeam)+
  labs(x = "Actual Score Differential (Positive = Win, Negative = Loss)", y = "")+
  guides(size = guide_legend(title = "How many points\n off was Vegas?",order = 3),
         colour =guide_legend(title=NULL,  order = 1),
         shape = guide_legend(title=NULL, order = 2)
         )+
  #Make axis titles into images!
  scale_y_discrete(name = NULL, labels = labels) +
  theme(axis.text.y = element_markdown(lineheight = 1.2))




p<-combined%>%
  filter(yearSeason == 2020)%>%
  #select(idGame, nameTeam, home, dateGame, plusminusTeam,yearSeason, dif_spread, sbr_spread,VegasCorrectRL)%>%
  mutate(result = ifelse(dif_spread<0, "Over", "Under"))%>%
  mutate(home = as.character(home),
         idGame = as.character(idGame))%>%
  group_by(idGame)%>%
  mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]))%>%
  filter(nameTeam == "Charlotte Hornets")%>%
  ggplot(aes(-sbr_spread,nameTeam2, colour = result, size = abs(dif_spread),shape = home))+
  geom_point(alpha = 0.5)+
  facet_grid(.~nameTeam)+
  labs(title = "Up to Date: {frame_time}",
    x = "Predicted Spread", y = "")+
  guides(size = guide_legend(title = "How many points\n off was Vegas?",order = 3),
         colour =guide_legend(title=NULL,  order = 1),
         shape = guide_legend(title=NULL, order = 2)
  )+
  theme_bw()

#animate(p, nframes = 60)
animate(p+
          transition_time(dateGame)+
          shadow_mark(), fps = 250, renderer = gifski_renderer("celtics.gif"))

combined%>%
  filter(yearSeason == 2020)%>%
  select(idGame, nameTeam, home, dateGame, plusminusTeam,yearSeason, dif_spread, sbr_spread,VegasCorrectRL)%>%
  mutate(result = ifelse(dif_spread<0, "Over", "Under"))%>%
  mutate(home = as.character(home),
         idGame = as.character(idGame))%>%
  group_by(idGame)%>%
  mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]))%>%
  filter(nameTeam == "Boston Celtics")%>%
  ggplot(aes(dateGame,nameTeam2, colour = result, size = abs(dif_spread),shape = home))+
  geom_point(alpha = 0.5)+
  labs(x = "", y = "")+
  guides(size = guide_legend(title = "How many points\n off was Vegas?",order = 3),
         colour =guide_legend(title=NULL,  order = 1),
         shape = guide_legend(title=NULL, order = 2)
  )+
  facet_grid(.~nameTeam)+
  #scale_x_continuous(limits = c(150, 300), breaks = c(225))+
  theme_bw()




#Check spreads to see what spreads are the most likely to go over or under.
combined%>%
  #take all of the favorited teams from the 2020 season
  filter(yearSeason == 2020, sbr_spread<0)%>%
  distinct(idGame, dateGame, plusminusTeam,yearSeason, dif_spread, sbr_spread,VegasCorrectRL)%>%
  #Remove pushes for now
  filter(dif_spread != 0)%>%
  mutate( sbr_spread = abs(sbr_spread),
          result = ifelse( dif_spread<0, "Over", "Under"))%>%
  mutate(result_count = ifelse(result == "Over", 1, -1))%>%
  ggplot()+
  geom_bar(stat = "identity", position = position_stack(), orientation = "x", 
           aes(x = sbr_spread, y = result_count, fill = result), alpha = 0.2)+
  geom_point(aes(x = sbr_spread, y = -dif_spread, colour = result), alpha = 0.1)+
  stat_smooth(aes(x = sbr_spread, y = -dif_spread, colour = result),geom='line', alpha=0.5, se=FALSE)+
  scale_x_continuous(breaks = seq(0, 20, 0.5), minor_breaks = seq(0, 20, 0.5))+
  #ylim(-50, 50)+
  theme_bw()

#Spreadsd over time
combined%>%
  filter(yearSeason == 2020,sbr_spread<0)%>%
  distinct(idGame, dateGame, plusminusTeam,yearSeason, dif_spread, sbr_spread,VegasCorrectRL)%>%
  mutate(result = ifelse( dif_spread<0, "Over", 
                          ifelse(dif_spread== 0, "Push", "Under")))%>%
  ggplot(aes(y = -dif_spread, x = dateGame, colour = result))+
  geom_point(alpha = 0.1)+
  stat_smooth(geom='line', se=FALSE)+
  theme_bw()


#total results 2019 by team (over vs under based on where line is set by team) and check by time


combined%>%
  filter(yearSeason == 2020)%>%
  select(idGame, nameTeam, dateGame, yearSeason, dif_total, sbr_total, ptsTotal)%>%
  mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
  ggplot(aes(y = dif_total, x = dateGame))+
  geom_col(aes(colour = result),alpha = 0.1)+
  #geom_smooth(color = "black")+
  stat_smooth(geom='line', alpha=0.5, se=FALSE)+
  facet_wrap(~nameTeam)


combined%>%
  filter(yearSeason == 2020)%>%
  select(idGame, nameTeam, dateGame, yearSeason, dif_total, sbr_total, ptsTotal)%>%mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
  mutate(result_count = ifelse(result == "Over", 1, -1))%>%
  ggplot()+
  geom_bar(stat = "identity", position = position_stack(),orientation = "x", 
           aes(x = sbr_total, y = result_count*10, fill = result))+
  geom_point(aes(x = sbr_total, y = dif_total, colour = result), alpha = 0.1)+
  stat_smooth(aes(x = sbr_total, y = dif_total),geom='line', alpha=0.5, se=FALSE, span = 0.5)+
  ylim(-50, 50)+
  theme_bw()+
  facet_wrap(~nameTeam)


#By SBR Total
ggplotly(combined%>%
  filter(yearSeason == 2020)%>%
  select(idGame, nameTeam, dateGame, yearSeason, dif_total, sbr_total, ptsTotal)%>%mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
  mutate(result_count = ifelse(result == "Over", 1, -1))%>%
  ggplot()+
  stat_smooth(aes(x = sbr_total, y = dif_total, colour = nameTeam),geom='line', alpha=0.5, se=FALSE, span = 0.7, size = 1.5)+
  ylim(-50, 50)+
  scale_colour_manual(values = DISTINCT_COLORS)+
  theme_bw()
)

#By Date (SBR Total)
ggplotly(combined%>%
           filter(yearSeason == 2020)%>%
           select(idGame, nameTeam, dateGame, yearSeason, dif_total, sbr_total, ptsTotal)%>%mutate(result = ifelse(ptsTotal < sbr_total, "Under", "Over"))%>%
           mutate(result_count = ifelse(result == "Over", 1, -1))%>%
           ggplot()+
           stat_smooth(aes(x = dateGame, y = dif_total, colour = nameTeam),geom='line', alpha=0.5, se=FALSE, span = 0.7, size = 1.5)+
           ylim(-50, 50)+
           scale_colour_manual(values = DISTINCT_COLORS)+
           theme_bw()
)



#Scatter plots ---------------------------------------------------------------------------
#Plot spreads x and y axis (vegas vs actual, with color showing the wager

sbr_long_pin%>%
  filter(bet == "rl")%>%
 # filter(sbrTeam == "Charlotte",yearSeason == 2020)%>%
  ggplot(aes(x = line, y = plusminusTeam, colour = ratio))+
  geom_point(alpha = 0.05)+
  geom_abline(slope = 1)+
  xlim(-60, 60)+
  ylim(-60, 60)+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_continuous(type = "viridis")

#How have teams done against the spread?
sbr_long_pin%>%
  filter(bet == "rl")%>%
  mutate(spread_result = ifelse(plusminusTeam<line, "W","L"))%>%
  filter(yearSeason == 2020)%>%
  ggplot(aes(x = line, y = plusminusTeam))+
  geom_point(aes(colour = spread_result))+
  geom_abline(slope = 1)+
  facet_wrap(~sbrTeam)+
  #xlim(-60, 60)+
  #ylim(-60, 60)+
  geom_smooth(method='lm', formula= y~x)+
  theme_bw()
  #scale_color_continuous(type = "viridis")



ggplotly(sbr_long_pin%>%
           filter(bet == "to")%>%
           filter(yearSeason == 2019)%>%
           ggplot(aes(x = line, y = ratio, colour = ptsTotal))+
           geom_point(alpha = .1)+
           geom_abline(slope = 1)+
           #xlim(150, 300)+
           #ylim(150, 300)+
           #  geom_smooth(method='lm', formula= y~x)+
           scale_color_continuous(type = "viridis"))

#how does consensus affect odds
sbr_long_pin%>%
  filter(bet == "ml", yearSeason == 2020)%>%
  ggplot(aes(x = ratio, y = consensus, colour = outcomeGame))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  theme_bw()

sbr_long_pin%>%
  filter(bet == "rl", yearSeason == 2020)%>%
  mutate(spread_result = ifelse(plusminusTeam<line, "W","L"))%>%
  ggplot(aes(x = ratio, y = consensus, colour = spread_result))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  theme_bw()



sbr_long_pin%>%
  filter(bet == "rl")%>%
  #filter(yearSeason == 2020)%>%
  ggplot(aes(x = line, y = plusminusTeam, colour = consensus))+
  geom_point(alpha = 0.5)+
  geom_abline(slope = 1)+
  scale_color_continuous(type = "viridis")+
  theme_bw()

sbr_long_pin%>%
  filter(bet == "rl")%>%
  filter(yearSeason == 2020)%>%
  mutate(spread_result = ifelse(plusminusTeam<line, "W","L"))%>%
  ggplot(aes(x = consensus, y = line, colour = spread_result))+
  geom_point(alpha = 0.1)+
  geom_smooth(method='lm', formula= y~x)+
  theme_bw()


#Totals odds aren't that important (mostly come in around -110)
sbr_long_pin%>%filter(bet == "to")%>%ggplot(aes(x = ratio))+geom_bar()


#Show all the games that Vegas has missed this year (moneyline, spread, and totals). How have they missed? Can we connect performance stats in understanding that?
#Take a dummy YES/NO did the bet get won (better for Moneyline)?
##Run regresions with performance stats to see what is predictive of a vegas miss. Potentially for spreads, you can do this regression on the amoutn missed by

#A couple of questions:
# How does higher conensus affect Lines/Odds?
## Are there certain situations where consensus picking may affect line (e.g., greater uncertainty of outcome) while #
## another situation where conensus may affect odds (e.g., we are more confident about this score but want to even out the books)

combined%>%
  filter(yearSeason == 2020)%>%
  #select(idGame, nameTeam, home, dateGame, plusminusTeam,yearSeason, dif_spread, sbr_spread,VegasCorrectRL)%>%
  mutate(result = ifelse(dif_spread<0, "Over", "Under"))%>%
  ggplot(aes(x = consensus_rl, y = dif_spread, shape = result, colour = ratio_rl))+
  geom_point()+
  scale_colour_viridis_c()

p<-combined%>%
  filter(yearSeason == 2020)%>%
  #select(idGame, nameTeam, home, dateGame, plusminusTeam,yearSeason, dif_spread, sbr_spread,VegasCorrectRL)%>%
  mutate(result = ifelse(dif_spread<0, "Over", "Under"))%>%
  group_by(idGame)%>%
  mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]))%>%
  #ggplot(aes(x = consensus_rl, y = ratio_rl, shape = VegasCorrectRL, colour = sbr_spread), alpha = 0.5)+
  ggplot(aes(x = sbr_spread, y = consensus_rl, shape = VegasCorrectRL, colour = ratio_rl), alpha = 0.5)+
  geom_point(aes(text = paste0(nameTeam, " Vs: ", nameTeam2, "\n",
                               "Date: ", dateGame, "\n",
                               #"Score: ", val, "\n",
                               "Vegas: ",sbr_spread, ", Actual: ",plusminusTeam)),alpha=0.5)+
  scale_colour_viridis_c()+
  facet_wrap(~VegasCorrectRL)+
  #stat_smooth(geom = "line")+
  theme_bw()

ggplotly(p, 
         tooltip = 'text',
         dynamicTicks = TRUE
)

             
             
### Export Results --------------------------------------------------------------------------------------------------

write.csv(x, ".",row.names=FALSE, na="0")
