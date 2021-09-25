###############################################################################
###
###   PURPOSE: 
###
###############################################################################



### Analyze Data ---------------------------------------------------------------------------------------------------
FirstModel<-function(DEPENDENT, INDEPENDENTS, type = "Logit"){
  
  #Establish formula
  f <- as.formula(
    paste(DEPENDENT, 
          paste(INDEPENDENTS, collapse = " + "), 
          sep = " ~ "))
  
  #Logit
  if (type == "Logit"){
  first_model<- 
    glm(f,
        data = reg_data%>% na.omit()%>%filter(yearSeason==2020),
        family = "binomial")
  

  }else{
    
  first_model<-
    lm(f, 
       data = reg_data%>% na.omit()%>%filter(yearSeason==2020))

  }
  
  return(first_model)
}
LinearModel<-function(DEPENDENT, INDEPENDENTS, type = "Logit"){
  
  first_model<-FirstModel(DEPENDENT, INDEPENDENTS, type = type)
  
  #How is fti
  prediction = augment(first_model, reg_data%>%filter(yearSeason == 2020), type.predict = "response")%>%
    left_join(crosswalk%>%distinct(idTeam, .keep_all=T))
  
  
  print(summary(first_model))
  #glance(first_model, model)
  
  
  print(ggplot(prediction)+
    #geom_violin(aes(x = as.factor(VegasCorrect), y = .fitted), alpha = 0.01, color = "blue")+
    geom_boxplot(aes(x = as.factor(eval(as.name(DEPENDENT))), y = .fitted), color = "blue")+
    xlab(DEPENDENT)+
    facet_wrap(~yearSeason))+
    theme_bw()
  
  
  
  return(prediction)
  
}

LinearToday<-function(DEPENDENT, INDEPENDENTS, type = "Logit"){
  
  first_model<-FirstModel(DEPENDENT, INDEPENDENTS, type = type)

  #Predict probability of whether Today's Vegas predictions are wrong
  if (type == "Logit"){
  predicted<-todays_info%>%
    mutate(prediction = predict(first_model, ., type = "response"))%>%
    left_join(crosswalk%>%distinct(idTeam, .keep_all=T))
  }else {
  predicted<-todays_info%>%
    mutate(prediction = predict(first_model, .))%>%
    left_join(crosswalk%>%distinct(idTeam, .keep_all=T))
  }
  return(predicted)  
}
#---------------------------------------------------------

#Category 1: Predict probability of a given bet. Should work for all three bet types
#Win/Lose = TeamAHome[various team stats,  strength of schedule, negative_win_shares_injury] + 
#          TeamBAway[various team stats, strength of schedule, negative_win_shares_injury] +
#          Odds[Team A (or Team B)] +
#          PercentageBettors[Team A (or Team B)] +
#          Some time fixed effects (since uncertainty is bound to change throughout the season? See if this is significant)




#Category 2: Predict spread given performance stats, independent of odds. Compare to vegas predicted spreads

#Category 3: Training and test dataset, binary classification?

#Category 4: Use nearest neighbor approach to find how previous games went

#Category 5: Simpler approach of score differential throughout game (this is probably more useful for real time betting)




#Helpful links:
##https://towardsdatascience.com/using-linear-regression-to-model-point-spreads-in-college-basketball-f7da5811c3da
##https://sports.sites.yale.edu/nba-model-math
