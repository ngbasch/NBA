#https://dataaspirant.com/2017/01/09/knn-implementation-r-using-caret-package/

# Helper packages
library(dplyr)      # for data wrangling
library(ggplot2)    # for awesome graphics
library(rsample)    # for creating validation splits
library(recipes)    # for feature engineering

# Modeling packages
library(caret)       # for fitting KNN models


#Establish formula
f <- as.formula(
  paste(DEPENDENT, 
        paste(INDEPENDENTS, collapse = " + "), 
        sep = " ~ "))

#ATtrition data
# create training (70%) set for the rsample::attrition data.
attrit <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)
set.seed(123)
churn_split <- initial_split(attrit, prop = .7, strata = "Attrition")
churn_train <- training(churn_split)
churn_test <- testing(churn_split)


# create training (70%) set for the rsample::attrition data.

set.seed(123)
churn_split <- reg_data%>%
  ungroup()%>%
  na.omit()%>%
  filter(yearSeason == 2020)%>%
  # mutate_at(vars(home, isB2B, isB2B_opp, contains("Vegas")), as.factor)%>%
  # mutate(home = ifelse(home == 1, "Home", "Away"),
  #        isB2B = ifelse(isB2B == 1, "Yes","No"),
  #        isB2B_opp = ifelse(isB2B == 1, "Yes","No"),
  #        VegasCorrectML= ifelse(VegasCorrectML == 1, "Vegas_Correct", "Vegas_Incorrect"),
  #        VegasCorrectRL= ifelse(VegasCorrectML == 1, "Vegas_Correct", "Vegas_Incorrect")
  #        )%>%
  #select(-idTeam, -dateGame, -idGame, -VegasCorrectRL, -VegasCorrectTO, -yearSeason)%>%
  initial_split(., prop = .7, strata = "nameTeam")
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

# Create blueprint
blueprint <- recipe(formula = f, data = churn_train) %>%
  step_nzv(all_nominal()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

t<-blueprint$var_info

# Create a resampling method
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5,
  #Un-comment this if you want to see progress
  verboseIter = TRUE,
  classProbs = TRUE,                 
  summaryFunction = twoClassSummary
)

# Create a hyperparameter grid search
hyper_grid <- expand.grid(
  k = floor(seq(1, nrow(churn_train)/3, length.out = 20))
)


# Fit knn model and perform grid search
knn_grid <- train(
  blueprint, 
  data = churn_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "ROC"
)

ggplot(knn_grid)

#most important variables?
vi <- varImp(knn_grid)

#Test 

test_pred<-predict(knn_grid, newdata = churn_test)

#Using confusion matrix, we can print stats for our results. it shows that our accuracy for test 
#set is XX%

confusionMatrix(test_pred, as.factor(churn_test$win))
