#https://towardsdatascience.com/building-a-deep-learning-model-using-keras-1548ca149d37
#https://blogs.rstudio.com/tensorflow/posts/2018-01-11-keras-customer-churn/
#For more background:
#https://www.xenonstack.com/blog/artificial-neural-network-applications/
#Regression Problem:
#https://www.datatechnotes.com/2019/01/regression-example-with-keras-in-r.html
#https://towardsdatascience.com/machine-learning-for-sports-betting-not-a-basic-classification-problem-b42ae4900782
#https://towardsdatascience.com/guide-to-building-a-college-basketball-machine-learning-model-in-python-1c70b83acb51

# Load libraries
library(keras)
library(lime)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
library(tidyverse)
library(tidyquant)


#Import Data ---------------------------------
churn_data_raw<-read_csv("C:\\Users\\nated\\Documents\\Documents_NB\\Projects\\Forecasting ML\\IBM Churn\\Input\\Telco-Customer-Churn.csv")


#Clean Data -------------------------------------

churn_data_tbl <- churn_data_raw %>%
  select(-customerID) %>%
  drop_na() %>%
  select(Churn, everything())

reg_data_tbl<-reg_data%>%
  #Temporary remove pushes. Don't do this when I actually try to predict
  filter(dif_spread !=0)%>%
  ungroup()%>%
  mutate(SpreadOver = ifelse(dif_spread<0, 1, 0),
         idTeam = as.character(idTeam))%>%
  select(-nameTeam, -dateGame,-idGame,-ptsTotal, -win, -plusminusTeam,
         -idTeam,-consensus_to, -ratio_to, -sbr_total,
         -dif_total, -dif_spread, -VegasCorrectML, -VegasCorrectRL, -VegasCorrectTO,
         
         
         )%>%
  select(SpreadOver, everything())

# Split test/training sets
set.seed(100)
train_test_split <- initial_split(reg_data_tbl, prop = 0.8)
train_test_split

# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

#Pre Process Data ----------------------------------

# Create recipe
rec_obj <- recipe(SpreadOver ~ ., data = train_tbl) %>%
  #step_discretize(tenure, options = list(cuts = 6)) %>%
  #step_log(TotalCharges) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  #step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train_tbl)


# Predictors
x_train_tbl <- bake(rec_obj, new_data = train_tbl) %>% select(-SpreadOver)
x_test_tbl  <- bake(rec_obj, new_data = test_tbl) %>% select(-SpreadOver)

# Response variables for training and testing sets
y_train_vec <- pull(train_tbl, SpreadOver)
y_test_vec  <- pull(test_tbl, SpreadOver)


#Build Model -------------------------------------------

# Building our Artificial Neural Network
model_keras <- keras_model_sequential()

model_keras %>% 
  
  # First hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Second hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu") %>% 
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", 
    activation         = "sigmoid") %>% 
  
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )

model_keras

# Fit the keras model to the training data
history <- fit(
  object           = model_keras, 
  x                = as.matrix(x_train_tbl), 
  y                = y_train_vec,
  batch_size       = 50, 
  epochs           = 35,
  validation_split = 0.30
)

#Make Predictions --------------------------------

# Predicted Class
yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
  class_prob = yhat_keras_prob_vec
)

estimates_keras_tbl

options(yardstick.event_first = FALSE)

#How do those predictions look? ---------------------
# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)

# Accuracy
estimates_keras_tbl %>% metrics(truth, estimate)

#Precision is when the model predicts "yes", how often is it actually "yes". Recall (also true positive rate or specificity) is when the actual value is "yes" how often is the model correct. We can get precision() and recall() measurements using yardstick.
tibble(
  precision = estimates_keras_tbl %>% precision(truth, estimate),
  recall    = estimates_keras_tbl %>% recall(truth, estimate)
)

#USE LIME TO EXPLAIN MODEL --------------------------------
# Setup lime::model_type() function for keras
class(model_keras)

model_type.keras.engine.sequential.Sequential <- function(x, ...) {
  "classification"
}

# Setup lime::predict_model() function for keras
predict_model.keras.engine.sequential.Sequential <- function(x, newdata, type, ...) {
  pred <- predict_proba(object = x, x = as.matrix(newdata))
  data.frame(Yes = pred, No = 1 - pred)
}

predict_model(x = model_keras, newdata = x_test_tbl, type = 'raw') %>%
  tibble::as_tibble()


# Run lime() on training set
explainer <- lime::lime(
  x              = x_train_tbl, 
  model          = model_keras, 
  bin_continuous = FALSE
)

explanation <- lime::explain(
  x_test_tbl[1:10, ], 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 4,
  kernel_width = 0.5
)


plot_features(explanation) +
  labs(title = "LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

#Correlation Analysis -------------------------------------
#Feature correlations to Churn
corrr_analysis <- x_train_tbl %>%
  mutate(Churn = y_train_vec) %>%
  correlate() %>%
  focus(Churn) %>%
  rename(feature = rowname) %>%
  arrange(abs(Churn)) %>%
  mutate(feature = as_factor(feature)) 
corrr_analysis

# Correlation visualization
corrr_analysis %>%
  ggplot(aes(x = Churn, y = fct_reorder(feature, desc(Churn)))) +
  geom_point() +
  # Positive Correlations - Contribute to churn
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[2]], 
               data = corrr_analysis %>% filter(Churn > 0)) +
  geom_point(color = palette_light()[[2]], 
             data = corrr_analysis %>% filter(Churn > 0)) +
  # Negative Correlations - Prevent churn
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[1]], 
               data = corrr_analysis %>% filter(Churn < 0)) +
  geom_point(color = palette_light()[[1]], 
             data = corrr_analysis %>% filter(Churn < 0)) +
  # Vertical lines
  geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  # Aesthetics
  theme_tq() +
  labs(title = "Churn Correlation Analysis",
       subtitle = paste("Positive Correlations (contribute to churn),",
                        "Negative Correlations (prevent churn)"),
       y = "Feature Importance")
