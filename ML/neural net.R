#https://towardsdatascience.com/building-a-deep-learning-model-using-keras-1548ca149d37
#https://blogs.rstudio.com/tensorflow/posts/2018-01-11-keras-customer-churn/

# Helper packages
library(dplyr)         # for basic data wrangling

# Modeling packages
library(keras)         # for fitting DNNs
library(tfruns)        # for additional grid search & model training functions

# Modeling helper package - not necessary for reproducibility
library(tfestimators)  # provides grid search & model training interface

# Import MNIST training data
mnist <- dslabs::read_mnist()
mnist_x <- mnist$train$images
mnist_y <- mnist$train$labels

# Rename columns and standardize feature values
colnames(mnist_x) <- paste0("V", 1:ncol(mnist_x))
mnist_x <- mnist_x / 255

# One-hot encode response
mnist_y <- to_categorical(mnist_y, 10)


model <- keras_model_sequential() %>%
  
  # Network architecture
  layer_dense(units = 128, activation = "relu", input_shape = ncol(mnist_x)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax") %>%
  
  # Backpropagation
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )

# Train the model
fit1 <- model %>%
  fit(
    x = mnist_x,
    y = mnist_y,
    epochs = 25,
    batch_size = 128,
    validation_split = 0.2,
    verbose = TRUE
  )

# Display output
fit1

plot(fit1)

#Make predictions ---------------------------------

predict(fit1, mnist_x)

# Predicted Class
yhat_keras_class_vec <- 
  predict_classes(object = model, 
                  x = as.matrix(x_test_tbl)) %>%
  as.vector()

# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()