

# Slide 10: Package prerequisite ------------------------------------------

library(keras)


# Slide 11: Data Requirement ----------------------------------------------

# one hot encode --> we use model.matrix(...)[, -1] to discard the intercept
data_onehot <- model.matrix(~ ., AmesHousing::make_ames())[, -1] %>% as.data.frame()

# Create training (70%) and test (30%) sets for the AmesHousing::make_ames() data.
# Use set.seed for reproducibility
set.seed(123)
split <- rsample::initial_split(data_onehot, prop = .7, strata = "Sale_Price")
train <- rsample::training(split)
test  <- rsample::testing(split)

# Create & standardize feature sets
# training features
train_x <- train %>% dplyr::select(-Sale_Price)
mean    <- colMeans(train_x)
std     <- apply(train_x, 2, sd)
train_x <- scale(train_x, center = mean, scale = std)

# testing features
test_x <- test %>% dplyr::select(-Sale_Price)
test_x <- scale(test_x, center = mean, scale = std)

# Create & transform response sets
train_y <- log(train$Sale_Price)
test_y  <- log(test$Sale_Price)

# zero variance variables (after one hot encoded) cause NaN so we need to remove
zv <- which(colSums(is.na(train_x)) > 0, useNames = FALSE)
train_x <- train_x[, -zv]
test_x  <- test_x[, -zv]

# check dimensions
dim(train_x)
dim(test_x)



# Slides 14-24: Developing the model --------------------------------------

model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 10, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae", "mape")
  )

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE
)

learn

plot(learn)


# Slide 27: Tuning the layers & nodes -------------------------------------

model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 500, activation = "relu", input_shape = ncol(train_x)) %>% #<<
  layer_dense(units = 250, activation = "relu") %>% # <<
  layer_dense(units = 125, activation = "relu") %>% #<<
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae", "mape")
  )

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE
)

plot(learn)


# Slide 29: Challenge #1 --------------------------------------------------

# Reduce the layers and nodes until you find stable errors.




# Slide 31: Adjust epochs -------------------------------------------------

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE,
  callbacks = list(  #<<
    callback_early_stopping(patience = 2) #<<
  ) #<<
)

learn

plot(learn)



# Slide 32: Add batch normalization ---------------------------------------

model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 100, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_batch_normalization() %>% #<<
  layer_dense(units = 50, activation = "relu") %>%
  layer_batch_normalization() %>% #<<
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae", "mape")
  )

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE,
  callbacks = list(  
    callback_early_stopping(patience = 2) 
  ) 
)

learn

plot(learn)



# Slide 33: Add dropout ---------------------------------------------------

model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 100, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.2) %>% #<<
  layer_dense(units = 50, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.2) %>% #<<
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae", "mape")
  )

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE,
  callbacks = list(  
    callback_early_stopping(patience = 2) 
  ) 
)


learn

plot(learn)


# Slide 34: Add weight regularization -------------------------------------

model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 100, activation = "relu", input_shape = ncol(train_x),
              kernel_regularizer = regularizer_l2(0.001)) %>% #<<
  layer_batch_normalization() %>%
  layer_dense(units = 50, activation = "relu",
              kernel_regularizer = regularizer_l2(0.001)) %>% #<<
  layer_batch_normalization() %>%
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae", "mape")
  )

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE,
  callbacks = list(
    callback_early_stopping(patience = 2)
  )
)

learn

plot(learn)



# Slide 36: Adjust learning rate ------------------------------------------

model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 100, activation = "relu", input_shape = ncol(train_x),
              kernel_regularizer = regularizer_l2(0.001)) %>% #<<
  layer_batch_normalization() %>%
  layer_dense(units = 50, activation = "relu",
              kernel_regularizer = regularizer_l2(0.001)) %>% #<<
  layer_batch_normalization() %>%
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "adadelta", #<<
    loss = "mse",
    metrics = c("mae", "mape")
  )

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE,
  callbacks = list(
    callback_early_stopping(patience = 2),
    callback_reduce_lr_on_plateau() #<<
  )
)

learn

plot(learn)



# Slide 38: Challenge #2 --------------------------------------------------

# Adjust the tuning parameters and see if you can further reduce the validation loss metric
# - Adjust layers/nodes
# - Increase epochs if you do not see a flatlined loss function
# - Add batch normalization
# - Add dropout
# - Add weight regularization
# - Adjust learning rate



# Slide 41: evaluate a new data set ---------------------------------------

(results <- model %>% evaluate(test_x, test_y))



# Slide 42: Predicting on new data ----------------------------------------

model %>% 
  predict(test_x) %>% 
  broom::tidy() %>% 
  dplyr::mutate(
    truth = test_y, 
    pred_tran = exp(x), 
    truth_tran = exp(truth)
  ) %>%
  yardstick::rmse(truth_tran, pred_tran)


