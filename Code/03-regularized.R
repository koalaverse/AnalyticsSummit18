# Slide 14: Prerequisites -------------------------------------------------

# packages used
library(rsample)      # data splitting & resampling
library(tidyr)        # data manipulation
library(dplyr)        # data manipulation
library(ggplot2)      # visualization
library(caret)        # data prep
{{library(glmnet)}}   # implementing regularized regression approaches

# data used
boston <- pdp::boston

# Slide 15: Create training & hold out data -------------------------------

set.seed(123)
data_split <- initial_split(boston, prop = .7, strata = "cmedv")

boston_train <- training(data_split)
boston_test  <- testing(data_split)


# Slide 15: Create target and feature data --------------------------------

train_x <- model.matrix(cmedv ~ ., boston_train)[, -1]
train_y <- boston_train$cmedv

test_x <- model.matrix(cmedv ~ ., boston_test)[, -1]
test_y <- boston_test$cmedv


# Slide 18: Challenge #1 ------------------------------------------------

# Create training (70%) and test (30%) sets for the AmesHousing::make_ames() data.
# Use set.seed(123)

set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7, strata = "Sale_Price")
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

# Create training and testing feature model matrices and response vectors.

ames_train_x <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
ames_train_y <- log(ames_train$Sale_Price)

ames_test_x <- model.matrix(Sale_Price ~ ., ames_test)[, -1]
ames_test_y <- log(ames_test$Sale_Price)

# What is the dimension of of your feature matrix?
dim(ames_train_x)


# Ridge Regression -----------------------------------------------

## Slide 19: fit ridge regression
boston_ridge <- glmnet(
  x = train_x,
  y = train_y,
  alpha = 0
)

## Slide 19: lambdas applied
boston_ridge$lambda
plot(boston_ridge, xvar = "lambda")
plot(boston_ridge, xvar = "dev")

## Slide 20: coefficients when lambda = 6963 vs lambda = 0.696
coef(boston_ridge)[, 1]
coef(boston_ridge)[, 100]

## Slide 21: fit CV ridge regression
boston_ridge <- cv.glmnet(
  x = train_x,
  y = train_y,
  alpha = 0,
  nfolds = 10
)

## Slide 21: plot CV MSE
plot(boston_ridge)

## Slide 22: min MSE & lambdas
boston_ridge$lambda.min
coef(boston_ridge, s = "lambda.min")
sqrt(boston_ridge$cvm[boston_ridge$lambda == boston_ridge$lambda.1se])

## Slide 24: plot coefficients
coef(boston_ridge, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Rank-order of variable influence") +
  xlab("Coefficient") +
  ylab(NULL)

# Challenge #2 ------------------------------------------------------------

## Slides 28 & 29: Apply CV Ridge regression to ames data
ames_ridge <- cv.glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 0
)

plot(ames_ridge)
ames_ridge$lambda.1se
coef(ames_ridge, s = "lambda.1se")
sqrt(ames_ridge$cvm[ames_ridge$lambda == ames_ridge$lambda.1se])

### just show plot
ames_ridge2 <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 0
)
plot(ames_ridge2, xvar = "lambda")
abline(v = log(ames_ridge$lambda.1se), col = "red", lty = "dashed")


# Lasso Regression --------------------------------------------------------

## Slide 32: fit lasso regression
boston_lasso <- glmnet(
  x = train_x,
  y = train_y,
  alpha = 1
)

## lambdas applied
boston_lasso$lambda
plot(boston_lasso, xvar = "lambda")
plot(boston_lasso, xvar = "dev")

## coefficients when lambda = 6.926 vs lambda = 0.007
coef(boston_lasso)[, 1]
coef(boston_lasso)[, 25]

## Slide 33: fit CV ridge regression
boston_lasso <- cv.glmnet(
  x = train_x,
  y = train_y,
  alpha = 1
)

## plot CV MSE
plot(boston_lasso)

boston_lasso$lambda.1se
coef(boston_lasso, s = "lambda.1se")


# Challenge #3 ------------------------------------------------------------

## Slide 36-37: Apply CV Lasso regression to ames data
ames_lasso <- cv.glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1
)

plot(ames_lasso)
ames_lasso$lambda.1se
coef(ames_lasso, s = "lambda.1se")
sqrt(ames_lasso$cvm[ames_lasso$lambda == ames_lasso$lambda.1se])
sqrt(ames_lasso$cvm[ames_lasso$lambda == ames_lasso$lambda.min])

### just show plot
ames_lasso2 <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1
)
plot(ames_lasso2, xvar = "lambda")
abline(v = log(ames_lasso$lambda.1se), col = "red", lty = "dashed")
abline(v = log(ames_lasso$lambda.min), col = "red", lty = "dashed")


# Elastic net -------------------------------------------------------------

## Slide 41: run different models
fold_id <- sample(1:10, size = length(train_y), replace=TRUE)

cv_lasso    <- cv.glmnet(train_x, train_y, alpha = 1.0, foldid = fold_id) 
cv_elastic1 <- cv.glmnet(train_x, train_y, alpha = 0.3, foldid = fold_id)
cv_elastic2 <- cv.glmnet(train_x, train_y, alpha = 0.6, foldid = fold_id)
cv_ridge    <- cv.glmnet(train_x, train_y, alpha = 0.0, foldid = fold_id)

par(mfrow = c(2, 2))
plot(cv_lasso)
plot(cv_elastic1)
plot(cv_elastic2)
plot(cv_ridge)

## Slide 42: Automate tuning
### tuning grid
tuning_grid <- tibble::tibble(
  alpha     = seq(0, 1, by = .1),
  rmse_min  = NA,
  rmse_1se  = NA
)

### loop through tuning grid
for(i in seq_along(tuning_grid$alpha)) {
  # fit CV model for each alpha value
  fit <- cv.glmnet(train_x, train_y, alpha = tuning_grid$alpha[i], foldid = fold_id)
  # extract MSE and lambda values
  tuning_grid$mse_min[i] <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i] <- fit$cvm[fit$lambda == fit$lambda.1se]
}

tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>%
  ggplot(aes(alpha, mse_min)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
  ggtitle("MSE ± one standard error")


# Challenge #4 ------------------------------------------------------------

## Not shown on slides: compare lasso to ridge to elastic with alpha = 0.5
fold_id <- sample(1:10, size = length(ames_train_y), replace=TRUE)
cv_lasso   <- cv.glmnet(ames_train_x, ames_train_y, alpha = 1.0, foldid = fold_id) 
cv_elastic <- cv.glmnet(ames_train_x, ames_train_y, alpha = 0.5, foldid = fold_id) 
cv_ridge   <- cv.glmnet(ames_train_x, ames_train_y, alpha = 0.0, foldid = fold_id)

par(mfrow = c(3, 1))
plot(cv_lasso)
plot(cv_elastic)
plot(cv_ridge)

## Slide 45-46: search across a range of alphas
tuning_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min   = NA,
  mse_1se   = NA,
  lambda_min = NA,
  lambda_1se = NA
)

for(i in seq_along(tuning_grid$alpha)) {
  fit <- cv.glmnet(ames_train_x, ames_train_y, alpha = tuning_grid$alpha[i], foldid = fold_id)
  tuning_grid$mse_min[i]   <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]   <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>%
  ggplot(aes(alpha, mse_min)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
  ggtitle("MSE ± one standard error")

## get the coefficients
best_fit <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1,
  lambda = subset(tuning_grid, alpha == 1)$lambda_min
)

library(dplyr)

best_fit %>%
  coef() %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point()



# Slide 48: Prediction ----------------------------------------------------

# some best model
cv_lasso   <- cv.glmnet(ames_train_x, ames_train_y, alpha = 1.0)
min(cv_lasso$cvm)

# predict
pred <- predict(cv_lasso, ames_test_x)
mse <- mean((ames_test_y - pred)^2)


# Alternative methods -----------------------------------------------------

## Slide 50: caret package
library(caret)

train_control <- trainControl(method = "cv", number = 10)

caret_mod <- train(
  x = ames_train_x,
  y = ames_train_y,
  method = "glmnet",
  preProc = c("center", "scale", "zv", "nzv"),
  trControl = train_control,
  tuneLength = 10
)

option1


## Slide 51: h2o package
library(h2o)
h2o.init()

# convert data to h2o object
ames_h2o <- ames_train %>%
  mutate(Sale_Price_log = log(Sale_Price)) %>%
  as.h2o()

# set the response column to Sale_Price_log
response <- "Sale_Price_log"

# set the predictor names
predictors <- setdiff(colnames(ames_train), "Sale_Price")


# try using the `alpha` parameter:
# train your model, where you specify alpha
ames_glm <- h2o.glm(
  x = predictors, 
  y = response, 
  training_frame = train,
  nfolds = 10,
  keep_cross_validation_predictions = TRUE,
  alpha = .25
)

# print the mse for the validation data
print(h2o.rmse(ames_glm, xval = TRUE))

# grid over `alpha`
# select the values for `alpha` to grid over
hyper_params <- list(
  alpha = seq(0, 1, by = .1),
  lambda = seq(0.0001, 10, length.out = 10)
)

# this example uses cartesian grid search because the search space is small
# and we want to see the performance of all models. For a larger search space use
# random grid search instead: {'strategy': "RandomDiscrete"}

# build grid search with previously selected hyperparameters
grid <- h2o.grid(
  x = predictors, 
  y = response, 
  training_frame = train, 
  nfolds = 10,
  keep_cross_validation_predictions = TRUE,
  algorithm = "glm",
  grid_id = "ames_grid", 
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian")
)

# Sort the grid models by mse
sorted_grid <- h2o.getGrid("ames_grid", sort_by = "mse", decreasing = FALSE)
sorted_grid

# grab top model id
best_h2o_model <- sorted_grid@model_ids[[1]]
best_model <- h2o.getModel(best_h2o_model)

# shut down h2o
h2o.shutdown()
