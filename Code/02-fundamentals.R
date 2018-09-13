
# Prerequisites -----------------------------------------------------------

library(rsample)  # data splitting & resampling
library(ggplot2)  # visualization
library(caret)    # provides consistent modeling syntax
library(recipes)  # feature pre-processing


# Slide 7 -----------------------------------------------------------------

# Make sure that you get the same random numbers
set.seed(123)
data_split <- initial_split(pdp::boston, prop = .7, strata = "cmedv")

boston_train <- training(data_split)
boston_test  <- testing(data_split)

nrow(boston_train)/nrow(pdp::boston)


# Slide 8 -----------------------------------------------------------------

# how do the distributions differ?
ggplot(boston_train, aes(x = cmedv)) + 
  geom_density(trim = TRUE) + 
  geom_density(data = boston_test, trim = TRUE, col = "red")


# Challenge #1 (slide 10) -------------------------------------------------

# Create training (70%) and test (30%) sets for the AmesHousing::make_ames() data. 
# Stratify on the Sale_Price variable and visualize this variable for the training 
# and testing sets to see if their distributions are similar.




# Slide 14 ----------------------------------------------------------------

# full rank 
full_rank <- dummyVars(~ ., boston_train, fullRank = TRUE)
boston_train <- predict(full_rank, boston_train) %>% as.data.frame()
boston_test  <- predict(full_rank, boston_test) %>% as.data.frame()

# 1-to-1 dummy encode (less than full rank)
one_hot <- dummyVars(~ ., boston_train, fullRank = FALSE)
boston_train <- predict(one_hot, boston_train) %>% as.data.frame()
boston_test  <- predict(one_hot, boston_test) %>% as.data.frame()



# Slide 15 ----------------------------------------------------------------

# Response Transformations

## log transformation
train_y <- log(boston_train$cmedv)
test_y  <- log(boston_test$cmedv)

## Box Cox transformation
lambda  <- forecast::BoxCox.lambda(boston_train$cmedv)
train_y <- forecast::BoxCox(boston_train$cmedv, lambda)
test_y  <- forecast::BoxCox(boston_test$cmedv, lambda)

## Inverse Box Cox
inv_box_cox <- function(x, lambda) {
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda) 
}


# Slide 16 ----------------------------------------------------------------

# Feature transformation

## name of features
features <- setdiff(names(boston_train), "cmedv")

## pre-process estimation based on training features
pre_process <- caret::preProcess(
  x      = boston_train[, features],
  method = c("BoxCox", "center", "scale")
)

## apply to both training & test
train_x <- predict(pre_process, boston_train[, features])
test_x  <- predict(pre_process, boston_test[, features])



# Challenge #2 (slide 19) -------------------------------------------------

# Perform the following feature engineering on your Ames training data:
# - Response variable
#    - Normalize
# 
# - Predictor variables
#    - One-hot encode
#    -Standardize (center & scale)
#    - Remove zero-variance variables




# Slide 23 ----------------------------------------------------------------

# Base lm
m1.base <- lm(cmedv ~ ., data = boston_train)
summary(m1.base)

# caret approach
m1.caret <- train(cmedv ~ ., data = boston_train, method = "lm")
summary(m1.caret)



# Slide 31 ----------------------------------------------------------------

# Create 10 fold with rsample::vfold_cv
set.seed(123)
cv_split <- vfold_cv(pdp::boston, v = 10, strata = "cmedv")
cv_split


# Slide 32 ----------------------------------------------------------------

# Perform CV with for loop

## create empty vector to store error metric
rmse <- vector(mode = "numeric", length = nrow(cv_split))

## iterate through each fold, model, predict, score
for (i in 1:nrow(cv_split)) {
  m <- lm(cmedv ~ ., data = analysis(cv_split$splits[[i]]))
  p <- predict(m, assessment(cv_split$splits[[i]]))
  rmse[i] <- caret::RMSE(p, assessment(cv_split$splits[[i]])$cmedv)
}

mean(rmse)


# Slide 33 ----------------------------------------------------------------

# Built-in CV (example with caret)
m2.caret <- train(
  cmedv ~ ., 
  data = boston_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

m2.caret


# Challenge #3 (slide 38) -------------------------------------------------

# Starting with the raw AmesHousing::make_ames() data:
# 1. split your data into 70 (training) / 30 (testing)
# 2. normalize the response variable
# 3. standardize the features (don't one-hot encode as lm will automate the dummy encoding)
# 4. apply a linear regression model with 10-fold cross validation
# 5. what is your average CV RMSE?


