
# Slide 14: Package prerequisite ------------------------------------------

library(rsample)  # data splitting 
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret


# Slide 14: Data prerequisite ---------------------------------------------

# convert some numeric variables to factors
attrition <- attrition %>%
  mutate(
    JobLevel = factor(JobLevel),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
  )
# Create training (70%) and test (30%) sets for the attrition data.
# Use set.seed for reproducibility
set.seed(123)
split <- initial_split(attrition, prop = .7, strata = "Attrition")
train <- training(split)
test  <- testing(split)


# Slide 16: Challenge #1 --------------------------------------------------

# How well does our assumption of conditional independence between the features 
# hold up?




# Slide 18: Default implementation of naive Bayes -------------------------

# create response and feature data
features <- setdiff(names(train), "Attrition")
x <- train[, features]
y <- train$Attrition

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

# results
confusionMatrix(nb.m1)


# Slide 19: Benchmark comparison ------------------------------------------

# initial naive results
confusionMatrix(nb.m1)

# distribution of Attrition rates across train & test set
table(train$Attrition) %>% prop.table()
table(test$Attrition) %>% prop.table()



# Slide 21: Continuous variable densities ---------------------------------

train %>% 
  select_if(is.numeric) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free", ncol = 2)



# Slide 24: Tuning --------------------------------------------------------

# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  adjust = 0:3,
  fL = 0:2
)

nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid
)

# top 3 modesl
nb.m2$results %>% 
  top_n(3, wt = Accuracy) %>%
  arrange(desc(Accuracy))



# Slide 26: Challenge #2 --------------------------------------------------

## Tune this model some more. Do you gain any improvement?

## Can you think if anything else you could do (i.e. pre-processing)?



# Slide 28: Predicting ----------------------------------------------------

# make predictions based on new data
pred <- predict(nb.m3, newdata = test)

# evaluate accuracy on holdout test set
confusionMatrix(pred, test$Attrition)
