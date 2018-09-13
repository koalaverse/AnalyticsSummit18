library(h2o)
library(caret)
library(tidyverse)

h2o.init(max_mem_size = "8g")

# Prep data ---------------------------------------------------------------

# raw
train <- read.csv("Kaggle/Ames/ames_train.csv") %>% as.tibble()
test  <- readr::read_csv("Kaggle/Ames/ames_test.csv")
solutions <- readr::read_csv("Kaggle/Ames/ames_test_with_solutions.csv")

# create feature names
y <- "Sale_Price"
x <- setdiff(names(train), y)

# turn sets into h2o objects
train.h2o <- as.h2o(train)
test.h2o  <- as.h2o(test)
solution.h2o <- as.h2o(solutions)


# Random Forests ----------------------------------------------------------

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(20, 30, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)

# build grid search 
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  nfolds = 5,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)

# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_rf_model <- h2o.getModel(best_model_id)

# Now let’s evaluate the model performance on a test set
(best_model_perf <- h2o.performance(model = best_rf_model, newdata = solution.h2o))

# RMSE of best model --> 24444.39
h2o.mse(best_model_perf) %>% sqrt()

model_path <- h2o.saveModel(object = best_rf_model, path = "/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/", force = TRUE)
rf <- h2o.loadModel("/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/rf_grid_model_89")


# Regularized Regression --------------------------------------------------

# grid over `alpha`
# select the values for `alpha` to grid over
hyper_params <- list(
  alpha = seq(0, 1, by = .1),
  lambda = seq(0.0001, 10, length.out = 10)
)

# build grid search with previously selected hyperparameters
grid_elastic <- h2o.grid(
  x = x, 
  y = y, 
  training_frame = train.h2o, 
  nfolds = 5,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE,
  algorithm = "glm",
  grid_id = "elastic_grid", 
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian")
)

# Sort the grid models by mse
sorted_grid <- h2o.getGrid("elastic_grid", sort_by = "mse", decreasing = FALSE)
sorted_grid

# grab top model id
best_en_model <- sorted_grid@model_ids[[1]]
best_en_model <- h2o.getModel(best_en_model)

# Now let’s evaluate the model performance on a test set
(best_en_perf <- h2o.performance(model = best_en_model, newdata = solution.h2o))

# RMSE of best model --> 25619.91
h2o.mse(best_en_perf) %>% sqrt()

model_path <- h2o.saveModel(object = best_en_model, path = "/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/", force = TRUE)
glm <- h2o.loadModel("/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/elastic_grid_model_109")



# GBM ---------------------------------------------------------------------

# Construct a large Cartesian hyper-parameter space
ntrees_opts <- c(10000)       # early stopping will stop earlier
max_depth_opts <- seq(1,20)
min_rows_opts <- c(1,5,10,20,50,100)
learn_rate_opts <- seq(0.001,0.01,0.001)
sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_per_tree_opts <- seq(0.3,1,0.05)
nbins_cats_opts <- seq(100,10000,100)

hyper_params <- list( 
  ntrees = ntrees_opts,
  max_depth = max_depth_opts,
  min_rows = min_rows_opts, 
  learn_rate = learn_rate_opts,
  sample_rate = sample_rate_opts,
  col_sample_rate = col_sample_rate_opts,
  col_sample_rate_per_tree = col_sample_rate_per_tree_opts,
  nbins_cats = nbins_cats_opts
  )


# Search a random subset of these hyper-parmameters. Max runtime 
# and max models are enforced, and the search will stop after we 
# don't improve much over the best 5 random models.
search_criteria = list(
  strategy = "RandomDiscrete",
  max_runtime_secs = 1800,
  max_models = 200, 
  stopping_metric = "AUTO", 
  stopping_tolerance = 0.00001, 
  stopping_rounds = 5, 
  seed = 123456
  )

# train models
gbm_grid <- h2o.grid(
  "gbm", 
  grid_id = "gbm_grid",
  x = x, 
  y = y,
  training_frame = train.h2o,
  nfolds = 5,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE,
  
  # stop as soon as mse doesn't improve by
  # more than 0.1% on the validation set, 
  # for 2 consecutive scoring events:
  stopping_rounds = 2,
  stopping_tolerance = 1e-3,
  stopping_metric = "MSE",
  
  # how often to score (affects early stopping):
  score_tree_interval = 100, 
  
  ## seed to control the sampling of the
  ## Cartesian hyper-parameter space:
  seed = 123456,
  hyper_params = hyper_params,
  search_criteria = search_criteria
  )

(gbm_sorted_grid <- h2o.getGrid(grid_id = "gbm_grid", sort_by = "mse", decreasing = FALSE))

# grab top model id
best_gbm_model <- gbm_sorted_grid@model_ids[[1]]
best_gbm_model <- h2o.getModel(best_gbm_model)

# Now let’s evaluate the model performance on a test set
best_gbm_perf <- h2o.performance(model = best_gbm_model, newdata = solution.h2o)

# RMSE of best model --> 21622.78
h2o.mse(best_gbm_perf) %>% sqrt()

model_path <- h2o.saveModel(object = best_gbm_model, path = getwd(), force = TRUE)
gbm <- h2o.loadModel("/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/gbm_grid_model_3")



# Ensemble ----------------------------------------------------------------

library(h2oEnsemble)

glm1 <- h2o.glm(
  x = x, y = y,
  family = "gaussian",
  training_frame = train.h2o,
  alpha = 1,
  lambda = 100,
  nfolds = 10,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE
)

rf1 <- h2o.randomForest(
  x = x, y = y, 
  training_frame = train.h2o,
  ntrees = 400,
  mtries = 30,
  sample_rate = 0.8,
  seed = 1,
  nfolds = 10,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE
)

gbm1 <- h2o.gbm(
  x = x, y = y, 
  distribution = "gaussian",
  training_frame = train.h2o,
  ntrees = 3320,
  max_depth = 17,
  min_rows = 20,
  learn_rate = 0.007,
  sample_rate = 0.9,
  col_sample_rate = 0.85,
  col_sample_rate_per_tree = 0.55,
  seed = 1,
  nfolds = 10,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE
)


# stack existing models
stack <- h2o.stackedEnsemble(
  x = x, 
  y = y,
  metalearner_algorithm = "gbm",
  model_id = "initial_ensemble",
  training_frame = train.h2o,
  base_models = list(glm1, rf1)
)

# Compute test set performance:
(perf <- h2o.performance(stack, newdata = solution.h2o))

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(stack, newdata = solution.h2o)



###################################################################
# Rerun models with standardized features and normalized response #
###################################################################
rm(list = ls())
h2o.shutdown(prompt = FALSE)
h2o.init(max_mem_size = "8g")

# Prep data ---------------------------------------------------------------

# raw
train <- read.csv("Kaggle/Ames/ames_train.csv") %>% as.tibble()
solutions <- readr::read_csv("Kaggle/Ames/ames_test_with_solutions.csv")

# normalize response
train2 <- train %>% mutate(Sale_Price = log(Sale_Price))
solutions2 <- solutions %>% mutate(Sale_Price = log(Sale_Price))

# create feature names
y <- "Sale_Price"
x <- setdiff(names(train), y)

# standardize features 
process <- preProcess(
  x = train2[, x],
  method = c("YeoJohnson", "center", "scale")
)

# apply to both training & test
train2 <- predict(process, train2)
solutions2  <- predict(process, solutions2)

# turn sets into h2o objects
train.h2o <- as.h2o(train2)
solution.h2o <- as.h2o(solutions2)


# Random Forests ----------------------------------------------------------

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(20, 30, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)

# build grid search 
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid2", 
  sort_by = "mse", 
  decreasing = FALSE
)

# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_rf_model <- h2o.getModel(best_model_id)

# Now let’s evaluate the model performance on a test set 
(best_model_perf <- h2o.performance(model = best_rf_model, newdata = solution.h2o))

# RMSE of best model --> 0.1223805
h2o.mse(best_model_perf) %>% sqrt()

# RMSE of best model (retransformed) --> 25458.95
pred <- h2o.predict(best_rf_model, newdata = solution.h2o)
RMSE(as.vector(exp(pred)), solutions$Sale_Price)

model_path <- h2o.saveModel(object = best_rf_model, path = "/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/", force = TRUE)
rf <- h2o.loadModel("/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/rf_grid_model_89")


# Regularized Regression --------------------------------------------------

# grid over `alpha`
# select the values for `alpha` to grid over
hyper_params <- list(
  alpha = seq(0, 1, by = .1),
  lambda = seq(0.0001, 10, length.out = 10)
)

# build grid search with previously selected hyperparameters
grid_elastic <- h2o.grid(
  x = x, 
  y = y, 
  training_frame = train.h2o, 
  nfolds = 5,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE,
  algorithm = "glm",
  grid_id = "elastic_grid2", 
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian")
)

# Sort the grid models by mse
sorted_grid <- h2o.getGrid("elastic_grid2", sort_by = "mse", decreasing = FALSE)
sorted_grid

# grab top model id
best_en_model <- sorted_grid@model_ids[[1]]
best_en_model <- h2o.getModel(best_en_model)

# Now let’s evaluate the model performance on a test set
(best_en_perf <- h2o.performance(model = best_en_model, newdata = solution.h2o))


# RMSE of best model --> 0.1159412
h2o.mse(best_en_perf) %>% sqrt()

# RMSE of best model (retransformed) --> 22511.5
pred <- h2o.predict(best_en_model, newdata = solution.h2o)
RMSE(as.vector(exp(pred)), solutions$Sale_Price)


model_path <- h2o.saveModel(object = best_rf_model, path = "/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/", force = TRUE)
glm <- h2o.loadModel("/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/elastic_grid2_model_10")



# GBM ---------------------------------------------------------------------

# Construct a large Cartesian hyper-parameter space
ntrees_opts <- c(10000)       # early stopping will stop earlier
max_depth_opts <- seq(1,20)
min_rows_opts <- c(1,5,10,20,50,100)
learn_rate_opts <- seq(0.001,0.01,0.001)
sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_per_tree_opts <- seq(0.3,1,0.05)
nbins_cats_opts <- seq(100,10000,100)

hyper_params <- list( 
  ntrees = ntrees_opts,
  max_depth = max_depth_opts,
  min_rows = min_rows_opts, 
  learn_rate = learn_rate_opts,
  sample_rate = sample_rate_opts,
  col_sample_rate = col_sample_rate_opts,
  col_sample_rate_per_tree = col_sample_rate_per_tree_opts,
  nbins_cats = nbins_cats_opts
)


# Search a random subset of these hyper-parmameters. Max runtime 
# and max models are enforced, and the search will stop after we 
# don't improve much over the best 5 random models.
search_criteria = list(
  strategy = "RandomDiscrete",
  max_runtime_secs = 1800,
  max_models = 200, 
  stopping_metric = "AUTO", 
  stopping_tolerance = 0.00001, 
  stopping_rounds = 5, 
  seed = 123456
)

# train models
gbm_grid <- h2o.grid(
  "gbm", 
  grid_id = "gbm_grid2",
  x = x, 
  y = y,
  training_frame = train.h2o,
  nfolds = 5,
  keep_cross_validation_predictions = TRUE,
  
  # stop as soon as mse doesn't improve by
  # more than 0.1% on the validation set, 
  # for 2 consecutive scoring events:
  stopping_rounds = 2,
  stopping_tolerance = 1e-3,
  stopping_metric = "MSE",
  
  # how often to score (affects early stopping):
  score_tree_interval = 100, 
  
  ## seed to control the sampling of the
  ## Cartesian hyper-parameter space:
  seed = 123456,
  hyper_params = hyper_params,
  search_criteria = search_criteria
)

(gbm_sorted_grid <- h2o.getGrid(grid_id = "gbm_grid2", sort_by = "mse", decreasing = FALSE))

# grab top model id
best_gbm_model <- gbm_sorted_grid@model_ids[[1]]
best_gbm_model <- h2o.getModel(best_gbm_model)

# Now let’s evaluate the model performance on a test set
best_gbm_perf <- h2o.performance(model = best_gbm_model, newdata = solution.h2o)

# RMSE of best model --> 0.109657
h2o.mse(best_gbm_perf) %>% sqrt()

# RMSE of best model (retransformed) --> 21601.25
pred <- h2o.predict(best_gbm_model, newdata = solution.h2o)
RMSE(as.vector(exp(pred)), solutions$Sale_Price)

h2o.saveModel(object = best_gbm_model, path = "/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/", force = TRUE)
gbm <- h2o.loadModel("/Users/bradboehmke/Dropbox/AnalyticsSummit18/Kaggle/Ames/gbm_grid2_model_3")


