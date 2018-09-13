###############################
# Setting Up Your Environment #
###############################

# The following packages will be used. Additional packages may need to be
# installed throughout the two-day course as needed
list.of.packages <- c(
  'alr3',          # for Swiss banknote data
  'AmesHousing',   # for Ames housing data
  'caret',         # for classification and regression training
  'devtools',      # for install_github() function
  'dplyr',         # for (mostly) pain free data wrangling
  'earth',         # for multivariate adaptive regression splines
  'forecast',      # for normalizing 
  'gbm',           # for generalized boosted models
  'ggplot2',       # for awesome plotting
  'glmnet',        # for elastic net (e.g., the lasso and ridge regression)
  'investr',       # for plotFit() function
  'keras',         # for deep learning
  'kernlab',       # for fitting support vector machines
  'klaR',          # for naive Bayes
  'magrittr',      # for using the forward pipe operator (i.e., %>%)
  'MASS',          # for LDA/QDA functions
  'mda',           # for multivariate adaptive regression splines
  'pdp',           # for partial dependence plots and ICE/c-ICE curvess
  'plotly',        # interactive plots
  'randomForest',  # for random forest 
  'ranger',        # for fast and efficient random forest
  'rpart',         # for binary recursive partitioning (i.e., decision trees)
  'rpart.plot',    # for plotting decision tree diagrams
  'rsample',       # for data splitting and some data sets
  'svmpath',       # for fitting the entire SVM regularization path 
  'tidyr',         # for easily tidying data
  'xgboost',       # for eXtreme Gradient Boosting
  'yardstick'      # for various ML metrics
)

# Run the following lines of code to install the packages you do not have
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[, "Package"])
]

if(length(new.packages)) {
  install.packages(new.packages)
}

# Install the vip package (for variable importance plots) from our GitHub repo
devtools::install_github("koalaverse/vip")
