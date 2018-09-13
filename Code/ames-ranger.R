# Load required packages
library(dplyr)
library(ggplot2)
library(pdp)
library(ranger)
library(vip)

# Load the data
ames <- as.data.frame(AmesHousing::make_ames())

# Fit a random forest
set.seed(2152)
ames_rf <- ranger(
  Sale_Price ~ .,
  data = ames,
  # xtest = subset(ames_tst, select = -Year_Sold),
  # ytst = ames_tst$Year_Sold,
  num.tree = 500,
  importance = "impurity"
)

# Print model summary
print(ames_rf)

# Variable importance plot
vip(ames_rf)

# Partial dependence plots
p1 <- ames_rf %>%
  partial(pred.var = "Overall_Qual") %>%
  autoplot(alpha = 0.1) +
  coord_flip()
p2 <- ames_rf %>%
  partial(pred.var = "Gr_Liv_Area") %>%
  autoplot()
p3 <- ames_rf %>%
  partial(pred.var = c("Gr_Liv_Area", "Total_Bsmt_SF"), chull = TRUE,
          progress = "text") %>%
  autoplot()
grid.arrange(p1, p2, p3, ncol = 3)

# Centered ICE curves
ames_rf %>%
  partial(pred.var = "Overall_Qual", ice = TRUE, center = TRUE) %>%
  plotPartial(alpha = 0.1, pch = 19, scales = list(x = list(rot = 45)))
