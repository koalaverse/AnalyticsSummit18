# Colors
dark2 <- RColorBrewer::brewer.pal(8, "Dark2")

# Simulate data
set.seed(1432)
circle <- as.data.frame(mlbench::mlbench.circle(
  n = 200,
  d = 2
))
names(circle) <- c("x1", "x2", "y")  # rename columns

# Naive Bayes classifier
library(e1071)
fit_nb <- naiveBayes(as.factor(y) ~ ., data = circle)

# Grid over which to evaluate decision boundaries
npts <- 500
xgrid <- expand.grid(
  x1 = seq(from = -1.25, 1.25, length = npts),
  x2 = seq(from = -1.25, 1.25, length = npts)
)

# Predicted probabilities (as a two-column matrix)
prob_nb <- predict(fit_nb, newdata = xgrid, type = "raw")

# Plot decision boundary: naive Bayes
plot(
  formula = x2 ~ x1, 
  data = circle, 
  cex = 1.2,
  pch = c(17, 19)[circle$y],
  col = adjustcolor(dark2[circle$y], alpha.f = 0.5),
  main = "Naive Bayes"
)
contour(
  x = sort(unique(xgrid$x1)), 
  y = sort(unique(xgrid$x2)), 
  z = matrix(prob_nb[, 1L], nrow = npts), 
  levels = 0.5,
  col = "black", 
  drawlabels = FALSE, 
  lwd = 2,
  lty = 1,
  add = TRUE
)
