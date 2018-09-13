# Setup ------------------------------------------------------------------------

# Load required packages
library(ggplot2)   # for awesome plotting
library(investr)   # for plotFit() function
library(magrittr)  # for forward pipe operator (i.e., %>%)
library(mda)       # for mars() function
library(pdp)       # for PDPs

# Better colors
set1 <- RColorBrewer::brewer.pal(9, "Set1")
dark2 <- RColorBrewer::brewer.pal(8, "Dark2")

# Helper function
plot_mars <- function(x, y, step = 1L, verbose = FALSE, ...) {
  fit <- mars(x, y, nk = step, prune = FALSE, ...)
  cuts <- unique(as.vector(fit$cuts)[-1L])
  if (verbose) {
    if (length(cuts) == 0) {
      message("Knot location: NA")
    } else {
      message("Knot location: ", 
              paste(round(cuts, digits = 4)), collapse = ", ")
    }
  }
  pd <- partial(fit, pred.var = "x", train = data.frame(x = x), 
                grid.resolution = 1000)
  pd %>%
    autoplot(color = "red2", size = 1.2) +
    geom_point(data = data.frame(x, y), aes(x, y)) +
    geom_vline(xintercept = cuts, linetype = "dashed") +
    theme_light() +
    labs(x = "x", y = "y")
}

# Simulate data
set.seed(1421)
x <- runif(100, min = -3, max = 3)
y <- -x^2 + rnorm(100)
simd <- data.frame(x, y)


# Plot forward pass steps ------------------------------------------------------

# Scatterplot of the data
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  theme_light() +
  ggtitle("Simulated quadratic data")

# Plot various steps in the forward pass
grid.arrange(
  plot_mars(x, y, step = 1) + ggtitle("Single region"),
  plot_mars(x, y, step = 3) + ggtitle("Two regions"),
  plot_mars(x, y, step = 5) + ggtitle("Three regions"),
  plot_mars(x, y, step = 7) + ggtitle("Four regions"),
  plot_mars(x, y, step = 9) + ggtitle("Five regions (overfitting?)"),
  plot_mars(x, y, step = 11) + ggtitle("Six regions (overfitting?)"),
  ncol = 3
)


# Manually build up a MARS model (forward pass only) ---------------------------

# Possible knot locations
knots <- sort(unique(x))

# Generate list of two basis functions for a particular knot: original + mirror
h <- function(k) {
  list(
    function(x) ifelse(x < k, k - x, 0), 
    function(x) ifelse(x > k, x - k, 0)
  )
}

# Plot basis pair
xx <- 0:100/100
yy1 <- h(0.5)[[1L]](xx)
yy2 <- h(0.5)[[2L]](xx)
plot(xx, yy1, type = "l", lwd = 3, col = dark2[1L],
     xlab = "x", ylab = "Basis function", las = 1)
lines(xx, yy2, lwd = 3, col = dark2[2L])
text(0.1, 0.2, label = expression((t-x)["+"]), cex = 2, col = dark2[1L])
text(0.9, 0.2, label = expression((x-t)["+"]), cex = 2, col = dark2[2L])

# Plot all possible basis function pairs for this example (this may not be very
# useful)
xx <- sort(x)
par(mfrow = c(10, 10), mar = c(0.1, 0.1, 0.1, 0.1))
for (k in knots) {
  plot(xx, h(k)[[1L]](xx), type = "l", lwd = 2, 
       col = adjustcolor(dark2[1L], alpha.f = 0.5),
       xlab = "", ylab = "", las = 1, axes = FALSE)
  lines(xx, h(k)[[2L]](xx), lwd = 2, col = adjustcolor(dark2[2L], alpha.f = 0.5))
}

# Step 1 of the forward pass

# For each knot, fit an ordinary LM and extract the resulting R-squared
step1 <- lapply(knots, function(k) {
  fit <- lm(y ~ h(k)[[1L]](x) + h(k)[[2L]](x))
  list(
    "fit" = fit,
    "rsquared" = summary(fit)$r.squared
  )
})

# Which model has the largest R-squared?
knot_id_step1 <- which.max(unlist(lapply(step1, function(x) x$rsquared)))
knots[knot_id_step1]

# Plot a few of the models built during the first step of the forward pass
par(mfrow = c(3, 3), mar = c(4, 4, 1/2, 1/2) + 0.1)
for (i in c(10, 20, 30, 40, 52, 60, 70, 80, 90)) {
  rsq <- round(summary(step1[[i]]$fit)$r.squared, digits = 3)
  plotFit(step1[[i]]$fit, data = simd, lwd.fit = 2, col.fit = "red2",
          pch = 19, col = adjustcolor("black", alpha.f = 0.25))
  text(0, -6, label = bquote(bold(R^2 == .(rsq))))
  abline(v = knots[i], lty = 2, col = adjustcolor("black", alpha.f = 0.25))
}

# Step 2 of the forward pass

# For each knot, fit an ordinary LM and extract the resulting R-squared
step2 <- lapply(knots, function(k) {
  fit <- lm(y ~ h(knots[knot_id_step1])[[1L]](x) + 
              h(knots[knot_id_step1])[[2L]](x) + 
              h(k)[[1L]](x) + h(k)[[2L]](x))
  list(
    "fit" = fit,
    "rsquared" = summary(fit)$r.squared,
    "sigma" = summary(fit)$sigma
  )
})

# Overlay R-squared values
dev.off()
plotFit(step1[[knot_id_step1]]$fit, data = simd, lwd.fit = 2, col.fit = "red2",
        pch = 19, col = adjustcolor("black", alpha.f = 0.25))
r2 <- unlist(lapply(step2, function(x) x$rsquared))
par(new = TRUE)
plot(knots, r2, col = "green3", new = FALSE, type = "l", lwd = 2,
     xlab = "", ylab = "", axes = FALSE)

# Which model has the largest R-squared?
knot_id_step2 <- which.max(unlist(lapply(step2, function(x) x$rsquared)))
knots[knot_id_step2]

# Plot a few of the models built during the first step of the forward pass
par(mfrow = c(3, 3), mar = c(4, 4, 1/2, 1/2) + 0.1)
for (i in c(10, 20, 30, 40, 52, 60, 77, 80, 90)) {
  rsq <- round(summary(step2[[i]]$fit)$r.squared, digits = 3)
  plotFit(step2[[i]]$fit, data = simd, lwd.fit = 2, col.fit = "red2",
          pch = 19, col = adjustcolor("black", alpha.f = 0.25))
  text(0, -6, label = bquote(bold(R^2 == .(rsq))))
  abline(v = knots[i], lty = 2, col = adjustcolor("black", alpha.f = 0.25))
}


# MARS: Boston housing example -------------------------------------------------

# Load required packages
library(earth)  # for fitting MARS models
library(vip)    # for variable importance plots

# Load the Boston housing example
data(boston, package = "pdp")

# Scatterplot matrix
pairs(boston[c("cmedv", "lstat", "rm", "age", "lon", "lat")],
      col = adjustcolor("purple2", alpha.f = 0.5))

# Fit a second-degree MARS model
boston_mars <- earth(
  cmedv ~ .,  
  data = boston,
  degree = 2  # tuning parameter 
)

# Print model summary
print(boston_mars)

# Print detailed model summary
summary(boston_mars)

# Plot model summary
plot(boston_mars)

# Variable importance plot
vip(boston_mars, num_features = 15)

# Partial dependence of cmedv on rm
p1 <- boston_mars %>%
  partial(pred.var = "rm") %>%
  autoplot(color = "red2", size = 1) +
  geom_point(data = boston, aes(x = rm, y = cmedv), alpha = 0.1) +
  theme_light()

# Partial dependence of cmedv on lstat
p2 <- boston_mars %>%
  partial(pred.var = "lstat") %>%
  autoplot(color = "red2", size = 1) +
  geom_point(data = boston, aes(x = lstat, y = cmedv), alpha = 0.1) +
  theme_light()

# Partial dependence of cmedv on rm and lstat
p3 <- boston_mars %>%
  partial(pred.var = c("rm", "lstat"), chull = TRUE) %>%  
  autoplot() +
  theme_light()

# Display plots side-by-side
grid.arrange(p1, p2, p3, ncol = 3)


# Your turn --------------------------------------------------------------------


# MARS: paranmeter tuning ------------------------------------------------------

# Load required packages
library(caret)

# Print tuning parameters
getModelInfo("earth")$earth$parameters

# Tune a MARS model
set.seed(1512)  # for reprocubility
boston_mars_tune <- train(
  x = subset(boston, select = -cmedv),
  y = boston$cmedv,
  method = "earth",
  metric = "Rsquared",
  trControl = trainControl(method = "repeatedcv", 
                           number = 5, repeats = 3),
  tuneGrid = expand.grid(degree = 1:5, nprune = 100)
)

# Print model tuning summary
print(boston_mars_tune)

# Plot model tuning summary
ggplot(boston_mars_tune) + theme_light()  # or use plot() for lattice graphics

# Print model summary (for final model)
summary(boston_mars_tune$finalModel)

# Variable importance plot
vip(boston_mars_tune, num_features = 15) 

# Partial dependence of cmedv on both rm and nox
pd <- partial(
  boston_mars_tune, 
  pred.var = c("rm", "nox"),
  chull = TRUE
)

# Interactive 3-D plot
plotly::plot_ly(
  x = ~rm,    # x-axis
  y = ~nox,   # y-axis
  z = ~yhat,  # z-axis
  data = pd,
  type = "mesh3d"
)


# Your turn --------------------------------------------------------------------


# MARS for logistic regression -------------------------------------------------

# Load the e-mail spam data
data(spam, package = "kernlab")  # classification

# Partition the data into train/test sets
set.seed(101)  # for reproducibility
trn_id <- createDataPartition(spam$type, p = 0.7, list = FALSE)
trn <- spam[trn_id, ]                # training data
tst <- spam[-trn_id, ]               # test data

# Fit a MARS model to the e-mail spam example
spam_mars <- earth(
  type ~ ., 
  data = trn, 
  degree = 3, 
  glm = list(family = binomial(link = "logit"))  # <<
)

# Print basic summary
spam_mars

# Variable importance plot
vip(spam_mars, num_features = 15)

# Partial dependence plots (pd3 make take a couple minutes!)
pd1 <- partial(spam_mars, pred.var = "remove",
               quantiles = TRUE, probs = 5:95/100)
pd2 <- partial(spam_mars, pred.var = "charExclamation", 
               quantiles = TRUE, probs = 5:95/100)
pd3 <- partial(spam_mars, pred.var = c("hp", "capitalLong"),
               quantiles = TRUE, probs = 5:95/100, progress = "text")
grid.arrange(
  autoplot(pd1) + theme_light(), 
  autoplot(pd2) + theme_light(), 
  plotPartial(pd3), 
  ncol = 3
)

# Confusion matrix based on test data
prob <- predict(spam_mars, newdata = tst, type = "response")[, 1L]
class_label <- ifelse(prob > 0.5, "spam", "nonspam")
(cm <- table("pred" = class_label, "obs" = tst$type))
1 - sum(diag(cm)) / nrow(tst)  # test error ~ 06.60%
