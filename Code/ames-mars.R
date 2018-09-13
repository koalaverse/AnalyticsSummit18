# MARS demonstration

# Questions for the students to ponder:
#
#   (1) Why use log(Sale_Price)? --- because of squared error loss!

# Load required packages
library(earth)
library(ggplot2)

# Load the Ames housing data
ames <- AmesHousing::make_ordinal_ames()

# Better colors
cols <- RColorBrewer::brewer.pal(8, "Dark2")

# Scatterplot of
p <- ggplot(ames, aes(x = Gr_Liv_Area, y = log(Sale_Price))) +
  geom_point(alpha = 0.1) +
  theme_light()
p

# Regression fits
fm1 <- lm(log(Sale_Price) ~ Gr_Liv_Area, data = ames)
fm2 <- earth(log(Sale_Price) ~ Gr_Liv_Area, data = ames)

# Add estimated regression lines
tbl <- tibble::tibble(
  x = seq(from = min(ames$Gr_Liv_Area), to = max(ames$Gr_Liv_Area), 
          length = 1000),
  y1 = predict(fm1, newdata = data.frame(Gr_Liv_Area = x)),
  y2 = predict(fm2, newdata = data.frame(Gr_Liv_Area = x))[, 1L]
)
p1 <- p + 
  geom_line(data = tbl, aes(x = x, y = y1), color = cols[1L], size = 1.2) +
  ggtitle("Simple linear regression")
p2 <- p + 
  geom_line(data = tbl, aes(x = x, y = y2), color = cols[2L], size = 1.2) +
  geom_vline(xintercept = fm2$cuts[, 1L][-1L], linetype = "dashed") +
  ggtitle("Adaptive linear splines")
gridExtra::grid.arrange(p1, p2, ncol = 2)

# Full MARS model (second-degree)
fm3 <- earth(log(Sale_Price) ~ ., data = na.omit(ames), degree = 1)
