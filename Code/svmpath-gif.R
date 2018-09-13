# Simulate data
set.seed(805)
norm2d <- as.data.frame(mlbench::mlbench.2dnormals(
  n = 100,
  cl = 2,
  r = 4,
  sd = 1
))
names(norm2d) <- c("x1", "x2", "y")  # rename columns

# Add an outlier
norm2d <- rbind(norm2d, data.frame("x1" = 0.5, "x2" = 1, "y" = 2))

# Load required packages
library(svmpath)

# Fit the entire regularization path
fit_smc <- svmpath(
  x = data.matrix(norm2d[c("x1", "x2")]), 
  y = ifelse(norm2d$y == 1, 1, -1)
)

# Make a gif (requires that ImageMagick beinstalled on machine)
png(file = "GIFs/svmpath%02d.png", width = 4, height = 4, 
    units = "in", res = 300)
for (i in sort(unique(fit_smc$Step))) {
  plot_svmpath(fit_smc, step = i)
}
dev.off()
sys_string <- paste(
  "convert -delay 10",
  paste("GIFs/svmpath", sprintf('%0.2d', max(fit_smc$Step)), ".png", 
        sep = "", collapse = " "),
  "GIFs/svmpath.gif"
)
system(sys_string)
file.remove(list.files(path = "GIFs", pattern = ".png", full.names = TRUE))

