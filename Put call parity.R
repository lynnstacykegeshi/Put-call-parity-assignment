# Load the necessary libraries
library(dplyr)
library(tidyr)

# Read options data from file
tesla <- read.csv("teslaoptions.csv")
View(tesla)

# Clean the data
tesla <- tesla %>%
  # Convert appropriate columns to numeric data types
  mutate_at(vars(c(3:4, 7, 9, 10:11)), as.numeric) %>%
  # Remove any rows with missing values
  drop_na()

# Calculate cmkt and pmkt
cmkt <- 0.5 * (tesla[, 3] + tesla[, 4])
pmkt <- 0.5 * (tesla[, 10] + tesla[, 11])
strike <- tesla[, 7]

# Find intersection point
idx <- which.min(abs(cmkt - pmkt))
x_intersect <- strike[idx]

# Plotting
call = plot(strike, cmkt, lwd = 3, col = 2, ylab = "Payoff",
            main = "Observed European option price ",
            xlab = "Strike Price")
put = points(strike, pmkt, cex = 0.8, col = "#006400")

abline(v = x_intersect, col = "black")

# Linear Regression model
fit1 <- lm(cmkt ~ pmkt + strike)
summary(fit1)

# Error analysis
error <- fit1$residuals
hist(error, probability = T)
shapiro.test(error)
shapiro.test(rnorm(1000))
qqnorm(error)
qqline(error)
