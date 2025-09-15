## =========================================================
## BME710 Lab — Linear Regression & Correlation in R
## Source: Glover & Mitchell, "An Introduction to Biostatistics using R"
## Chapter 10: Linear Regression and Correlation
## This script mirrors the code shown in the chapter and adds inline
## documentation so students can follow each step.
## =========================================================


## ---------------------------------------------------------
## Example 10.1 — Simple Linear Regression
## Relationship between Temperature (X) and Heart Rate (Y)
## ---------------------------------------------------------

# 1) Load data from the textbook site
data.Ex10.1 <- read.table(
  "http://waveland.com/Glover-Mitchell/Example10-1.txt",
  header = TRUE
)

# Quick peek at the last few rows (mirrors the book’s printout)
tail(data.Ex10.1, n = 3)

# 2) Make a scatterplot (X first, then Y for plot(); labels help!)
plot(
  data.Ex10.1$Temperature, data.Ex10.1$HrtRt,
  main = "Heart Rate vs Temperature",
  xlab = "Temperature (°C)", ylab = "BPM"
)

# Alternate one-liner using the data frame directly (X must be col 1, Y col 2)
plot(
  data.Ex10.1,
  main = "Heart Rate vs Temperature",
  xlab = "Temperature (°C)", ylab = "BPM"
)

# 3) Fit least-squares regression line: lm(Y ~ X)
lm.Ex10.1 <- lm(HrtRt ~ Temperature, data = data.Ex10.1)

# Print coefficients (intercept & slope)
lm.Ex10.1

# 4) Add the fitted regression line to the plot
abline(lm.Ex10.1, col = "red")

# 5) ANOVA table to test H0: beta (slope) = 0
aov.Ex10.1 <- aov(HrtRt ~ Temperature, data = data.Ex10.1)
summary(aov.Ex10.1)

# 6) 95% & 99% CI for coefficients (intercept and slope)
confint(lm.Ex10.1)             # default 95%
confint(lm.Ex10.1, level = 0.99)

# 7) Confidence interval for mean response at a given X (Temperature)
# Single X = 9
new <- data.frame(Temperature = 9)
predict(lm.Ex10.1, new, interval = "confidence")  # 95% CI for mean µ_Y|X

# Multiple X's at once (9, 17, 21)
new <- data.frame(Temperature = c(9, 17, 21))
predict(lm.Ex10.1, new, interval = "confidence")

# 8) Confidence band lines across the observed X’s
# Build a data frame of fitted values + 95% CI bounds at each observed X
fitted_ci <- as.data.frame(predict(lm.Ex10.1, interval = "confidence"))

# Re-plot the data and regression line
plot(
  data.Ex10.1,
  main = "Heart Rate versus Temperature",
  xlab = "Temperature (°C)", ylab = "BPM"
)
abline(lm.Ex10.1, col = "red")

# Add confidence band boundaries
lines(data.Ex10.1$Temperature, fitted_ci$lwr, col = "blue", lty = "longdash")
lines(data.Ex10.1$Temperature, fitted_ci$upr, col = "blue", lty = "longdash")


