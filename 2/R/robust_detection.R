# Libraries
library(MASS)
library(reshape2)
library(ggplot2)

# ---------- PART 3.1 : Robust Outlier Detection ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
quantitative_data <- data[quantitative_cols]

## Use MCD estimator (Coverage parameter of 0.75)
COVERAGE <- 0.75
set.seed(0)
robust <- cov.rob(quantitative_data, cor = TRUE, quantile.used = COVERAGE * dim(data)[1], method = "mcd")

## Retrieve robust means and covariance matrix
robust_mean <- robust$center
robust_cov <- robust$cov

## Compute robust Mahalanobis distances
robust_maha <- mahalanobis(quantitative_data, robust_mean, robust_cov)
robust_maha <- data.frame(robust_maha)

## Compute sample average and sample covariance
sample_mean <- colMeans(quantitative_data)
sample_cov <- cov(quantitative_data)

## Compute classic Mahalanobis distances
classic_maha <- mahalanobis(quantitative_data, sample_mean, sample_cov)
classic_maha <- data.frame(classic_maha)

## Compare both distances through a DD-plot
distances <- data.frame(c(classic_maha, robust_maha))
plt <- ggplot(distances, aes(x = distances[, 1], y = distances[, 2]))
plt <- plt + geom_point() + geom_vline(xintercept = qchisq(0.95, dim(quantitative_data)[2]), col = "red") 
plt <- plt + geom_hline(yintercept = qchisq(0.95, dim(quantitative_data)[2]), col = "red") 
plt <- plt + labs(x = "Classic Mahalanobis distances", y = "Robust Mahalanobis distances")
ggsave(filename = "products/pdf/compare_maha.pdf")
