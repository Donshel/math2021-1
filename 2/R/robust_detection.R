# Libraries
library(MASS)
library(reshape2)
library(ggplot2)
library(hexbin)
library(dplyr)

# ---------- PART 3.1 : Robust Outlier Detection ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
quantitative_data <- data[quantitative_cols]

## Use MCD estimator (Coverage parameter of 0.75)
h <- floor((dim(quantitative_data)[1] + dim(quantitative_data)[2] + 1)/2)
set.seed(0)
robust <- cov.rob(quantitative_data, cor = TRUE, quantile.used = h, method = "mcd")

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
ggsave(filename = "products/pdf/compare_maha_most_robust.pdf")

# Compute outlying rates for both methods
rob_outliers_rate <- sum(robust_maha > qchisq(.95, dim(quantitative_data)[2]))/dim(data)[1]
classical_outliers_rate <- sum(classic_maha > qchisq(.95, dim(quantitative_data)[2]))/dim(data)[1]

# Retrieve outlying observations for both methods
outlying_rob_idx <- robust_maha > qchisq(.95, dim(quantitative_data)[2])
outlying_classic_idx <- classic_maha > qchisq(.95, dim(quantitative_data)[2])

outlying_obs_rob <- quantitative_data[outlying_rob_idx, ]
outlying_obs_classic <- quantitative_data[outlying_classic_idx, ] 

non_outlying <- anti_join(quantitative_data, outlying_obs_rob)
common_outliers <- inner_join(outlying_obs_classic, outlying_obs_rob)
only_robust_outliers <- anti_join(outlying_obs_rob, outlying_obs_classic)

# Plot histograms of quantitative data (outlying and non outlying)
melted <- melt(quantitative_data)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms.pdf", plt)

# Plot histograms of quantitative data (non outlying)
melted <- melt(non_outlying)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms_non_outlying.pdf", plt)

# Plot histograms of common outlying observations
melted <- melt(common_outliers)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms_common_outliers.pdf", plt)

# Plot histograms of robust outlying observations not detected by the classical approach
melted <- melt(only_robust_outliers)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms_robust_only.pdf", plt)

# Plot histograms of all robust outlying observations
melted <- melt(outlying_obs_rob)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms_robust_all.pdf", plt)
