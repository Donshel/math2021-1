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

## Compute outlying rates for both methods
rob_outliers_rate <- sum(robust_maha > qchisq(.95, dim(quantitative_data)[2]))/dim(data)[1]
classical_outliers_rate <- sum(classic_maha > qchisq(.95, dim(quantitative_data)[2]))/dim(data)[1]

## Retrieve outlying observations for both methods
outlying_rob_idx <- robust_maha > qchisq(.95, dim(quantitative_data)[2])
outlying_classic_idx <- classic_maha > qchisq(.95, dim(quantitative_data)[2])

outlying_obs_rob <- quantitative_data[outlying_rob_idx, ]
outlying_obs_classic <- quantitative_data[outlying_classic_idx, ] 

non_outlying <- anti_join(quantitative_data, outlying_obs_rob)
common_outliers <- inner_join(outlying_obs_classic, outlying_obs_rob)
only_robust_outliers <- anti_join(outlying_obs_rob, outlying_obs_classic)

## Plot histograms of quantitative data (outlying and non outlying)
melted <- melt(quantitative_data)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms.pdf", plt)

## Plot histograms of quantitative data (non outlying)
melted <- melt(non_outlying)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms_non_outlying.pdf", plt)

## Plot histograms of common outlying observations
melted <- melt(common_outliers)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms_common_outliers.pdf", plt)

## Plot histograms of robust outlying observations not detected by the classical approach
melted <- melt(only_robust_outliers)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms_robust_only.pdf", plt)

## Plot histograms of all robust outlying observations
melted <- melt(outlying_obs_rob)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms_robust_all.pdf", plt)

## Compute rates for O3: 
# - the rate of outlying observations having a concentration > maximum concentration of non outlying
summary(non_outlying["O3"])
summary(outlying_obs_rob["O3"])
rate_O3_bigger_200_outlying <- sum(outlying_obs_rob["O3"] > 200)/dim(outlying_obs_rob)[1]

## Compute rates for CO: 
# - the rate of non outlying observations having a concentration < 2000
# - the rate of outlying observations having a concentration < 2000
# - the rate of outlying observations having a concentration > maximum concentration of non outlying
summary(non_outlying["CO"])
summary(outlying_obs_rob["CO"])
rate_CO_less_2000_non_outlying <- sum(non_outlying["CO"] < 2000)/dim(non_outlying)[1]
rate_CO_less_2000_outlying <- sum(outlying_obs_rob["CO"] < 2000)/dim(outlying_obs_rob)[1]
rate_CO_bigger_2600_outlying <- sum(outlying_obs_rob["CO"] > 2600)/dim(outlying_obs_rob)[1]

## Compute rates for NO2: 
# - the rate of non outlying observations having a concentration > median concentration of outlying
# - the rate of outlying observations having a concentration > maximum concentration of non outlying
summary(non_outlying["NO2"])
summary(outlying_obs_rob["NO2"])
rate_NO2_bigger_68_non_outlying <- sum(non_outlying["NO2"] > 68)/dim(non_outlying)[1]
rate_NO2_bigger_100_outlying <- sum(outlying_obs_rob["NO2"] > 100)/dim(outlying_obs_rob)[1]

## Compute rates for SO2: 
# - the rate of non outlying observations having a concentration > median concentration of outlying
# - the rate of outlying observations having a concentration > maximum concentration of non outlying
summary(non_outlying["SO2"])
summary(outlying_obs_rob["SO2"])
rate_SO2_bigger_21_non_outlying <- sum(non_outlying["SO2"] > 21)/dim(non_outlying)[1]
rate_SO2_bigger_36_outlying <-  sum(outlying_obs_rob["SO2"] > 36)/dim(outlying_obs_rob)[1]

## Compute rates for PM10: 
# - the rate of non outlying observations having a concentration > median concentration of outlying
# - the rate of outlying observations having a concentration > maximum concentration of non outlying
summary(non_outlying["PM10"])
summary(outlying_obs_rob["PM10"])
rate_PM10_bigger_190.5_non_outlying <- sum(non_outlying["PM10"] > 190.5)/dim(non_outlying)[1]
rate_PM10_bigger_190_outlying <- sum(outlying_obs_rob["PM10"] > 190)/dim(outlying_obs_rob)[1]

## Compute rates for PM2.5: 
# - the rate of non outlying observations having a concentration > median concentration of outlying
# - the rate of outlying observations having a concentration > maximum concentration of non outlying
summary(non_outlying["PM2.5"])
summary(outlying_obs_rob["PM2.5"])
rate_PM2.5_bigger_149_non_outlying <- sum(non_outlying["PM2.5"] > 149)/dim(non_outlying)[1]
rate_PM2.5_bigger_173_outlying <- sum(outlying_obs_rob["PM2.5"] > 173)/dim(outlying_obs_rob)[1]

## Retrieve most extreme observations (6 obs. having distance > 250 on the graph)
most_extreme_obs <- quantitative_data[robust_maha > 250, ]
most_extreme_obs["distance"] <- robust_maha[robust_maha > 250, ]
most_extreme_obs <- most_extreme_obs[order(most_extreme_obs$distance), ]
