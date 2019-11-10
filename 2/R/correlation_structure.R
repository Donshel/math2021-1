# Libraries
library(MASS)
library(qgraph)

# ---------- PART 3.2 : Further investigation of the correlation structure of the quantitative variables ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
quantitative_data <- data[quantitative_cols]

## Compute robust estimation of correlation matrix using MCD estimator (Coverage parameter of 0.75)
## on quantitative variables and compare with classical correlation matrix (do not take time variables
## into account).
classic_corr <- cor(quantitative_data)

COVERAGE <- 0.75
set.seed(0)
robust <- cov.rob(quantitative_data, cor = TRUE, quantile.used = COVERAGE * dim(data)[1], method = "mcd")
robust_cor <- robust$cor

## Graphical model to visualize conditional independence.
# Use classic covariance matrix
classic_cov <- cov(quantitative_data)
qgraph(classic_cov)

# Use L1-regularized covariance matrix






