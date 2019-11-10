# Libraries
library(MASS)
library(qgraph)
library(huge)

# ---------- PART 3.2 : Further investigation of the correlation structure of the quantitative variables ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
data <- data[complete.cases(data), ]
attach(data)
n <- dim(data)[1]
d <- dim(data)[2]

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
robust_corr <- robust$cor


## Graphical model to visualize conditional independence (set fade to FALSE in order to see all edges)
# Use classic covariance matrix
classic_cov <- cov(quantitative_data)
qgraph(classic_cov, fade = FALSE, edge.labels = TRUE, filetype = "pdf", filename = "products/pdf/conditional_dep_S")

# Choose optimal lambda for L1 regularization with BIC method
seqlambda <- seq(0,1, length.out=10)
BIC <- NULL
for(lambda in seqlambda)
{
  l1reg <- huge(classic_cov, lambda, method="glasso", cov.output= TRUE)
  l1prec <- solve(as.matrix(l1reg$cov[[1]])) ; l1loglik <- l1reg$loglik * n/2
  BIC <- c(BIC, -2*l1loglik +
             (d + sum(upper.tri(l1prec, diag=TRUE) != 0) * log(n)))
}
lambda <- seqlambda[which.min(BIC)]

# Perfom L1 regularization with optimal lambda and plot resulting graph
# Only slight changes can be noticed in the edges' weights.
l1reg <-  huge(classic_cov, lambda, method = "glasso", cov.output = TRUE)
reg_cov <- l1reg$cov[[1]]
rownames(reg_cov) <- rownames(classic_cov)
colnames(reg_cov) <- colnames(classic_cov)
qgraph(reg_cov, fade = FALSE, edge.labels = TRUE, filetype = "pdf", filename = "products/pdf/conditional_dep_regu")
