# Libraries
library(corrplot)
library(ggplot2)
library(MASS)
library(qgraph)
library(huge)

# ---------- PART 3.2 : Further investigation of the correlation structure of the quantitative variables ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
quantitative_data <- data[quantitative_cols]

## 1. Robust correlation matrix
set.seed(0)
h <- floor(0.75 * dim(data)[1])
robust <- cov.rob(quantitative_data, cor = TRUE, quantile.used = h, method = "mcd")

pdf("products/pdf/classic_correlation.pdf")
corrplot(cor(quantitative_data), method = "color", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

pdf("products/pdf/robust_correlation.pdf")
corrplot(robust$cor, method = "color", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

## 2. Graphical models

### 2.a Classic covariance
classic_cov <- cov(quantitative_data)
qgraph(classic_cov, fade = FALSE, edge.labels = TRUE, filetype = "pdf", filename = "products/pdf/qgraph_classic_cov")

### 2.b L1-regularized covariance

#### Optimal lambda using BIC method
lambda <- seq(0, 1, length.out = 10)
BIC <- rep(0, 10)

n <- dim(quantitative_data)[1]
p <- dim(quantitative_data)[2]

for (i in 1:length(lambda)) {
	l1reg <- huge(classic_cov, lambda[i], method = "glasso", cov.output = TRUE)
	l1prec <- solve(as.matrix(l1reg$cov[[1]]))
	BIC[i] <- -n * l1reg$loglik +
		log(n) * (p + sum(l1prec[upper.tri(l1prec, diag = TRUE)] != 0))
}

plt <- ggplot() + geom_line(aes(x = lambda, y = BIC))
ggsave(filename = "products/pdf/optimal_lambda.pdf", plt)

lambda <- lambda[which.min(BIC)]

#### L1-regularization with optimal lambda
l1reg <- huge(classic_cov, lambda, method = "glasso", cov.output = TRUE)
l1_cov <- l1reg$cov[[1]]
rownames(l1_cov) <- rownames(classic_cov)
colnames(l1_cov) <- colnames(classic_cov)

qgraph(l1_cov, fade = FALSE, edge.labels = TRUE, filetype = "pdf", filename = "products/pdf/qgraph_l1_cov")
