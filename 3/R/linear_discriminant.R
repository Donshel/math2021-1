# Library
library(corrplot)
library(MASS)

# ------------- PART 5 : Linear Discriminant Analysis -------------- 
data <- read.table("products/csv/binarized.csv", header = TRUE, na.strings = NA, sep = ",")
attach(data)

# 1.

quantitative_var <- c('year', 'month', 'temp', 'pres', 'dewp', 'rain', 'wspd', 'wdir')
quantitative_data <- scale(data[, quantitative_var]) # scaled !

g <- 2
n <- dim(data)[1]

lda_full <- lda(x=quantitative_data, grouping=alert)

## Canonical variable
print(lda_full$scaling)

## Scores
scores <- as.matrix(quantitative_data) %*% cbind(lda_full$scaling)

pdf("products/pdf/lda_full_scatter.pdf")
plot(scores, alert)
dev.off()

pdf("products/pdf/lda_full_boxplot.pdf")
boxplot(scores ~ alert)
dev.off()

## Power
l1 <- (g - 1) * lda_full$svd[1]^2 / n
gamma1 <- l1 / (1 + l1)
print(gamma1)

# 2. Leave one out
p <- length(quantitative_var)

gamma = rep(0, p)

for (i in 1:p) {
    index <- rep(TRUE, p)
    index[i] = FALSE

    lda_partial <- lda(x=quantitative_data[, index], grouping=alert)
    l <- (g - 1) * lda_partial$svd[1]^2 / n
    gamma[i] <- l / (1 + l)
}

cbind(quantitative_var, gamma)

quantitative_var <- c('month', 'temp', 'pres', 'dewp', 'wspd', 'wdir')
quantitative_data <- scale(data[, quantitative_var]) # scaled !

lda_final <- lda(x=quantitative_data, grouping=alert)

## Canonical variable
print(lda_final$scaling)

## Power
l <- (g - 1) * lda_final$svd[1]^2 / n
gamma <- l / (1 + l)
print(gamma)

## Scores
scores <- as.matrix(quantitative_data) %*% cbind(lda_final$scaling)

# 3. Classification

## Prior
table(alert) / length(alert)

## Leave-one-out

pred <- rep(0, n)

for (i in 1:n) {
    index <- rep(TRUE, n)
    index[i] <- FALSE

    lda <- lda(x=quantitative_data[index,], grouping=alert[index])

    scores <- as.matrix(quantitative_data[index,]) %*% cbind(lda$scaling)

    g0 <- scores[alert[index] == 0]
    g1 <- scores[alert[index] == 1]

    mu0 <- mean(g0)
    mu1 <- mean(g1)

    z <- sum(quantitative_data[i,] * cbind(lda$scaling))

    pred[i] <- abs(z - mu1) < abs(z - mu0)
}

## Confusion matrix
conf_mat <- table(alert, pred)

print(conf_mat)

# 4. Homoscedasticity

cor <- cor(quantitative_data[alert == 0,])
pdf("products/pdf/corr_group_false.pdf")
corrplot(cor, method = "color", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

cor <- cor(quantitative_data[alert == 1,])
pdf("products/pdf/corr_group_true.pdf")
corrplot(cor, method = "color", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

var(g0)
var(g1)
