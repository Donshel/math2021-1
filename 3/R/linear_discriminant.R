# Libraries

# ------------- PART 3 : Logistic Discriminant -------------- 
data <- read.table("products/csv/binarized.csv", header = TRUE, na.strings = NA, sep = ",")
attach(data)

# use of quantitative variables instead of all explanatory (discrete, quantitative, binary, etc)
quantitative_var <- c('year', 'month', 'temp', 'pres', 'dewp', 'rain', 'wspd', 'wdir')

linear_discriminant <- lda(formula = alert ~ year + month + temp + pres + dewp + rain + wspd + wdir)
print(linear_discriminant)
prop_discriminant = linear_discriminant$svd^2/sum(linear_discriminant$svd^2)
print(prop_discriminant)

lda_scores <- as.matrix(data[, quantitative_var]) %*% as.matrix(linear_discriminant$scaling)
c_lda_scores <- scale(lda_scores, center = TRUE, scale = FALSE)
plot(c_lda_scores, alert)

g <- 2
n <- dim(data)[1]

lda <- lda(formula = alert ~ month + temp + pres + dewp + rain + wspd + wdir)
lambda_1 <- (g-1) * lda$svd**2
lambda_1 / (1 + lambda_1)
lda <- lda(formula = alert ~ year + temp + pres + dewp + rain + wspd + wdir)
lambda_1 <- (g-1) * lda$svd**2
lambda_1 / (1 + lambda_1)
lda <- lda(formula = alert ~ year + month + pres + dewp + rain + wspd + wdir)
lambda_1 <- (g-1) * lda$svd**2
lambda_1 / (1 + lambda_1)
lda <- lda(formula = alert ~ year + month + temp + dewp + rain + wspd + wdir)
lambda_1 <- (g-1) * lda$svd**2
lambda_1 / (1 + lambda_1)
lda <- lda(formula = alert ~ year + month + temp + pres + rain + wspd + wdir)
lambda_1 <- (g-1) * lda$svd**2
lambda_1 / (1 + lambda_1)
lda <- lda(formula = alert ~ year + month + temp + pres + dewp + wspd + wdir)
lambda_1 <- (g-1) * lda$svd**2
lambda_1 / (1 + lambda_1)
lda <- lda(formula = alert ~ year + month + temp + pres + dewp + rain + wdir)
lambda_1 <- (g-1) * lda$svd**2
lambda_1 / (1 + lambda_1)
lda <- lda(formula = alert ~ year + month + temp + pres + dewp + rain + wspd)
lambda_1 <- (g-1) * lda$svd**2
lambda_1 / (1 + lambda_1)

# weird results for lambda_1....

