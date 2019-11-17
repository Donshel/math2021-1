# Libraries
library(MASS)
library(ade4)
library(corrplot)


# ---------- PART 3.3.1 : PCA Analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
quantitative_data <- data[quantitative_cols]

## Perform PCA 
##  - on the correlation matrix (units and scales are too different)
##  - using the non-robust estimation has been chosen, as no big differences can be osberved
##    and as robust estimations eliminates outliers, what thus explains less the data.

# The correlation matrix of the data can be connected with the result of the PCA
corr <- cor(quantitative_data)
corrplot(corr)

# P.C. and their corresponding percentage of total variance (scree plot)
res_PCA <- princomp(quantitative_data, cor = TRUE)
pc_var <- res_PCA$sdev ** 2 # exactly the same as eigen(corr)$values
pdf("products/pdf/pca_scree_plot.pdf")
plot(pc_var/sum(pc_var), type="b", ylab = "Percentage of variance", xlab = "Principal component index")
grid()
dev.off()

# Explainability of the first two P.C.
variance_explained_2d <- (pc_var[1] + pc_var[2])/sum(pc_var)
print(paste("The first two principal components explain", round(variance_explained_2d*100, digits=2), "% of the variance"))

# Loadings of the first two P.C.
loadings <- res_PCA$loadings
pdf("products/pdf/pca_loadings.pdf")
par(mfrow=c(2,1))
for (i in 1:2) {
  barplot(loadings[,i], main=paste("PC", i))
}
par(mfrow=c(1,1))
dev.off()

# Correlation between PCA variables and classic variables
pdf("products/pdf/pca_variables_correlation.pdf")
rescore <- cor(quantitative_data, res_PCA$scores)
corrplot(rescore)
dev.off()

# Percentage of variability of each variable in each P.C.
pdf("products/pdf/pca_explained_variability.pdf")
corrplot(rescore ** 2)
dev.off()

# Correlation Circle
pdf("products/pdf/pca_correlation_circle.pdf")
s.corcircle(rescore[,1:2])
dev.off()
