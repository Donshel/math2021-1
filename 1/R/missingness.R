# ---------- Libraries ----------
library(naniar)
library(plotrix)
library(ggplot2)
library(colorspace)

# ---------- PART 2 : Missingness ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

## Missingness visualization
pdf("products/pdf/missingness_visualization.pdf")
vis_miss(data)
dev.off()

## Missingness 
columns <- colnames(data)
columns_miss <- columns[colSums(is.na(data)) > 0]
pdf("products/pdf/missingness_patterns.pdf")
gg_miss_upset(data, nset = length(columns_miss))
dev.off()

## Missingness rate for each columns
pdf("products/pdf/missingness_rates.pdf")
nb_na <- colSums(is.na(data[, columns_miss]))
barplot(nb_na / nrow(data), legend.text = nb_na, col = rainbow_hcl(length(columns_miss)))
dev.off()

## Z(i, j) = z-score of mean(i | is.na(j)) as an estimator of mean(i)
Z <- matrix(NA, length(columns), length(columns_miss))
rownames(Z) <- columns
colnames(Z) <- columns_miss
means <- apply(data, 2, function (x) mean(x, na.rm = TRUE))
stds <- apply(data, 2, function (x) sd(x, na.rm = TRUE))
for (i in 1:length(columns_miss)) {
    indexes_NA <- is.na(data[columns_miss[i]])
    means_NA <- apply(data[indexes_NA, ], 2, function (x) mean(x, na.rm = TRUE))
    n_NA <- sum(indexes_NA)
    Z[, i] <- (means_NA - means) / (stds / sqrt(n_NA))
}

pdf("products/pdf/zs_mean_NA.pdf")
color2D.matplot(Z, extremes = c("deepskyblue1", "coral1"), na.color = 'white', show.legend = TRUE,
    show.values = TRUE, axes = FALSE, xlab = "X", ylab = "Y", yrev = TRUE)
axis(1, at = 0.5:(ncol(Z) - .5), labels = colnames(Z))
axis(2, at = (nrow(Z) - .5):0.5, labels = rownames(Z), las = 1)
dev.off()

## Conditional boxplots of variables likely to explain the missingness of other variables (MAR) 
wspd_wdirmissing <- data.frame(wspd = data["wspd"])
wspd_wdirmissing["wdir_missing"] = is.na(wdir)
plt <- ggplot(data = data, aes(x = is.na(wdir), y = wspd)) + geom_boxplot()
plt <- plt + labs(x = "Missingness of Wind Direction", y = "Wind Speed [m/s]")
ggsave("products/pdf/wspd_wdirmiss.pdf", plt)
