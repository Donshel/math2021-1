# ---------- Libraries ----------
library(naniar)
library(plotrix)
library(colorspace)

# ---------- PART 2 : Missingness ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

## Naniar plots
pdf("products/pdf/missingness_visualization.pdf")
vis_miss(data)
dev.off()

pdf("products/pdf/missingness_patterns.pdf")
gg_miss_upset(data)
dev.off()

## Matrices of z-scores
columns = colnames(data)
columns_miss = columns[colSums(is.na(data)) > 0]

M <- matrix(NA, length(columns), length(columns_miss))
rownames(M) = columns
colnames(M) = columns_miss

for (i in 1:length(columns_miss)) {
	means <- apply(data, 2, function (x) mean(x, na.rm = TRUE)) 
	stds <- apply(data, 2, function (x) sd(x, na.rm = TRUE))

	indexes_NA = is.na(data[columns_miss[i]])
	means_NA <- apply(data[indexes_NA, ], 2, function (x) mean(x, na.rm = TRUE))
	M[, i] <- (means_NA - means) / stds
}

pdf("products/pdf/zs(mean(Y|X=NA)).pdf")
color2D.matplot(M, extremes = c("lightblue", "orange"), na.color = 'white', show.legend = TRUE,
	show.values = TRUE, axes = FALSE, xlab = "X", ylab = "Y", yrev = TRUE)
axis(1, at = 0.5:(ncol(M)-.5), labels = colnames(M))
axis(2, at = (nrow(M)-.5):0.5, labels = rownames(M), las = 1)
dev.off()

## Missingness rate for each columns
pdf("products/pdf/missingness_rates.pdf")
nb_na <- colSums(is.na(data[, columns_miss]))
barplot(nb_na/nrow(data), legend.text = nb_na, col = rainbow_hcl(length(columns_miss)))
dev.off()

## Conditional boxplots of variables likely to explain the missingness of other variables (MAR) 
## ... according to the discussion made from the heatmap of zscore and the missingness rates