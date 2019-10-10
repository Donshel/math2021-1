# ---------- Libraries ----------
library(naniar)

# ---------- PART 2 : Missingness ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

## Naniar plots
pdf("products/pdf/vis_miss.pdf")
vis_miss(data)
dev.off()

pdf("products/pdf/gg_miss_upset.pdf")
gg_miss_upset(data)
dev.off()

## Matrices of moment's ratios
mat <- matrix(rep(0, ncol(data) * ncol(data)), ncol = ncol(data))
colnames(mat) <- names(data)
rownames(mat) <- names(data)

M <- mat
V <- mat

for (i in 1:ncol(data)) {
	a <- data[,i]
	for (j in 1:ncol(data)) {
		b <- data[,j]
		if (i != j & sum(is.na(b)) > 0) {
			M[i, j] <- mean(a[is.na(b)], na.rm = TRUE) / mean(a[!is.na(b)], na.rm = TRUE)
			V[i, j] <- var(a[is.na(b)], na.rm = TRUE) / var(a[!is.na(b)], na.rm = TRUE)
		} else {
			M[i, j] <- NA
			V[i, j] <- NA
		}
	}
}
