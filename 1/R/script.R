# ---------- Libraries ----------
library(ggplot2)
library(dplyr)
library(naniar)
library(hash)

# ---------- Part 1 : Data handling ----------
data <- read.table("resources/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")

## Remove "station" variable
data <- subset(data, select = -c(station))

## Sample randomly 500 rows
set.seed(0)
data <- data[sample.int(nrow(data), 500),]
attach(data)

## Convert "year", "month", "day" and "hour" to a "timestamp" variable and replace "index"
data$index <- as.numeric(strptime(paste(year, month, day, hour, sep = "-"), "%Y-%m-%d-%H"))
colnames(data)[1] <- "timestamp"

## Convert the compass-like wind direction ("wdir") to sine ("wdir_sin") and cosine ("wdir_cos")
compass <- c("E", "ENE", "NE", "NNE", "N", "NNW", "NW", "WNW", "W", "WSW", "SW", "SSW", "S", "SSE", "SE", "ESE")
angles <- c(seq(0, 15 / 16, length.out = 16) * 2 * pi)
data$wdir <- angles[match(wdir, compass)]
attach(data)
data$wdir_sin <- sin(wdir)
data$wdir_cos <- cos(wdir)
data <- subset(data, select = -c(wdir))

## Binarization of rain
data$rain <- rain > 0
attach(data)
print(table(rain) / nrow(data))

# ---------- PART 2 : Missingness ----------

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

pdf("products/pdf/vis_miss.pdf")
vis_miss(data)
dev.off()

pdf("products/pdf/gg_miss_upset.pdf")
gg_miss_upset(data)
dev.off()

## Matrice of mean-ratios
M <- cor(data)
for (i in 1:ncol(data)) {
	a <- data[,i]
	for (j in 1:ncol(data)) {
		b <- data[,j]
		if (i != j & sum(is.na(b)) > 0) {
			M[i, j] <- mean(a[is.na(b)], na.rm = TRUE) / mean(a[!is.na(b)], na.rm = TRUE)
		} else {
			M[i, j] <- 0
		}
	}
}
print(M)

pdf("products/pdf/ggplot.pdf")
ggplot(data, aes(x = O3, y = NO2)) + geom_miss_point()
dev.off()

pdf("products/pdf/boxplot.pdf")
boxplot(NO2[!is.na(O3)], NO2[is.na(O3)])
dev.off()

# ---------- PART 3 : Exploratory analysis ----------
print(cor(data, use = "pairwise.complete.obs"))
