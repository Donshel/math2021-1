# Libraries
library(Rtsne)
library(rgl)

# ---------- PART 3.3.2 : tSNE Analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
quantitative_data <- data[quantitative_cols]

# 3D
tSNE = Rtsne(quantitative_data, dims = 3, perplexity = 10)
plot3d(tSNE$Y[,1], tSNE$Y[,2], tSNE$Y[,3], col = data$rain + 3)

# 2D with outliers highlight
## Twenty most outlying observations (robust mahanalobis distance, see robust_detection.R)
h <- floor((dim(quantitative_data)[1] + dim(quantitative_data)[2] + 1)/2)
robust <- cov.rob(quantitative_data, cor = TRUE, quantile.used = h, method = "mcd")
robust_maha <- mahalanobis(quantitative_data, robust$center, robust$cov)
sorted_maha <- sort(robust_maha, index.return = TRUE)
ten_outlyings <- sorted_maha$ix[1:20]

pdf("products/pdf/tSNE.pdf")
tSNE = Rtsne(quantitative_data)
plot(tSNE$Y, asp = 1, col = 8, xlab = "X1", ylab = "X2")
points(tSNE$Y[ten_outlyings,], col = "red")
grid()
dev.off()
