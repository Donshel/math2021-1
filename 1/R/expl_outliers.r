# Libraries
library(ggplot2)
library(reshape2)

# ---------- PART 3.4 : Outliers Detection ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
quantitative_data <- data[, quantitative_cols]

# Z-scores
melted <- melt(data.frame(scale(quantitative_data)))
plt <- ggplot(melted, aes(x = variable, y = value))
plt <- plt + geom_point(alpha = 1/3)
plt <- plt + labs(x = "Variable", y = "Z-score")
plt <- plt + geom_hline(yintercept = 1.96, linetype = "dashed", color = "red")
plt <- plt + geom_hline(yintercept = -1.96, linetype = "dashed", color = "red")
ggsave("products/pdf/z_scores.pdf", plt)

zs_outliers <- apply(abs(scale(quantitative_data)) > 1.96, 2, sum)

# Mahalanobis distance
mean_vec <- colMeans(quantitative_data)
cova <- cov(quantitative_data)
maha <- mahalanobis(quantitative_data, mean_vec, cova)

temp <- as.data.frame(matrix(maha, ncol = 1))
temp$index = as.numeric(rownames(temp))

plt <- ggplot(temp, aes(x = index, y = V1))
plt <- plt + geom_bar(stat = "identity")
plt <- plt + labs(x = "Index", y = "Mahalanobis distance")
plt <- plt + geom_hline(yintercept = qchisq(0.95, length(quantitative_cols)), linetype = "dashed", color = "red")
ggsave("products/pdf/mahalanobis.pdf", plt)

maha_outliers <- sum(temp$V1 > qchisq(0.95, length(quantitative_cols)))

plt <- ggplot(temp, aes(y = V1))
plt <- plt + geom_boxplot()
plt <- plt + labs(x = "",  y = "Mahalanobis distance")
plt <- plt + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave("products/pdf/mahalanobis_boxplot.pdf", plt)
