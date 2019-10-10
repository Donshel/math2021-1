# Libraries
library(corrplot)
library(ggplot2)

# ---------- PART 3 : Exploratory analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

## Covariances and correlations
cova <- cov(data, use = "complete.obs")
corr <- cor(data, use = "complete.obs")

pdf("products/pdf/corr.pdf")
corrplot(corr)
dev.off()

## Pollutants dependance to temperature
for (x in c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")) {
	plt <- ggplot(data, aes(x = temp, y = data[, x])) + labs(y = x) + geom_point()
	ggsave(filename = paste("products/pdf/temp_", x, ".pdf", sep = ""), plt)
}

## Meteorological dependance to temperature
for (x in c("pres", "dewp")) {
	plt <- ggplot(data, aes(x = temp, y = data[, x])) + labs(y = x) + geom_point()
	ggsave(filename = paste("products/pdf/temp_", x, ".pdf", sep = ""), plt)
}

plt <- ggplot(data, aes(x = pres, y = dewp, colour = temp)) + geom_point() + scale_color_gradient(low = "blue", high = "orange")
ggsave(filename = "products/pdf/pres_dewp.pdf", plt)

## Temperature dependance to seasons
seconds <- 24 * 3600 * 365
t <- (timestamp %% seconds) / seconds

model <- nls(temp ~ a * sin(2 * pi * t + b) + c, start = list(a = mean(temp, na.rm = TRUE), b = 0, c = mean(temp, na.rm = TRUE)))
x <- seq(0, 1, length.out = 100)
y <- predict(model, newdata = list(t = x))

pdf("products/pdf/time_temp.pdf")
plot(t, temp, pch = 19)
lines(x, y, col = "red", lty = 2, lwd = 2)
dev.off()
