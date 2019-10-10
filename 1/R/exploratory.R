# ---------- PART 3 : Exploratory analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
attach(data)

## Covariances and correlations
cova <- cov(data, use = "complete.obs")
core <- cor(data, use = "complete.obs")

## Pollutants dependance to temperature
pdf("products/pdf/temp_PM25.pdf")
plot(temp, PM2.5)
dev.off()

pdf("products/pdf/temp_PM10.pdf")
plot(temp, PM10)
dev.off()

pdf("products/pdf/temp_SO2.pdf")
plot(temp, SO2)
dev.off()

pdf("products/pdf/temp_NO2.pdf")
plot(temp, NO2)
dev.off()

pdf("products/pdf/temp_CO.pdf")
plot(temp, CO)
dev.off()

pdf("products/pdf/temp_O3.pdf")
plot(temp, O3)
dev.off()

## Meteorological dependance to temperature
pdf("products/pdf/temp_pres.pdf")
plot(temp, dewp)
dev.off()

pdf("products/pdf/temp_dewp.pdf")
plot(temp, dewp)
dev.off()

pdf("products/pdf/temp_dewp.pdf")
plot(temp, dewp)
dev.off()

## Temperature dependance to seasons
seconds <- 24 * 3600 * 365
t <- (timestamp %% seconds) / seconds

pdf("products/pdf/time_temp.pdf")

plot(t, temp)

model <- nls(temp ~ a * sin(2 * pi * t + b) + c, start = list(a = mean(temp, na.rm = TRUE), b = 0, c = mean(temp, na.rm = TRUE)))
x <- seq(0, 1, length.out = 100)
y <- predict(model, newdata = list(t = x))
lines(x, y, col = "red", lty = 2)

dev.off()
