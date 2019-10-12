# Libraries
library(ggplot2)
library(corrplot)
library(reshape2)
library(tidyverse)

# ---------- PART 3.2 : Multivariate exploratory analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

## Overview
### Relative pollutants' concentration
pollutants = c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
rel_pollutants <- data
for (p in pollutants) {
  rel_pollutants[, p] <- rel_pollutants[, p] / max(rel_pollutants[, p], na.rm = TRUE) # should be better with sd (standart deviation) instead of max
}

### Overview of the correlation between variables
corr <- cor(data, use = "complete.obs")
pdf("products/pdf/correlation.pdf")
corrplot(corr)
dev.off()

### Pollutant's pairs scatterplots
set.seed(0)
NB_SAMPLES = 100
sampled_indexes = sample(1:nrow(data), NB_SAMPLES)
pdf("products/pdf/pairs_pollutants.pdf")
pairs(data[sampled_indexes, pollutants])
dev.off()


## Temperature
### Relative pollutants' concentration w.r.t. temperatures
data.pollutants <- melt(rel_pollutants[c("temp", pollutants)], id.vars = "temp")
plt <- ggplot(data.pollutants, aes(x = temp, y = value, color = variable)) + geom_point() + 
  geom_smooth() + labs(colour = "Pollutant", y = "Relative Pollutant Concentration", 
  x = "Temperature in °C")
ggsave(filename = "products/pdf/pollutants~temp.pdf", plt)

### Temperature w.r.t. the month
spd <- 3600 * 24 * 365
day_in_year <- (timestamp %% spd) / (3600 * 24)

model <- nls(temp ~ a * sin(2 * pi * day_in_year/365 + b) + c, 
  start = list(a = mean(temp, na.rm = TRUE), b = 0, c = mean(temp, na.rm = TRUE)))
x <- seq(0, 365, length.out = 100)
y <- predict(model, newdata = list(day_in_year = x))
pdf("products/pdf/temp~time.pdf")
plot(day_in_year, temp, pch = 19)
lines(x, y, col = "red", lty = 2, lwd = 3)
dev.off()

### Meteorological dependance to temperature
plt <- ggplot(data, aes(x = temp, y = pres, colour = dewp)) + geom_point() + 
  scale_color_gradient(low = "blue", high = "orange")
ggsave(filename = "products/pdf/pres_dewp~temp.pdf", plt)


## Time
### Relative pollutants' concentrations over the days of the year
data.pollutants <- melt(rel_pollutants[c("month", "day", pollutants)], id.vars = c("month", "day"))
plt <- ggplot(data.pollutants, aes(x = ((month - 1)*27.83 + day), y = value, color = variable)) + 
  geom_point() + geom_smooth() + labs(colour = "Pollutant", y = "Relative Pollutant Concentration",
  x = "Day in Year")
ggsave(filename = "products/pdf/pollutants~months.pdf", plt)

### Relative pollutant's concentration over the hours
data.pollutants <- melt(rel_pollutants[c("hour", pollutants)], id.vars = "hour")
plt <- ggplot(data.pollutants, aes(x = hour, y = value, color = variable)) + geom_point() + 
  geom_smooth() + labs(colour = "Pollutant", y = "Relative Pollutant Concentration", x = "Hour")
ggsave(filename = "products/pdf/pollutants~hour.pdf", plt)


## Wind speed and wind direction
### Relative pollutant's concentration w.r.t. wind speed
data.pollutants <- melt(rel_pollutants[c("wspd", pollutants)], id.vars = "wspd")
plt <- ggplot(data.pollutants, aes(x = wspd, y = value, color = variable)) + geom_point() + 
  geom_smooth() + labs(colour = "Pollutant", y = "Relative Pollutant Concentration", 
  x = "Wind Speed in m/s")
ggsave(filename = "products/pdf/pollutants~wspd.pdf", plt)

### Median temperatures w.r.t. the wind direction
temp_wdir = aggregate(temp ~ wdir, data, median)
plt <- ggplot(temp_wdir, aes(x = as.factor(wdir), y = temp)) +
  geom_bar(stat = "identity", fill = alpha("blue", 0.3)) +
  ylim(-25, 25) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.text.y = element_text(colour = "grey30", margin = margin(l = 0, t = 0, b = 0, r = -260)),
    axis.title = element_blank(),
    plot.margin = margin(l = 260, t = 0, b = 0, r = 0) 
  ) + 
  coord_polar(start = - pi/2 + 1/16/2 * 2*pi, direction = -1)
ggsave(filename = "products/pdf/temp~wdir.pdf", plt)

### Relative median pollutant's concentration w.r.t. the wind direction
data.pollutants <- data[c("wdir", pollutants)]
data.pollutants = aggregate(. ~ wdir, data.pollutants, median)
for (p in pollutants) {
  data.pollutants[, p] <- data.pollutants[, p] / max(data.pollutants[, p], na.rm = TRUE)
}
data.pollutants <- melt(data.pollutants, id = "wdir")

plt <- ggplot(data.pollutants, aes(x = as.factor(wdir), y = value, color = variable)) + 
  geom_bar(stat = "identity", aes(fill = variable), position = "dodge") +
  ylim(-1, 1) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.text.y = element_text(colour = "grey30", margin = margin(l = 0, t = 0, b = 0, r = -230)),
    axis.title = element_blank(),
    plot.margin = margin(l = 230, t = 0, b = 0, r = 0)
  ) + 
  coord_polar(start = - pi/2 + 1/16/2 * 2*pi, direction = -1)
ggsave(filename = "products/pdf/pollutants~wdir.pdf", plt)
