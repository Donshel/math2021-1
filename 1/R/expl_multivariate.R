# Libraries
library(ggplot2)
library(corrplot)
library(reshape2)

# ---------- PART 3.2 : Multivariate exploratory analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

## Overview
### Z-Scores of pollutant concentrations
pollutants <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
z_pollutants <- data
z_pollutants[pollutants] <- scale(z_pollutants[pollutants])

### Overview of the correlation between variables
corr <- cor(data)
pdf("products/pdf/correlation.pdf")
corrplot(corr, method = "color", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

### Pollutant's pairs scatterplots
set.seed(0)
NB_SAMPLES <- 100
sampled_indexes <- sample(1:nrow(data), NB_SAMPLES)
pdf("products/pdf/pairs_pollutants.pdf")
pairs(data[sampled_indexes, pollutants])
dev.off()

## Temperature
### Z-Scores of pollutant concentrations w.r.t. temperatures
NB_MAX_STD <- 3 # y limit of 3 times the std for plots
limits = c(max(min(z_pollutants[pollutants]), -3), min(max(z_pollutants[pollutants]), 3))
pollutants_temp <- melt(z_pollutants[c("temp", pollutants)], id.vars = "temp")

plt <- ggplot(pollutants_temp, aes(x = temp, y = value, color = variable)) 
plt <- plt + geom_point() + geom_smooth(aes(fill = variable)) + coord_cartesian(ylim = limits)
plt <- plt + labs(
    colour = "Pollutant",
    fill = "Pollutant",
    y = "Z-Scores of Pollutant Concentrations [-]", 
    x = expression("Temperature ["*degree*"C]")
)
ggsave(filename = "products/pdf/pollutants_temp.pdf", plt)

### Temperature w.r.t. the month
spd <- 3600 * 24 * 365
day_in_year <- (timestamp %% spd) / (3600 * 24)
temp_month = data.frame(temperature = temp, day = day_in_year)

plt <- ggplot(temp_month, aes(x = day_in_year, y = temperature))
plt <- plt + geom_point() + geom_smooth()
plt <- plt + labs(y = expression("Temperature ["*degree*"C]"), x = "Day of the Year")
ggsave("products/pdf/temp_months.pdf", plt)

### Meteorological dependance to temperature
plt <- ggplot(data, aes(x = pres, y = dewp, color = temp))
plt <- plt + geom_point() + scale_color_gradient(low = "blue", high = "orange")
plt <- plt + labs(x = "Pressure [hPa]", y = expression("Dew point ["*degree*"C]"), color = expression("Temp. ["*degree*"C]"))
ggsave(filename = "products/pdf/pres_dewp_temp.pdf", plt)

## Time
### Z-Scores of pollutant concentrations over the days of the year
pollutants_months <- melt(z_pollutants[c("month", "day", pollutants)], id.vars = c("month", "day"))

plt <- ggplot(pollutants_months, aes(x = ((month - 1)*27.83 + day), y = value, color = variable))
plt <- plt + geom_point() + geom_smooth(aes(fill = variable)) + coord_cartesian(ylim = limits)
plt <- plt + labs(
    fill = "Pollutant",
    colour = "Pollutant",
    y = "Z-Scores of Pollutant Concentrations [-]",
    x = "Day in Year"
) 
ggsave(filename = "products/pdf/pollutants_months.pdf", plt)

### Relative pollutant's concentration over the hours
pollutants_hours <- melt(z_pollutants[c("hour", pollutants)], id.vars = "hour")
plt <- ggplot(pollutants_hours, aes(x = hour, y = value, color = variable)) 
plt <- plt + geom_point() + geom_smooth(aes(fill = variable)) + coord_cartesian(ylim = limits)
plt <- plt + labs(
    fill = "Pollutant",
    colour = "Pollutant",
    y = "Z-Scores Pollutant Concentrations [-]",
    x = "Hour"
)
ggsave(filename = "products/pdf/pollutants_hour.pdf", plt)

## Wind speed and wind direction
### Relative pollutant's concentration w.r.t. wind speed
pollutants_wspd <- melt(z_pollutants[c("wspd", pollutants)], id.vars = "wspd")
plt <- ggplot(pollutants_wspd, aes(x = wspd, y = value, color = variable))
plt <- plt + geom_point() + geom_smooth(aes(fill = variable)) + coord_cartesian(ylim=limits)
plt <- plt + labs(
    fill = "Pollutant",
    colour = "Pollutant",
    y = "Z-Scores of Pollutant Concentrations [-]", 
    x = "Wind Speed [m/s]"
)
ggsave(filename = "products/pdf/pollutants_wspd.pdf", plt)

### Median wind speed w.r.t. the wind direction
medianwspd_wdir <- aggregate(wspd ~ wdir, data, median)
max_medianwspd <- max(medianwspd_wdir["wspd"])

plt <- ggplot(medianwspd_wdir, aes(x = as.factor(wdir), y = wspd))
plt <- plt + geom_bar(stat = "identity", fill = alpha("blue", 0.3))
plt <- plt + ylim(- 1.15 * max_medianwspd, 1.15 * max_medianwspd)
plt <- plt + theme_minimal() + theme(
    axis.text = element_blank(),
    axis.text.y = element_text(colour = "grey30", margin = margin(l = 0, t = 0, b = 0, r = -230)),
    axis.title = element_blank(),
    plot.margin = margin(l = 230, t = 0, b = 0, r = 0) 
)
plt <- plt + coord_polar(start = - pi/2 + 1/16/2 * 2*pi, direction = -1)
ggsave(filename = "products/pdf/medianwspd_wdir.pdf", plt)

### Median temperatures w.r.t. the wind direction
mediantemp_wdir <- aggregate(temp ~ wdir, data, median)
limits <- max(abs(mediantemp_wdir["temp"]))

plt <- ggplot(mediantemp_wdir, aes(x = as.factor(wdir), y = temp))
plt <- plt + geom_bar(stat = "identity", fill = alpha("blue", 0.3))
plt <- plt + ylim(-1.15*limits, 1.15*limits)
plt <- plt + theme_minimal() + theme(
    axis.text = element_blank(),
    axis.text.y = element_text(colour = "grey30", margin = margin(l = 0, t = 0, b = 0, r = -230)),
    axis.title = element_blank(),
    plot.margin = margin(l = 230, t = 0, b = 0, r = 0) 
)
plt <- plt + coord_polar(start = - pi/2 + 1/16/2 * 2*pi, direction = -1)
ggsave(filename = "products/pdf/mediantemp_wdir.pdf", plt)

### Scaled (by std) median pollutant's concentration w.r.t. the wind direction
medianpollutants_wdir <- data[c("wdir", pollutants)]
scaled <- scale(medianpollutants_wdir[pollutants], center = FALSE)
medianpollutants_wdir[pollutants] <- scaled

medianpollutants_wdir <- aggregate(. ~ wdir, medianpollutants_wdir, median)
medianpollutants_wdir <- melt(medianpollutants_wdir[c("wdir", pollutants)], id = "wdir")
limits <- max(abs(medianpollutants_wdir["value"]))

plt <- ggplot(medianpollutants_wdir, aes(x = as.factor(wdir), y = value)) 
plt <- plt + geom_bar(stat = "identity", aes(fill = variable), position = "dodge")
plt <- plt + labs(color = "Poluttants", fill = "Pollutants")
plt <- plt + ylim(-1.15 * limits, 1.15 * limits)
plt <- plt + theme_minimal()
plt <- plt + theme(
    axis.text = element_blank(),
    axis.text.y = element_text(colour = "grey30", margin = margin(l = 0, t = 0, b = 0, r = -230)),
    axis.title = element_blank(),
    plot.margin = margin(l = 230, t = 0, b = 0, r = 0)
)
plt <- plt + coord_polar(start = - pi/2 + 1/16/2 * 2*pi, direction = -1)
ggsave(filename = "products/pdf/medianpollutants_wdir.pdf", plt)
