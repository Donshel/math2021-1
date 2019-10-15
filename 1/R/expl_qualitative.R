# Libraries
library(ggplot2)
library(reshape2)

# ---------- PART 3.3 : Qualitative variables impact on quantitative one ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

pollutants <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
atmospherical <- c("temp", "pres", "wspd", "dewp")

# Pollutants' concentrations w.r.t. the rain
data.pollutants <- data[c("rain", pollutants)]
data.pollutants <- melt(data.pollutants, id.vars = "rain")
plt <- ggplot(data = data.pollutants, aes(x = rain, y = value)) + geom_boxplot() 
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "Rain", y = "")
ggsave(filename = "products/pdf/pollutants_rain.pdf", plt)


# Atmospherical conditions w.r.t. the rain
data.pollutants <- data[c("rain", atmospherical)]
data.pollutants <- melt(data.pollutants, id.vars = "rain")
plt <- ggplot(data = data.pollutants, aes(x = rain, y = value)) + geom_boxplot() 
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "Rain", y = "")
ggsave(filename = "products/pdf/atmospherical_rain.pdf", plt)
