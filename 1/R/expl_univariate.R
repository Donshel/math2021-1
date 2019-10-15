# Libraries
library(ggplot2)
library(reshape2)

# ---------- PART 3.1 : Univariate exploratory analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
data.histograms <- melt(data[, quantitative_cols])
plt <- ggplot(data.histograms, aes(x = value)) + geom_histogram()
plt <- plt+ facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("products/pdf/histograms.pdf", plt)
