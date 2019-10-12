# Libraries
library(ggplot2)

# ---------- PART 3.1 : Univariate exploratory analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

columns <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
for (col in columns) {
    plt <- qplot(data[, col], geom = "histogram", xlab = col)
    ggsave(paste("products/pdf/hist_", col, '.pdf', sep = ""), plt)
}

