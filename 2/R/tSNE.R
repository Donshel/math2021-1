# Libraries
library(Rtsne)

# ---------- PART 3.3.2 : tSNE Analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
quantitative_data <- data[quantitative_cols]

tSNE = Rtsne(quantitative_data)
plot(tSNE$Y, asp = 1, col = data$rain + 3)
