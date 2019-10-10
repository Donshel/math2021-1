# ---------- Libraries ----------

# ---------- Part 1 : Data handling ----------
data <- read.table("resources/csv/data.csv", header = TRUE, na.strings = "NA", sep = ",")

## Remove "station" variable
data <- subset(data, select = -c(station))

## Sample randomly 500 rows
set.seed(0)
data <- data[sample.int(nrow(data), 500),]

## Convert "year", "month", "day" and "hour" to a "timestamp" variable and replace "index"
data$index <- as.numeric(strptime(paste(data$year, data$month, data$day, data$hour, sep = "-"), "%Y-%m-%d-%H"))
colnames(data)[1] <- "timestamp"

## Convert the compass-like wind direction ("wdir") to sine ("wdir_sin") and cosine ("wdir_cos")
compass <- c("E", "ENE", "NE", "NNE", "N", "NNW", "NW", "WNW", "W", "WSW", "SW", "SSW", "S", "SSE", "SE", "ESE")
angles <- c(seq(0, 15, 1) * pi / 8)
data$wdir <- angles[match(data$wdir, compass)]

## Binarization of rain
data$rain <- data$rain > 0

## Write table
dir.create("products", showWarnings = FALSE)
dir.create("products/csv", showWarnings = FALSE)
write.table(data, "products/csv/data.csv", sep = ",", quote = FALSE, row.names = FALSE)
