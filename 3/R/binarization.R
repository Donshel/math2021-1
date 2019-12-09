# Libraries

# ------------- PART 1 : Preliminaries for binary classification -------------- 
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
dim(data)
data <- data[complete.cases(data), ]
dim(data)

attach(data)

summary(data)

pollutants <- c('PM2.5', 'PM10', 'SO2', 'NO2', 'O3')
recommandations <- c(25, 50, 20, 200, 100)

surpass <- function(pollutants, thresholds) {
  unfullfilled_recommendations <- as.data.frame(t(apply(pollutants, 1, function(x) { x > thresholds })))
  unfullfilled_recommendations['alert'] <- apply(unfullfilled_recommendations, 1, 'any')
  return(unfullfilled_recommendations)
}

unfullfilled_recommendations <- surpass(data[, pollutants], recommandations)
exceeding_rates <- apply(unfullfilled_recommendations, 2, mean)
exceeding_rates

# As 80% of day being signaled could seem excessive (due to PM), we could alert only
# when PM surpass MARGIN * WHO_recommandation where we chose MARGIN such that one day 
# of out two is signaled on average.

margins <- c(3, 3, 1, 1, 1)
unfullfilled_recommandations_with_margins <- surpass(data[, pollutants], recommandations * margins)
exceeding_rates <- apply(unfullfilled_recommandations_with_margins, 2, mean)
exceeding_rates

binarized_data <- subset(data, select = -c(PM2.5, PM10, SO2, NO2, O3))
binarized_data['alert'] <- unfullfilled_recommandations_with_margins['alert']
write.table(binarized_data, "products/csv/binarized.csv", sep = ",", quote = FALSE, row.names = FALSE)
