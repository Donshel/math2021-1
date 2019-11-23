# Libraries

# ------------- PART 3 : Logistic Discriminant -------------- 
data <- read.table("products/csv/binarized.csv", header = TRUE, na.strings = NA, sep = ",")
attach(data)

# use of quantitative variables instead of all explanatory (discrete, quantitative, binary, etc)
quantitative_var <- c('year', 'month', 'temp', 'pres', 'dewp', 'rain', 'wspd')
