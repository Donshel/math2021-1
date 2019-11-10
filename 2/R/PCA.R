# Libraries
library("FactoMineR")
library("factoextra")

# ---------- PART 3.3.1 : PCA Analysis ----------
data <- read.table("products/csv/data.csv", header = TRUE, na.strings = NA, sep = ",")
data <- data[complete.cases(data), ]
attach(data)

dir.create("products", showWarnings = FALSE)
dir.create("products/pdf", showWarnings = FALSE)

quantitative_cols <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "temp", "pres", "dewp", "wspd")
quantitative_data <- data[quantitative_cols]

## Perform PCA on the correlation matrix (units are too different).
## The non-robust estimation has been chosen, as no big differences can be osberved
## and as robust estimations eliminates outliers, what thus explains less the data.
## 91.75% of variability explained by first two factors (~ in the 2D projection)
corr <- cor(quantitative_data)
PCA_corr <- PCA(corr)
PCA <- PCA(quantitative_data, scale.unit = TRUE)

# Principal components and their corresponding percentage of variance
factors <- PCA$eig
print(factors)

# Plot correlation circle for the first two PCs
plt <- fviz_pca_var(PCA, title = "")
ggsave(filename = "products/pdf/correlation_circle.pdf")


fviz_pca_biplot(PCA, geom.ind = c("point"), geom.var = c("arrow", "text"), habillage = data$rain)
