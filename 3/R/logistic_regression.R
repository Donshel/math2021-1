# Libraries
library(corrplot)
library(car)
library(MASS)

# ------------- PART 2 : Logistic Regression Model -------------- 
data <- read.table("products/csv/binarized.csv", header = TRUE, na.strings = NA, sep = ",")

# Part 1 : Finding a good logistic model

explanatory_var <- c('year', 'month', 'temp', 'pres', 'dewp', 'rain', 'wspd', 'wdir')

## Convert the compass-like wind direction ("wdir") to radians from east direction, going counter-clockwise 
compass <- c("E", "ENE", "NE", "NNE", "N", "NNW", "NW", "WNW", "W", "WSW", "SW", "SSW", "S", "SSE", "SE", "ESE")
angles <- c(seq(0, 15, 1) * pi / 8)
data$wdir <- angles[match(data$wdir, compass)]
attach(data)

linear_classification <- glm(alert ~ year + month + temp + pres + dewp + rain + wspd + wdir, family = binomial(link = "logit"))
summary(linear_classification)
# From the summary, we see that all explanatory variables seem to have an importance influence on the logistic model.
# The last column (Pr(> |z|)) represents the probability that the corresponding coefficient should be equal to 0
# instead of the 'estimate' value. From these probabilities, we cannot conclude to the exclusion of a particular
# variable, and thus we cannot conclude to the non-significance any variable.

# Draw correlation plot of the explanatory variables
corrplot(cor(data[, explanatory_var]))
# From the correlation plot, we can clearly see strong correlation between the atmospherical variables.
# A solution would be to use principal component analysis to get rid of this multicolinearity.
# BUT IS MULTICOLINEARITY A REAL PROBLEM HERE?????

# Compute VIF scores to check for multicolinearity
VIF_scores <- vif(linear_classification)
print(VIF_scores)
# From these scores, we can again notice multicolinearity between the atmospherical variables.
# VIF scores look problematic for dewp and temp, as they are bigger than 10.

# Conduct variable selection using backward selection.
# The minimal AIC is obtained when no variables are removed.
stepAIC(linear_classification)

# Conduct variable selection using forward selection.
# Again, we see that the full model is the optimal one, as the AIC is minimized.
null_model <- glm(alert ~ 1, family = binomial(link = "logit"))
stepAIC(null_model, scope = alert ~ year + month + temp + pres + dewp + rain + wspd + wdir, direction = "forward")

# Predict (should avoid using predict)
predictions <- (predict.glm(object = linear_classification, newdata = data[, explanatory_var], type = "response") > 0.5)
errors <- (data['alert'] != predictions) # /!\ error on the TRAINING set ()
mean(errors)


# Part 2 : Leave-One-Out cross-validation
# Use full model
LOO_Posterior <- rep(0, dim(data)[1])
for (i in 1:dim(data)[1]) {
  idx <- rep(TRUE, dim(data)[1])
  idx[i] <- FALSE
  
  linear_classification <- glm(alert[idx] ~ year[idx] + month[idx] + temp[idx] + pres[idx] + dewp[idx] + rain[idx] + wspd[idx] + wdir[idx], family = binomial(link = "logit"))
  prediction <- linear_classification$coefficients %*% c(1, as.matrix(data[i, explanatory_var]))
  LOO_Posterior[i] <- exp(prediction)/(1 + exp(prediction))
}

# Construct confusion matrix
conf_mat <- table(alert, as.integer(LOO_Posterior > 0.5))

# Compute sensitivity and specificity
sens_or_spec <- function(confusion_matrix, spec = FALSE) {
  if (spec) {
    specificity <- (confusion_matrix[1,1])/(confusion_matrix[1,1] + confusion_matrix[1,2])
    return(specificity) 
  }
  else {
    sensitivity <- (confusion_matrix[2,2])/(confusion_matrix[2,2] + confusion_matrix[2,1])
    return(sensitivity)
  }
}
sensitivity <- sens_or_spec(conf_mat)
specificity <- sens_or_spec(conf_mat, spec = TRUE)
