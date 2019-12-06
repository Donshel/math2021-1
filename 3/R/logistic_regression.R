# Libraries
library(corrplot)
library(car)
library(MASS)
library(ggplot2)

# ------------- PART 2 : Logistic Regression Model -------------- 
data <- read.table("products/csv/binarized.csv", header = TRUE, na.strings = NA, sep = ",")
attach(data)

# Part 1 : Finding a good logistic model

explanatory_var <- c('year', 'month', 'temp', 'pres', 'dewp', 'rain', 'wspd', 'wdir')


linear_classification <- glm(alert ~ year + month + temp + pres + dewp + rain + wspd + wdir, family = binomial(link = "logit"))
summary(linear_classification)

# Draw correlation plot of the explanatory variables
correlation_plot <- cor(data[, explanatory_var])
pdf("products/pdf/correlation_plot.pdf")
corrplot(correlation_plot, method = "color", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

# Compute VIF scores to check for multicolinearity
VIF_scores <- vif(linear_classification)
print(VIF_scores)

# As there is multicolinearity between temp, pres, dewp, try each model by introducing one of them
# And compare the respective coefficients to the ones obtained in the full model.
# In full model: temp ~ -0.24125; pres ~ -0.07032, dewp ~ 0.15057
# In respective models: temp ~ -0.023704, pres ~ -0.0001157, dewp ~ -0.0005504
glm_temp <- glm(alert ~ year + month + temp + rain + wspd + wdir, family = binomial(link = "logit"))
summary(glm_temp)

glm_pres <- glm(alert ~ year + month + pres + rain + wspd + wdir, family = binomial(link = "logit"))
summary(glm_pres)

glm_dewp <- glm(alert ~ year + month + dewp + rain + wspd + wdir, family = binomial(link = "logit"))
summary(glm_dewp)

# As multicolinearity introduces an issue here, build model with PC1 of pres, temp, dewp and with remaining variables
pca <- princomp(data[c('temp', 'pres', 'dewp')])
modified_explanatory_var <- c('year', 'month', 'rain', 'wspd', 'wdir', 'PC1')
modified_data <- data[c('year', 'month', 'rain', 'wspd', 'wdir', 'alert')]
modified_data['PC1'] <- pca$scores[, 1]
attach(modified_data)
linear_classification <- glm(alert ~ year + month + rain + wspd + wdir + PC1, family = binomial(link = "logit"))
summary(linear_classification)

# Conduct variable selection using backward selection.
stepAIC(linear_classification)

# Conduct variable selection using forward selection.
null_model <- glm(alert ~ 1, family = binomial(link = "logit"))
stepAIC(null_model, scope = alert ~ year + month + PC1 + rain + wspd + wdir, direction = "forward")

# Predict (should avoid using predict)
predictions <- (predict.glm(object = linear_classification, newdata = modified_data[, modified_explanatory_var], type = "response") > 0.5)
errors <- (modified_data['alert'] != predictions) # /!\ error on the TRAINING set ()
mean(errors)

# Look at fitted values
fitted_values <- linear_classification$fitted.values
summary(fitted_values)
plt <- ggplot(data.frame(fitted_values), aes(y = fitted_values)) + geom_boxplot()
plt <- plt + labs(x = "", y = "Fitted values")
ggsave("products/pdf/boxplots_fitted_values.pdf", plt)

# Look at Pearson residuals
residuals <- resid(linear_classification, type = "pearson")
summary(residuals)
plt <- ggplot(data.frame(residuals), aes(y = residuals)) + geom_boxplot()
plt <- plt + labs(x = "", y = "Pearson residuals")
ggsave("products/pdf/boxplots_residuals.pdf", plt)

# Part 2 : Leave-One-Out cross-validation
# Use full model
LOO_Posterior <- rep(0, dim(modified_data)[1])
for (i in 1:dim(modified_data)[1]) {
  idx <- rep(TRUE, dim(modified_data)[1])
  idx[i] <- FALSE
  
  linear_classification <- glm(alert[idx] ~ year[idx] + month[idx] + PC1[idx] + rain[idx] + wspd[idx] + wdir[idx], family = binomial(link = "logit"))
  prediction <- linear_classification$coefficients %*% c(1, as.matrix(modified_data[i, modified_explanatory_var]))
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
print(sensitivity)
specificity <- sens_or_spec(conf_mat, spec = TRUE)
print(specificity)

# Plot ROC curve
ROC_maker <- function(scores, memberships) {
  cutoff <- sort(scores)[1:(length(scores)-1)]
  sensitivity <- rep(0, length(cutoff))
  specificity <- rep(0, length(cutoff))
  idx <- 1
  for (cut in cutoff) {
    data <- as.data.frame(scores)
    data["Membership"] <- as.integer(data > cut)
    
    confusion_matrix <- table(memberships, data[, "Membership"])
    sensitivity[idx] <- sens_or_spec(confusion_matrix)
    specificity[idx] <- sens_or_spec(confusion_matrix, spec = TRUE)
    idx <- idx + 1
  }
  sensitivity <- c(sensitivity, 0)
  specificity <- c(specificity, 1)
  plot(1 - specificity, sensitivity, type="l")
  
  return(list(spec = (1 - specificity), sens = sensitivity))
}

ROC <- ROC_maker(LOO_Posterior, alert)

# Compute AUC
AUC <- function(ROCx, ROCy)
{
  n <- length(ROCx)
  base <- ROCx[1:(n-1)] - ROCx[2:n]
  height <- ROCy[2:n]
  return(sum(base*height))
}
auc <- AUC(ROCx = ROC$spec, ROCy = ROC$sens)

