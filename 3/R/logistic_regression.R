# Libraries

# ------------- PART 2 : Logistic Regression Model -------------- 
data <- read.table("products/csv/binarized.csv", header = TRUE, na.strings = NA, sep = ",")
attach(data)

# Part 1

explanatory_var <- c('year', 'month', 'temp', 'pres', 'dewp', 'rain', 'wspd', 'wdir')

linear_classification <- glm(alert ~ year + month + temp + pres + dewp + rain + wspd + wdir, family = binomial(link = "logit"))
summary(linear_classification)

predictions <- (predict.glm(object = linear_classification, newdata = data[, explanatory_var], type = "response") > 0.5)

errors <- (data['alert'] != predictions) # /!\ error on the TRAINING set ()
mean(errors)

# An objective strategy needs to be used in order to select the explanatory variables 
# to include in the final model (using summary(linear_classification), interpretation of
# weights, PC decomposition, etc etc) (TP5)

# look at the residuals and at the fitted values. Comment and interpret (???)


# Part 2

# Resort to a leave-one out cross-validation technique2 in order to characterize the clas- 
# sification performance of the procedure. At each step of the CV, a similar model as the 
# “optimal” model derived in question 1 needs to be fitted in order to derive the classification 
# rule. Construct a confusion matrix and measure its corresponding error rate. Comment
# ???????
