# Multiple linear regression
library(readr)

# NA check
na_check <- function(df) {
  count <- sapply(df, function(x) sum(is.na(x)))
  percentage <- count/nrow(df)
  na.df <- data.frame(count, percentage)
  rownames(na.df) <- colnames(df)
  return(na.df)
}

# load data
dataset <- read_csv("50_Startups.csv")
na_check(dataset) # check

# Encoding categorical data
dataset$State <- factor(dataset$State,
                          level = c('New York', 'California', 'Florida'),
                          labels = c(1, 2, 3))

# train test split
library(caTools)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

# model fitting
regressor <- lm(formula = Profit ~.,
                data = training_set)
# info on the regressor
summary(regressor)
regressor_summary <- summary(regressor)
# regression coefficients, residuals, df, r2, adj r2
regressor_summary$coefficients
regressor_summary$residuals
regressor_summary$df
regressor_summary$r.squared
regressor_summary$adj.r.squared

# predict
y_pred <- predict(regressor, newdata = test_set)
# RMSE
RMSE(test_set$Profit, y_pred)

# backward elimination
# x: exampls to be trained
# sl: significance level
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05 
backwardElimination(training_set, SL)
