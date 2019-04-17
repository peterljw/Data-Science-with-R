# Simple linear regression
library(readr)
library(caTools)
library(ggplot2)
library(plotly)
library(MLmetrics)
dataset <- read_csv("Salary_Data.csv")

# train test split
split <- sample.split(dataset$Salary, SplitRatio = 2/3)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

# fit slr
regressor <- lm(formula = Salary ~ YearsExperience,
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
RMSE(test_set$Salary, y_pred)

# visualize
# training
ggplotly(ggplot() +
           geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
                      color = 'blue') +
           geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
                     color = 'green') +
           ggtitle('Salary vs Experience (Training Set)') +
           xlab('Years of Experience') +
           ylab('Salary ($)'))
# testing
ggplotly(ggplot() +
           geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
                      color = 'red') +
           geom_line(aes(x = test_set$YearsExperience, y = predict(regressor, newdata = test_set)),
                     color = 'green') +
           ggtitle('Salary vs Experience (Test Set)') +
           xlab('Years of Experience') +
           ylab('Salary ($)'))

