library(readr)
library(e1071)

# import data
dataset <- read_csv("Position_Salaries.csv")
dataset <- dataset[2:3]

# initialize regressor
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression')
# visualize
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  xlab('Level') +
  ylab('Salary')

# predict
y_pred <- predict(regressor, newdata = data.frame(Level = 6.5))
                  