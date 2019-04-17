library(readr)
library(rpart)

# import data
dataset <- read_csv("Position_Salaries.csv")
dataset <- dataset[2:3]

# initialize regressor
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))
  
# # visualize
# library(ggplot2)
# ggplot() +
#   geom_point(aes(x = dataset$Level, y = dataset$Salary),
#              colour = 'red') +
#   geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
#             colour = 'blue') +
#   xlab('Level') +
#   ylab('Salary')

# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  xlab('Level') +
  ylab('Salary')

# predict
y_pred <- predict(regressor, newdata = data.frame(Level = 6.5))
