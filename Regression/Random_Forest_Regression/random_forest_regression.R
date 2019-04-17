library(readr)
library(randomForest)
library(ggplot2)

# import
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# initialize regressor
regressor <- randomForest(x = dataset[1], # x expects a dataframe
                          y = dataset$Salary, # y expects a vector
                          ntree = 10) 
# visualize
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  xlab('Level') +
  ylab('Salary')

# predict
y_pred = predict(regressor, data.frame(Level = 6.5))
