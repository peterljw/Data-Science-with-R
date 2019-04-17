# Polynomial Regression

# importing the dataset
dataset <- read.csv('Position_Salaries.csv')

# preprocess
dataset <- dataset[2:3]

# fit
dataset$Level2 <- (dataset$Level)^2
dataset$Level3 <- (dataset$Level)^3
poly_reg <- lm(formula = Salary ~ .,
               data = dataset)
summary(poly_reg)
poly_reg$coefficients

# visualize
p <- ggplot()
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)
y_grid <- predict(poly_reg, newdata = data_frame(Level = x_grid,
                                                 Level2 = x_grid^2,
                                                 Level3 = x_grid^3))
poly_df <- data_frame(x_grid, y_grid)
p + geom_point(data = dataset, aes(x = Level, y = Salary)) + 
  geom_line(data = poly_df, aes(x = x_grid, y = y_grid))

# predict
new_level <- c(1.5, 2.5, 3.5)
y_pred <- predict(poly_reg, data_frame(Level = new_level,
                                       Level2 = new_level^2,
                                       Level3 = new_level^3))
