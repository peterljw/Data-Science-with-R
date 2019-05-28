# Data Preprocessing

# Importing the dataset
dataset <- read.csv('Data.csv')

# Dealing with missing continuous data
dataset$COLUMN <- ifelse(is.na(dataset$COLUMN), 
                         ave(dataset$COLUMN, FUN = function(x) mean(x, na.rm = T)),
                         dataset$COLUMN)
dataset$COLUMN <- ifelse(is.na(dataset$COLUMN), 
                         ave(dataset$COLUMN, FUN = function(x) median(x, na.rm = T)),
                         dataset$COLUMN)

# Encoding categorical data
dataset$COLUMN <- factor(dataset$COLUMN,
                         level = c(sort(unique(dataset$COLUMN))),
                         labels = c(1, 2, 3, ...))

# Splitting the dataset into the Training set and Test set
library(caTools)
split <- sample.split(dataset$TARGET, SplitRatio = 0.8)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

# Feature Scaling
training_set[, c(...)] <- scale(training_set[, c(...)])
test_set[, c(...)] <- scale(test_set[, c(...)])

# Remove rows with NA's (only if the missing data consists of <5% of the data)
remove_na_rows <- function(df) {
  df_T <- as.data.frame(t(df))
  df <- df[sapply(df_T, function(x) all(!is.na(x))),]
  return(df)
}


