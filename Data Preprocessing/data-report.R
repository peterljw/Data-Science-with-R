library(tidyverse)
library(RColorBrewer)
# ------------------- Helper Functions -------------------

# takes a dataframe with continuous-values columns and return a data report table
data_report_cont <- function(dataset) {
  feature <- colnames(dataset)
  count <- dim(dataset)[1]
  missing <- sapply(dataset, function(x) sum(is.na(x))/count)
  card <- sapply(dataset, function(x) length(unique(x[!is.na(x)])))
  summary_df <- t(sapply(dataset, function(x) summary(x[!is.na(x)])))
  std <- sapply(dataset, function(x) sd(x[!is.na(x)]))
  df <- data_frame("Feature" = feature, "Count" = count, 
                   "Missing %" = missing, 
                   "Card." = card, "Min" = summary_df[,1], 
                   "1st Qrt." = summary_df[,2],
                   "Median" = summary_df[,3],
                   "Mean" = summary_df[,4],
                   "3rd Qrt." = summary_df[,5],
                   "Max" = summary_df[,6],
                   "Std. Dev." = std)
  return(df)
}

data_report_disc <- function(dataset) {
  feature <- colnames(dataset)
  count <- dim(dataset)[1]
  missing <- sapply(dataset, function(x) sum(is.na(x))/count)
  card <- sapply(dataset, function(x) length(unique(x[!is.na(x)])))
  mode <- sapply(dataset, function(x) names(sort(table(x), 
                                                 decreasing = T))[1])
  mode_freq <- sapply(dataset, function(x) sort(table(x), 
                                                decreasing = T)[1])
  mode_percent <- mode_freq/count
  second_mode <- sapply(dataset, function(x) names(sort(table(x), 
                                                        decreasing = T))[2])
  second_mode_freq <- sapply(dataset, function(x) sort(table(x), 
                                                       decreasing = T)[2])
  second_mode_percent <- second_mode_freq/count
  df <- data_frame("Feature" = feature, "Count" = count, 
                   "Missing" = missing, "Card." = card,
                   "Mode" = mode, "Mode Freq." = mode_freq, 
                   "Mode %" = mode_percent,
                   "2nd Mode" = second_mode, "2nd Mode Freq." = second_mode_freq, 
                   "2nd Mode %" = second_mode_percent)
  return(df)
}


# ------------------- Load Data -------------------
# glimpse the data
dataset <- tbl_df(read_csv("Data/SkillCraft1_Dataset.csv"))

# TO DO
# adjust types of the variables properly

# overview
glimpse(dataset)

# ------------------- Data Report -------------------
# TO DO
# set the indicies for continuous/discrete columns
# cont_index <- c(...)
# disc_index <- c(...)

# obtain reports
data_report_a <- data_report_cont(dataset[,cont_index])
data_report_b <- data_report_disc(dataset[,disc_index])
write.csv(data_report_a, file = "data_report_a.csv")
write.csv(data_report_b, file = "data_report_b.csv")

# missing data
## consider removing columns which have 60% or more data missing
data_report_a$`Missing %`>= 0.6
## fill in the missing data with proper methods
## fix: replace with medians
for(feature in data_report_a$Feature[data_report_a$`Missing %`>0]) {
  dataset[feature] <- ifelse(is.na(unlist(dataset[feature])), 
                             ave(unlist(dataset[feature]), FUN = function(x) median(x, na.rm = T)),
                             unlist(dataset[feature]))
}
# outlier
## check for irregular outliers and consider clamp transformation
outliers <- sapply(dataset[, cont_index], function(x) boxplot(unlist(x), plot=FALSE)$out)
# irregular cardinality
## observe the data report to draw conclusions

# lower and upper panels for correlation/scatter plot matrix
panel.cor <- function(x, y) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
upper.panel<-function(x, y) {
  points(x,y, pch = 19, col = colors[unlist(dataset[target_index])])
}
# correlation matrix with scatter plot
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
n <- length(unlist(unique(dataset[target_index])))
colors <- sample(col_vector, n)
# TO DO: index the columns of interest for the correlation matrix
# pairs(dataset[,...], lower.panel = panel.cor, 
# upper.panel = upper.panel, oma=c(3,3,3,15))
par(xpd = TRUE)
legend("bottomright", legend=names(unlist(unique(dataset[target_index]))),
       fill=colors, cex = 0.7)

# save the adjusted data after necessary adjustments
saveRDS(dataset, file = "dataset.rds")

