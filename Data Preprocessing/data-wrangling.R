library(tidyverse)

# --------- Basic ---------
# convert data to tbl class
df.iris <- tbl_df(iris)
df.mtcars <- tbl_df(mtcars)

# summary info (# of obs, variables, type)
glimpse(df.iris)
glimpse(df.mtcars)

# --------- Reshaping ---------
# sort based on column values
df.iris %>%
  arrange(Sepal.Length) # low to high
df.mtcars %>%
  arrange(desc(mpg)) # high to low

# rename columns
df.mtcars %>%
  rename(miles_per_gallon = mpg) # new = old

# create dataframe
id <- c("1", "2")
gpa <- c("3.9", "3.7")
unit <- c("16", "20")
df <- data_frame(id, gpa, unit)

# long/wide transformation
df %>%
  gather(gpa, unit, 2:3) %>% # wide to long
  spread(gpa, unit) # long to wide

# Unite/separate columns
df %>%
  unite("gpa_unit", c(gpa, unit), sep = ",") %>%
  separate(gpa_unit, c("gpa", "unit"), sep = ",")

# --------- Subset by Rows ---------
df.iris %>%
  filter(Sepal.Length > 2) %>% # extract rows based on logical criteria
  sample_frac(0.9, replace = T) %>% # sample with fraction
  sample_n(50, replace = T) %>% # sample with number
  distinct() %>% # remove duplicates
  slice(1,3) # subset rows by positions

# --------- Subset by Columns ---------
df.iris %>%
  select(Sepal.Length, Sepal.Width) %>% # select by column names
  select(-Sepal.Length) # select all but some columns

# --------- Data Summary ---------
df.iris %>%
  summarise_each(funs(mean)) # apply fun to each column
df.iris %>%
  summarise(median = median(Sepal.Length)) # apply fun to one column
df.mtcars %>%
  count(cyl, vs) # unique counts for a column or for multiple column values
summary(df.mtcars$mpg) # normal summary applied to a column

# --------- Group Data ---------
df.iris %>%
  group_by(Species) %>% # group by a column
  summarise(avg = mean(Sepal.Length)) # summarise each group

# --------- Make New Variable ---------
df.iris %>%
  mutate(Sepal = Sepal.Length + Sepal.Width) %>% # append new column to the right
  transmute(SepalSqr = Sepal^2) # create new column and drop all old columns of the whole tibble

# --------- Combine Data Sets ---------
x1 <- c("a", "b", "c")
x2 <- c(T, F, T)
df1 <- data_frame(x1, x2)
x1 <- c("a", "b", "d")
x3 <- c(F, F, F)
df2 <- data_frame(x1, x3)

# join rows from df2 to df1 based on some column (retain rows from df1)
left_join(df1, df2, by = "x1")
# join rows from df1 to df2 based on some column (retain rows from df2)
right_join(df1, df2, by = "x1")
# join and retain only rows common to both df's
inner_join(df1, df2, by = "x1")
# join and retain all rows in both df's
full_join(df1, df2, by = "x1")

# Filtering Join
# Keep rows in df1 that have a match in df2
semi_join(df1, df2, by = "x1")
# Keep rows in df1 that don't hve matches in df2
anti_join(df1, df2, by = "x1")

# Set Operations
df2 <- rename(df2, x2 = x3)
intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)

# Binding
df1 %>%
  bind_rows(df2)
df1 %>%
  bind_cols(df2)