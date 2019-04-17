library(tidyverse)

glimpse(mpg)

# ------------ One Variable ------------
# continuous
a <- ggplot(mpg, aes(hwy)) # put your choice of variable inside aes
a + geom_area(stat = "bin")
a + geom_freqpoly()
a + geom_density(kernel = "gaussian") # common
a + geom_dotplot()
a + geom_histogram(bins = 20) # common
ggplot(mpg, aes(x="hwy", y=hwy)) + geom_boxplot() # common

# discrete
a <- ggplot(df.mpg, aes(model)) # put your choice of variable inside aes
a + geom_bar() # common

# ------------ Two Variables ------------
# continuous vs. continuous
f <- ggplot(mpg, aes(cty, hwy))
f + geom_jitter() # common
f + geom_point() # common
f + geom_smooth(model = lm) # common
f + geom_text(aes(label = cty))

# discrete x vs. continuous y
g <- ggplot(mpg, aes(class, hwy))
g + geom_bar(stat = "identity") # common
g + geom_boxplot() # common
g + geom_dotplot(binaxis = "y", stackdir = "center")
g + geom_violin(scale = "area")

# discrete x vs. discrete y
h <- ggplot(diamonds, aes(cut, color))
h + geom_jitter()

# continous function
x <- c(-10:10)
y <- x^2
df <- data_frame(x, y)
j <- ggplot(df, aes(x, y))
j + geom_area()
j + geom_line() # common
j + geom_step(direction = "hv")

# continous bivariate distribution
x <- rnorm(10000)
y <- runif(10000, min = -1, max = 1)
df <- data_frame(x, y)
i <- ggplot(df, aes(x, y))
i + geom_bin2d()
i + geom_density2d()
i + geom_hex()

# ------------ Maps ------------
df <- data_frame(murder = USArrests$Murder,
                 state = tolower(rownames(USArrests)))
map <- map_data("state")
l <- ggplot(df, aes(fill = murder))
l + geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat)