# Day_R_2.R (Day 1)
# 12 April 2018
# Adnaan Emandien
# Purpose: to practice some of the concepts that we will encounter


# Load librarys -----------------------------------------------------------

library(tidyverse)
# Integers ----------------------------------------------------------------

# Generate some integer data
integer_r <- as.integer(seq(5, 14, by = 1))

# Look at a brief summary of them
summary(integer_r)


# Continuous --------------------------------------------------------------

# Generate a sequence of numeric values
numeric_r <- seq(23, 43, length.out = 10)


# Dates -------------------------------------------------------------------

# One may perform some arithmetic with dates
as.Date("2005-12-31") - as.Date("2005-12-25")#difference between dates in time
# or for example
dates_R <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day") # Keep in mind the correct format
# There's so much  more
summary(integer_r)

# Dataframes --------------------------------------------------------------

# different types of data combined into one dataframe (tibble)

# Create the base dataframe
# create new columns (3) with data previously assigned
# "Integers", "numeric", and "dates" can be seen as the column names
df_r <- data.frame(integers = integer_r,
                   numeric = numeric_r,
                   dates = dates_R)
# Then upgrade it to a tibble
df_r <- as_tibble(df_r)
summary(df_r)


# Categories --------------------------------------------------------------

# Electronics
elec_r <- as.factor(c("laptop",
                      "desktops",
                      "cell phones"))
# People
people_r <- as.factor(c("funny",
                        "beautiful",
                        "beanies"))
# Colours 
colour_r <- as.factor(c("red", "blue"))

# Ordinal data ------------------------------------------------------------

# Here we still have qualitative data
# but with some sort of order

colour_qual <- ordered(c("blue", "green",
                            "yellow", "orange",
                            "red"),
                          levels = c("blue", "green",
                                     "yellow", "orange",
                                     "red"))

# Binary ------------------------------------------------------------------

# These are generally represented as: TRUE or FALSE
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)

# Characters --------------------------------------------------------------

sites_r <- c("Yztervarkpunt", "Betty's Bay",
             "Gansbaai", "Sea Point")

# Missing values ----------------------------------------------------------

chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA) #Data not available #Account for total number of cases, even if it's not present
summary(chicks_nest) #A summary
mean(chicks_nest) #The mean
sd(chicks_nest) #The standard deviation


# Descriptive statistics --------------------------------------------------

# First create a dataframe
chicks <- as_tibble(ChickWeight)

chicks %>% 
  summarise(chicken_count = n())
# or
nrow(chicks)


# Measures of central tendency --------------------------------------------

# Calculate mean weight
chicks %>% 
  summarise(mean_wt = mean(weight))

# Be mre specific
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% # group by diets
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))

# Visualise the density of the data
ggplot(data = filter(chicks, Time == 21),
                     aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4)


# Skewness ----------------------------------------------------------------

# Calculate the numeric value
# First load the libraries
library(e1071)
# Compare the difference in mean and median against skewness
chicks %>% 
  summarise(mean_wt = mean(weight))

# Be mre specific
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% # group by diets
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))

# skewness = where does most of the data lie? ie skew

# Kurtosis ----------------------------------------------------------------

# Calculate the kurtosis of the tails of a distribution
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% # group by diets
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurtosis_wt = kurtosis(weight))

# Measures of variability -------------------------------------------------

# Below is a summary of many different statistical properties
wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median (weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.5),
            wt_quart3 = quantile(weight, 0.75))


