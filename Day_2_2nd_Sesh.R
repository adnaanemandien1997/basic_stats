# Day_2_2nd_Sesh.R
# Adnaan Emandien
# 13 April 2018
# Day 2
# The day in which we discuss data visualisations and distributions

# Load libraries ----------------------------------------------------------

library(tidyverse)

# Manual calculations -----------------------------------------------------

# Random data generated
# The mean 
r_dat <- data.frame(dat = rnorm(n = 700, mean = 372, sd = 50),
                    sample = "A")# to get random normal data

# Quick visualisation
ggplot(data = r_dat, aes(x = dat)) +
  geom_density()
# The mean
# Sum of all points
# divided by
# the number of all the points
r_dat %>% 
  summarise(r_sum = sum(dat),      # Always avoid putting in static data
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))

# The median
# Brute force with  base R
order(r_dat$dat)[length(r_dat$dat)/2]
# For median, first order the data*
# Or use tidy
r_dat %>% 
  arrange(dat) %>% 
  slice(12)

  r_dat$dat[(length(r_dat$dat)+1)/2]
# Or the tidy automagic way
r_dat %>% 
  summarise(r_median = median(dat))

# Variance 
# The sum of
 # Each value 
   # minus the 
    # mean
      # Squared
# Divided by
 # The count of samples minus one
r_dat %>% 
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error * r_error) %>% # add column onto existing dataframe (the anomaly)
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
            # Or use the built in function
            r_var_func = var(dat))
# The standard deviation
r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))

# Exercise 1 --------------------------------------------------------------

summary(ChickWeight$weight)

ChickWeight %>% 
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))

# Visulations -------------------------------------------------------------

# First load our libraries
# The few packages contain most functions necessary
# to make publication ready figures
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)

# Load our SA time data
sa_time <- read_csv("SA_time.CSV")

# Edit our data
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
         rep("Joburg", 2)))


sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human)

# Qualitative -------------------------------------------------------------

# generally use proportion (relative to something)
# count usually for something like a histogram
# see whatever is appropriate for the subject and the reader 

# stacked bar graphs
sa_count <- sa_long %>% # should only be 3 columns wide and 3 rows wide
  count(time_type) %>% 
  mutate(prop = n/sum(n))

# Stacked bar graphs
ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

# Stackeed proportion bar graph
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

# Pie chart # unprofessional
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie chart", subtitle = "but why though?",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0)+
  theme_minimal()
# fine for small sets of data... but still just leave it.

# Continuous data ---------------------------------------------------------

# Histograms
ggplot(data = sa_long, aes(x = minutes))+
  geom_histogram()

# Oh no!
# Let's get rid of that one value...
sa_clean <- sa_long %>% 
  filter(minutes < 300)

# A faceted histogram 1 (side by side)
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(fill = time_type), position = "dodge")+
  facet_wrap(~time_type, scales = "free_x")

# Try again # A faceted histogram 2 (side by side)
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(fill = time_type), position = "dodge")+
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# This one shows relative proportions
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(fill = time_type), 
                 position = "dodge", binwidth = 1)+
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# So analysed, overall population has their own way of distinguishing between now, now now and just now
# distributions are the same
# Homework: Ask family members

# Box plots
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type))
# Visualisation of summary table
# q1 and q3 at the end line and beginning line
# middle line is median
# dot is MAX value
# between q1 and q3 interquartile range
# tail = q1 or q3 x 1.5 BUT why are tails not same length on either side then?
 # tail shows all datapoints within q1 or q3 x1.5
 # blue more variable than red, so red has an outlier

# Notched boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type), notch = TRUE)
# if you want to display your data this way, bar graph < box plot

# Calculate summary stats for plotting over boxplots
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

# plot these means over the boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type), notch = TRUE)+
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")

# Relationships -----------------------------------------------------------

# A basic scatterplot
ggplot(data = sa_time, aes(y = now_now, x = just_now))+
  geom_point()+
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))

# Adding trend lines
ggplot(data = sa_time, aes(y = now_now, x = just_now))+
  geom_point(aes(colour = geo))+
  geom_smooth(aes(colour = geo), method = "lm")+
  coord_equal(xlim = c(0, 60), ylim = c(0,60))

