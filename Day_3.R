# Day_3.R
# 17 April 2017
# Adnaan Emandien
# Distributions


# Some notes --------------------------------------------------------------


# Load packages -------------------------------------------------------

library(fitdistrplus)
library(logspline)
library(tidyverse)


r_norm <- rnorm(n = 1000, mean = 13, sd = 1)
# can see that dara is normal
hist(r_norm)
descdist(r_norm, discrete = FALSE, boot = 100)

# uniform
y  <- runif(100)
par(mfrow = c(1, 1))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# t-tests -----------------------------------------------------------------

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))


# check assumptions -------------------------------------------------------

# Normality
# For this we may use the Shapiro-Wilk test
shapiro.test(r_dat$dat)
shapiro.test(r_dat$dat)[1] # only output the w value 
shapiro.test(r_dat$dat)[2] # only output the p-value
# But that is testing all of the data together
# We must be a bit more clever about how we make this test
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2])) # Summarise data to 2 values
# How to test if data is normal, usuallyin biology data is not normal
# Remember, the data are normal when p > 0.05
# The data are non-normal when p <= 0.05


# Check homoscedasticity --------------------------------------------------

# There are many ways to check for homoscedaticity
# Which is the similiarity of variance of the sample sets
# For now we will simply say that this assumptions is met when
# the variance of the samples are not more than 2 - 4 times greater
# than one another

# Check everything at once..
# WRONG
var(r_dat$dat)

# or do it the tidy
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))
# r_norm_dis are p-values
# r_norm_var is variance
 # these are two assumptions that have to be met (see 6.1 in the workbook)

# A one sample t-test -----------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")
# test normality of distribution

# Perhaps a visualisation?

# Run the test
t.test(r_one$dat, mu = 20) # which column we want to run the test on

# Run a test we know will produce a significant result
t.test(r_one$dat, mu = 30)
# df = degrees of freedom = sample size - 1

# Pick a side -------------------------------------------------------------

# Are these data SMALLER/LESS than the population mean
t.test(r_one$dat, mu = 20, alternative = "less")
# or greater
t.test(r_one$dat, mu = 20, alternative = "greater")

# But what about for the larger population mean?
# Are the samples less than the population of 30?
t.test(r_one$dat, mu = 30, alternative = "less")
# What about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")

# two smaple t-tests ------------------------------------------------------

# Create another dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))
# run a default/ basic test
t.test(dat ~ sample, data = r_two, var.equal = TRUE)
# df = 38 because 20+20-2=38

# Pick a side
# Is A less than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")
# Is A greater than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")


# Ecklonia Task -----------------------------------------------------------

# Ecklonia_Task.R
# Adnaan Emandien
# 17 April 2018



# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)

# Load data ---------------------------------------------------------------

read_csv(file = "ecklonia.csv")

ecklonia <- read_csv(file ="ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = var(value)[1],
            stipe_mass_norm = as.numeric(shapiro.test(value)[2]))
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")


# Exercise 1 --------------------------------------------------------------
#Find or create your own normally distributed data and 
#think of a hypothesis you could use a t-test for. 
#Write out the hypothesis, test it, and write a one sentence conclusion for it. 
#Provide all of the code used to accomplish this.

# Set-up ------------------------------------------------------------------

library(tidyverse)
library(plotly)

# Generate random data ----------------------------------------------------

random_data <- data.frame(dat = c(rnorm(n = 800, mean = 20, sd = 2),
                                  rnorm(n = 800, mean = 25, sd = 2)),
                          sample = c(rep("A", 800), rep("B", 800)))


# Formulate a hypothesis --------------------------------------------------

#Hypothesis was based on the following visualization

ggplot(data = random_data, aes(x = dat, fill = sample)) +
  geom_histogram(colour = "black", position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")

#Thus
#H0: Sample A is not greater than Sample B
#H1: Sample A is greater than Sample B

# Check assumptions -------------------------------------------------------

random_data %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))

#Run a t-test

t.test(dat ~ sample, data = random_data, var.equal = TRUE)

#Two Sample t-test
#data:  dat by sample
#t = -50.132, df = 1598, p-value = 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -5.218771 -4.825770
#sample estimates:
# mean in group A mean in group B 
#19.89810        24.92037

#In conclusion sample A is significantly greater than sample B (t = -50.132, df = 1598, p-value = <2.2e-16)
#The data used in this exercise is random thus results obtained when conducting the t-test will vary
