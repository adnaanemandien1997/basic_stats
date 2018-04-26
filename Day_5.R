# Day_5.R 
# Adnaan Emandien
# 20 April 2018

# Load libraries ----------------------------------------------------------

library(tidyverse)
# library(Rmisc) # Unfortunately this overrides many dplyr functions
library(ggpubr)
library(corrplot)

# Load data ---------------------------------------------------------------

snakes <- read_csv("snakes.csv") %>% 
  mutate(day = as.factor(day))

 # snakes$day <- as.factor(snakes$day)
# Manipulate the data -----------------------------------------------------

snakes_summary <- snakes %>% 
  group_by(day) %>% 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))

# Formulate hypothesis ----------------------------------------------------

# H0: There is NO difference in the number of openings from day to day (NULL HYPOTHESIS)
# H1: There is a difference in the number of openings from day to day (ALTERNATIVE HYPOTHESIS)
 # Experiment designed to have only one of two outcomes (above) either H1 or H0

# Test a hypothesis -------------------------------------------------------

snakes.summary2 <- Rmisc::summarySE(data = snakes,
                             measurevar = "openings", # measure variance of openings
                             groupvars = c("day")) # group by day

# Then visualise the data
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, 
               aes(x = day, xend = day, y = openings - ci, 
                   yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.05)

snakes.summary2
# y = mean - ci
# yend = mean + ci

# But wait, we have two factors, so we need another null hypothesis

# H0: There is no difference between snakes with respect to 
 # the number of openings at which they habituate.
# H0: There is no difference between days in terms of
 # the number of openings at which the snakes habituate.

snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov) # P < 0.05, in terms of H02, we reject the null hypothesis
                        # need degree of freedom, sum of squares (amount of variance), need to get the f value, p > or < 0.05

# Test both hypotheses
snakes.all.aov <- aov(openings ~ day + snake, data = snakes) 
summary(snakes.all.aov)


# Testing assumptions afterwards ------------------------------------------

# First visualise normality of data
snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)
# Then visualise homoscedasticity of results
plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

# Check Tukey results
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)

# Visualise the factor interaction
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)


# Exercise ----------------------------------------------------------------

# Get the moth data from Github
# Run a two-way ANOVA on them


# Load data ---------------------------------------------------------------

moth_traps <- read_csv("moth_traps.csv") %>% 
  gather(key = "trap", value = "count", -Location)


# Manipulate data ---------------------------------------------------------

moth_traps_summary <- moth_traps %>% 
  group_by(Location) %>% 
  summarise(moth_traps_mean = mean(count),
            moth_traps_sd = sd(count))


# Formulate hypothesis ----------------------------------------------------

# H0: There is NO difference in the number of moths found at specific traps from location to location (NULL HYPOTHESIS)
# H1: There is a difference in the number of moths found at specific traps from location to location (ALTERNATIVE HYPOTHESIS)

moth_traps_summary2 <- summarySE(data = moth_traps,
                             measurevar = "count", # measure variance of openings
                             groupvars = c("Location"))

# Visualise the data ------------------------------------------------------

# Then visualise the data
ggplot(data = moth_traps, aes(x = Location, y = count)) +
  geom_segment(data = moth_traps_summary2, 
               aes(x = Location, xend = Location, y = count - ci, 
                   yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.05) 

moth_traps_summary2

# H0: There is no difference between counts of moths between various locations
# H0: There is no difference between counts of moths between various traps

moth_traps_aov <- aov(count ~ Location, data = moth_traps)
summary(moth_traps_aov) # P < 0.05, in terms of H02, we reject the null hypothesis


# Test both hypotheses
moth_traps_all_aov <- aov(count ~ Location + trap, data = moth_traps)
summary(moth_traps_all_aov)

# Testing assumptions afterwards ------------------------------------------

# First visualise normality of data
moth_traps_residuals <- residuals(moth_traps_all_aov)
hist(moth_traps_residuals)
# Then visualise homoscedasticity of results
plot(fitted(moth_traps_all_aov), residuals(moth_traps_all_aov))

# Check Tukey results
moth_traps.tukey <- TukeyHSD(moth_traps_all_aov, which = "Location")
plot(moth_traps.tukey)

# Visualise the factor interaction
ggplot(data = moth_traps, aes(x = as.factor(Location),
                          y = count,
                          colour = trap)) +
  geom_line(size = 3) +
  geom_point(size = 4)
# RWS: THis doesn't quite make sense as a visualisation...



plot1 <- ggplot(moth_traps, aes(x = Location, y = count)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

plot2 <- ggplot(moth_traps, aes(x = trap, y = count)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

plot3 <- ggplot(moth_traps, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap))
  # geom_jitter(width = 0.05, shape = 21, aes(fill = trap))

library(ggpubr)
finalplot <- ggarrange(plot1, plot2, plot3, nrow = 2, ncol = 2, labels = "AUTO")
finalplot

# Regressions -------------------------------------------------------------

# How much does n independent variable affect the dependent variable?

# For the explanation of this statistical analysis
# we are going to use eruption data from Ol' faithful

# Look at the topw of the data
head(faithful)

# Plot a quick scatterplot
ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point()

# There is a signficance linear relationship between 

# Form a hypothesis -------------------------------------------------------

# H0: Waiting time does NOT influence the duration of an eruption
# H1: Waiting time does influence the duration of an excpeiton


# Test a hypothesis -------------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm) # Estimate = c in "y = mx + c"
                    # residual standadr error = unknowns
                     # (Intercept - )
                     # Adjusted R = amount of variation. By knowing waiting time we can explain 81% of the variation of eruptions
                      # Best fit line closer to ..... higher r2
geom_smooth(method = "lm", se = F, colour ="hotpink")


# Correlations ------------------------------------------------------------

# ordinal data - small, medium or large
# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("ecklonia.csv")

# Formulate hypothesis ----------------------------------------------------

# H0: There is no relationship between frond length and frond mass
 # for the kelp Ecklonia maxima
# H1: There is relationship between frond length and frond mass
 # for the kelp Ecklonia maxima

# Test a hypothesis -------------------------------------------------------

cor.test(ecklonia$frond_length, ecklonia$frond_mass) # $ = filter out, df = n - factors (2)

#visualise the data
ggplot(data = ecklonia, aes(x = frond_length, y = frond_mass)) +
  geom_point()

# Run multiple tests at once ----------------------------------------------

ecklonia_sub <- ecklonia %>%
  select(frond_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor

# Spearman rank test ------------------------------------------------------

# First create an ordinal column
ecklonia$length <- as.numeric(cut((ecklonia$frond_length + ecklonia$stipe_length), 3))

# Then run a Spearman test
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")


# Kendall rank test -------------------------------------------------------

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Visualise all the things!  ----------------------------------------------

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

corrplot(ecklonia_pearson, method = "circle")


# Heat map ----------------------------------------------------------------

library(reshape2)
melted_eck <- melt(ecklonia_pearson)

ggplot(melted_eck, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")
