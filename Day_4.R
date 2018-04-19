# Day_4.R
# Adnaan Emandien
# 19 April 2018
# ANOVA

# Some notes --------------------------------------------------------------


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)

# t-test ------------------------------------------------------------------

# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>%
  filter(Diet %in% c(1, 2), Time == 21) # Filtering out diets 1 and 2 (excluding diets 3 and 4) at time 21 (the last day only)
# are these two diets resulting in different sizes after day 21?
 
# t-test
t.test(weight ~ Diet, data = chicks_sub)
 # We do not reject the null hypothesis that there is a difference 
# IS there, between 4 diets, a difference in the means
# one factor (Diet) with 4 levels

# ANOVA - one-way ---------------------------------------------------------

# Question: is there a difference in chicken mass attained after 21 days
# after the chickens having been fed four different diets?

# null hypothesis: there is no difference in chicken mass at 21 days after
# having been fed one of four diets

chicks_21 <- chicks %>%
  filter(Time == 21)

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

# chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
# summary(chicks.aov1)
 # Can also filter this in one step

# PR is probability: we do not accepts H0, we accept H1
# after 21 days, the four different diets have a significant effect on the mass of chickens
# ANOVA tells us there is a difference, but we do not know which diets have the effect and which doesnt


ggplot(data = chicks_21, aes(x = Time, y = weight, fill = Diet))+
  geom_boxplot(notch = TRUE) # If notches dont overlap they are significantly different
# diet 1 does not overlap with diet 3 and diet 4
# diet 3 overlaps with diet 2 and diet 4
 # but this graphs gives just a suggestion... so do another test

# Tukey HSD test ----------------------------------------------------------

TukeyHSD(chicks.aov1)
# compares diets to each other (ie. diet 1 vs diet 2)
# p-adj: 2-1 no significance, does not result in different size chicks
# 3-1 there is a difference
# if lwr is + we know that there is a significant difference (upr & lwr is above zero)
# all others are not significant

# if lower is positive then there is significantly different

# Boxplot
ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
# coord_flip()+
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))
# if lwr and upr go across zero, then not significant

# segments showing confidence intervals
# dataframe of segments
chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_Tukey$pairs <- as.factor(row.names(chicks_Tukey))

ggplot(data = chicks_Tukey)+
  geom_segment(aes(x = pairs, xend = pairs, y = lwr, yend = upr +2))+
  geom_hline(aes(yintercept = 0), size = 0.5, colour = "red", linetype = "dashed")+
  coord_flip()
  
ggplot(data = chicks_Tukey)+
  geom_errorbar(aes(x = pairs, ymin = lwr, ymax = upr))+
  geom_hline(aes(yintercept = 0), size = 0.5, colour = "red", linetype = "dashed")+
  coord_flip()

# Or just plot confidence intervals the base R way...
plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))

# Multiple factor ANOVA ---------------------------------------------------

library(tidyverse)

# H0 = There is no change in chicken mass (kg) from Day 1 to DAy 21.

chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0, 2, 21))

# Visualise the data
ggplot(data = chicks_0_21,aes(x = Time, y = weight)) +
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))

# Run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Perform a Tukey post-hoc test
TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21)))

# Look at the confidence intervals
plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))

# summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))
# summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2))))
 # Can look at difference at specific days by only by doing this 
 # but doing many anovas also increases error
# PR = SIgnificance and F determines PR

# formula: what is effect off diet on weight (weight ~ diet and effect of time on weight (+ as.factor(Time)))
# df = n - 1
# df of c(0, 21) = 1 because two values

# Look only at day 0 and 21 for both Time and Diet
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
# Note the increase in the degrees of freedom for the time factor
# But no increase for the d.f for Diet

# Now to look at interactions BETWEEN factors
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# Let's look at the Tukey results
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))
plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

# Create a line graph to help explain this concept
# First create mean values by Time and Diet
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = TRUE))

# The visualise it
ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet))+
  geom_line(size = 2) +
  geom_point(shape = 15, size = 5)
  

# non-parametic tests -----------------------------------------------------

# But what if...
# ... we dont have normal data?

# For a t-test we rather use Wilcox rank sum test
wilcox.test() # And then one fills this in the same as for t.test()

# And now for the Kruskall-Wallis
kruskal.test(weight ~ Diet, data = chicks_0_21)

# load this for a non-parametic post hoc test
library(pgirmess)
kruskalmc(weight ~ Diet, data = chicks_0_21)
