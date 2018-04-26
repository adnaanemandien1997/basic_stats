# ANOVA_exercise.R
# Adnaan Emandien
# 25 April 2018
# FINAL

# Load libraries ----------------------------------------------------------

library(tidyverse)

# 7.4.1 Exercise 1 --------------------------------------------------------

# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))

# Does feed have an effect on pig weight?
# H0: There is no difference in pig weight after feeding on one of the four diets
# H1: There is a difference in pig weight after feeding on one of the four diets

pigs.aov <- aov(mass ~ feed, data = bacon)
summary(pigs.aov)

# Pr < 0.05, so we reject the null hypothesis
# Meaning that there is a difference in pig weight after feeding on one of the four diets
# Which feed has the biggest effect?

# Visualise with boxplots -------------------------------------------------

ggplot(data = bacon, aes(x = feed, y = mass, fill = feed )) +
  geom_boxplot(notch = TRUE)
# none of the notches overlap

# Tukey -------------------------------------------------------------------

TukeyHSD(pigs.aov)
# p-adj < 0.05 for all feeds: shows that all diets are different and significant
# lwr and upr does not go across zero in all cases

# Visualise this ----------------------------------------------------------

plot(TukeyHSD(pigs.aov))


# 7.4.2 Exercise 2 --------------------------------------------------------

teeth <- datasets::ToothGrowth
# Does differences in doses of orange juice have an effect on the length of tooth growth in guniea pigs
# H0: There is no difference in tooth lengths of guinea pigs receiving one of three doses of orange juice 
# H1: There is a difference in tooth lengths of guinea pigs receiving one of three doses of orange juice


# Filter out only Orange Juice doses --------------------------------------

teeth_oj <- ToothGrowth %>% 
  filter(supp == "OJ")

# ANOVA -------------------------------------------------------------------

teeth.aov <- aov(len ~ as.factor(dose), data = teeth_oj)
summary(teeth.aov)
# Pr < 0.05, so we reject the null hypothesis
# there is a difference in tooth lengths of guinea pigs receiving one of three doses of orange juice

# Which doses have the effect?

# Visualise using a boxplot -----------------------------------------------

ggplot(data = teeth_oj, aes(x = as.factor(dose), y = len, fill = as.factor(dose))) +
  geom_boxplot(notch = TRUE)
# none of the dose notches overlap, suggesting that they are different

# Tukey -------------------------------------------------------------------

TukeyHSD(teeth.aov)
# p-adj < 0.05, very low for all. so all doses are significant and different 
# lwr and upr does not cross zero

# Visualise this ----------------------------------------------------------

plot(TukeyHSD(teeth.aov))

# 7.4.3 Exercise 3 --------------------------------------------------------

teeth <- datasets::ToothGrowth

# H0: interactions between supplement and dose have NO effect on length of teeth
# H1: interactions between supplement and dose DO have an effect on length of teeth

# looking at only length by supplement
summary(aov(len ~ supp, data = teeth))

TukeyHSD((aov(len ~ supp, data = teeth)))

plot(TukeyHSD((aov(len ~ supp, data = teeth))))

# dose was done in previous example, but running too many ANOVAS increases error
# so... 

# now to look at interactions BETWEEN factors
summary(aov(len ~ supp * as.factor(dose), data = teeth)) 
# pr < 0.05, so we reject the null hypothesis
# so, interactions between supplement and dose DOES havae an effcet on length of teeth

# So which have the effect?
TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth)))

plot(TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth))))
# On plot, all combinations not crossing zero show which combinations of supplement and diet have the most effect on length of teeth 
