# Day_6.R
# Confidence intervals
# Adnaan Emandien
# 26 April 2018


# load libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(rcompanion)
dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95, digits = 3)



# Load data ---------------------------------------------------------------

Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)
# Create the graph --------------------------------------------------------

library(ggplot2)

ggplot(data = dat1, aes(y = Mean, x = Sex)) +
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes(ymin = Mean - Trad.lower,
                    ymax = Mean + Trad.upper,
                    colour = Teacher))
# Calculate confidence and plot them around the mean in this graphs
facet_wrap(~Teacher)


# Testing assumptions -----------------------------------------------------

# log transformed, square root, cubic root

dat2 <- data %>%
  mutate(logdata = log(Steps),
         log10 = log10(Steps),
         sqrt = sqrt(Steps),
         cubert = (Steps)^(1/3))

ggplot(data = dat2, aes(x = Steps)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")
logplot <- ggplot(data = dat2, aes(x = logdata)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")
log10plot <- ggplot(data = dat2, aes(x = log10)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")
sqrtplot <- ggplot(data = dat2, aes(x = sqrt)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")
cubertplot <- ggplot(dat = dat2, aes(x = cubert)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")
ggarrange(logplot, log10plot, sqrtplot, cubertplot,
          ncol = 2, nrow = 2,common.legend = TRUE)

# mutate(ln.step = log(Steps),
        #log10.step = log10(Steps),
        #cube.step = Steps^(1/3),
        #sqrt.step = sqrt(Steps)) %>% 
# Select(-Student, -Rating) %>% 
# Gather(key = "data.type", value = "trans.data",
          #-Sex, -Teacher)

# ggplot (data = dat2, aes( x = trans.data)) +
#  geom_histogram() +
#  facet_grid(Sex ~ Teacher)


# Iris data ANOVA ---------------------------------------------------------

iris.dat <- as.tibble(iris)

# H0: There is NO significant differences is petal.width between the three iris species

shapiro.test(iris$Petal.Width)
# p < 0.05

iris %>%
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2]))
# For setosa and versicolor p < 0.05 = normal
# For virginica p > 0.05 = non-normal
# We can thus take the whole group as non-normal

# Do a Fruskal-Wallis test in stead  of an ANOVA

kruskal.test(Petal.Width ~ Species, data = iris)
