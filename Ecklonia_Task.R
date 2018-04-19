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





