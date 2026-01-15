##################################################
# In-class exercises for chapter 4: central tendency, dispersion, and outliers
# Research questions: what is the typical case? How typical is that case?
# Jack Santucci
# 2023-02-14
##################################################

##### Load packages and data

load('../data/Art_and_Practice_NEW.RData')

library(ggplot2)

library(magrittr)

library(tidyverse)

##################################################
# Central tendency
##################################################

##### Categorical data: plot and calculate mode

# Figure 4-1
ggplot(nes, aes(x=pid7)) +
  geom_bar(fill="#0000ff") +
  theme_minimal() +
  ggtitle("Figure 4-1: Strong Democrats Outnumber Strong Republicans") +
  xlab("Party Identification") +
  ylab("Number of Respondents") +
  coord_flip() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

# calculate mode (Santucci)

mode(nes$pid7) # NO! This gives you a data structure. There is no function for mode.

which.max(summary(nes$pid7)) # or...

nes$pid7 %>% summary %>% which.max # need magrittr package to use pipe operator

##### Continuous data: plot and calculate mean

# Code Chunk 4-2a
mean(world$homicide, na.rm = TRUE)

# Figure 4-2
ggplot(world, aes(homicide)) +
  geom_histogram(bins=20, fill = "#0000ff") +
  labs(title="Figure 4-2: Average Homicide Rate for the World") +
  xlab("Number of Homicides per 100,000 Population") +
  ylab("Count") +
  geom_vline(xintercept=8.45, col="#bf0000") +
  annotate("text", x = 12, y = -7, label = "8.45", col="#bf0000", size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

##### Continuous data: plot and calculate median (better measure of central tendency when there are many outliers)

# Code Chunk 4-3a
median(world$homicide, na.rm = TRUE)

# Figure 4-3
ggplot(world, aes(homicide)) +
  geom_histogram(bins=20, fill = "#0000ff") +
  ggtitle("Figure 4-3: Median Homicide Rate for the World") +
  xlab("Number of Homicides per 100,000 Population") +
  ylab("Count") + 
  geom_vline(xintercept=3.3, col="#bf0000") +
  annotate("text", x = 7, y = -7, label = "3.3", col="#bf0000", size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

##### Continuous data: plot and compare mean and median

# Figure 4-4
ggplot(world, aes(durable)) +
  geom_histogram(bins=20, fill = "#0000ff", alpha=.5) +
  ggtitle("Figure 4-4: The Mean and Median Differ") +
  xlab("Number of Years Since Significant Constitutional Change") +
  ylab("Count") +
  geom_vline(xintercept=27.2, col="#bf0000") +
  geom_vline(xintercept=17, col="#0000ff") +
  annotate("text", x = 100, y = 30,
           label = "Mean = 27.2", col="#bf0000", size = 3) +
  annotate("text", x = 100, y = 35,
           label = "Median = 17", col="#0000ff", size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

# Brown finds error in the data (U.S. observation)

# Code Chunk 4-5a
world$newdurable <- world$durable
world$newdurable <- replace(world$newdurable,
                            world$newdurable==199, 1999)

mean(world$newdurable, na.rm=TRUE)
## [1] 38.61392

# Figure 4-5
ggplot(world, aes(durable)) +
  geom_histogram(bins=20, fill = "#0000ff", alpha=.5) +
  labs(title="Figure 4-5: Extreme Value Moves the Mean") +
  xlab("Number of Years Since Significant Constitutional Change") +
  ylab("Count")+
  geom_vline(xintercept=27.2, col="#bf0000") +
  geom_vline(xintercept=17, col="#0000ff") +
  geom_vline(xintercept=38.6, col="#00ff00",
             linetype = "dashed") +
  annotate("text", x = 125, y = 30,
           label = "Mean With Typo Moves From 27.2 to 38.6",
           col = "#00ff00", size = 3) +
  annotate("text", x = 125, y = 25,
           label = "Median Stays the Same = 17", col = "#0000ff", size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

##################################################
# Dispersion
##################################################

##### Range

summary(states$infant)

# What is the interquartile range? Give the distance between the 25th and 75th percentile (the middle half of the data)

##### Standard deviation (Average distance from the mean)

summary(states$femleg, na.rm=T) # from Santucci

sd(states$femleg, na.rm=T)

##### The standard deviation increases with the range of the data.

# Code Chunk 4-13a
h <- data.frame(1:500)
set.seed(42)
h$p1 <- rnorm(500,0,1) # Santucci: 500 random draws from a normal distribution. We tell R that this 'fake data' has a mean of 0 and standard deviation of 1.
h$p2 <- rnorm(500,0,2)
h$p3 <- rnorm(500,0,3)

# Code Chunk 4-13b

data_long <- gather(h, condition, measurement,
                    p1:p3, factor_key=TRUE)

data_long$condition <- factor(data_long$condition,
                              levels = c("p1","p2","p3"),
                              labels = c("Std=1", "Std=2",
                                         "Std=3"))

# Figure 4-6
ggplot(data_long, aes(measurement)) +
  geom_histogram(aes(measurement), fill = "#0000ff") +
  facet_grid(.~condition) +
  labs(title="Figure 4-6: Standard Deviation of Random Variable Doubled and Tripled") +
  xlab("") +
  ylab("Count") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

