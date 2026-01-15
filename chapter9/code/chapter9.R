##################################################
# Chapter 9 exercises: testing for significant differences between two sample means
##################################################

##### Load packages, data...

nes <- read.csv('../data/nes.csv')

states <- read.csv('../data/states.csv')

library(ggplot2)

library(knitr)

library(pander)

##### Using fake data (v1), ask whether a $10 difference in cash-on-hand between two groups is statistically significant. In other words, can we say so with 95% confidence?

# Figure 9-5
v1 <- seq(-4, 4, length = 10000)
v2<- dt(v1, 30)
df <- data.frame(v1, v2)

ggplot(df, aes(v1, v2)) +
  geom_line() +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold")) +
  geom_ribbon(data=subset(df ,v1>2.042 & v1<4),aes(ymax=v2),ymin=0,
              fill="#0000bf",colour=NA) +
  geom_ribbon(data=subset(df ,v1< -2.042 & v1> -4),aes(ymax=v2),ymin=0,
              fill="#0000bf",colour=NA) +
  scale_fill_brewer(guide="none") +
  ggtitle("Figure 9-5: A $10 Difference Is Significant") +
  xlab("Standard Deviations") +
  ylab("Density") +
  geom_vline(xintercept = 2.5, linetype=2) +
  geom_vline(xintercept = -2.5, linetype=2) +
  scale_x_continuous(sec.axis=sec_axis(~.*4.00+0,
                                       name = "Difference in Dollars"))

##### Two hypothesis tests using NES data.

# Table 9-2
nes$white <- ifelse(nes$race=="White", 1, 0)
p <- t.test(ftpolice ~ white, data=nes)
pander(p)

# Table 9-3
p <- t.test(medinc ~ trumpwin, data = states)
pander(p)

t.test(medinc ~ trumpwin, data=states)
