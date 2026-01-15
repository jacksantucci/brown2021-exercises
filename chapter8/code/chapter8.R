##################################################
# In-class exercises for Chapter 8 (fundamentals of probability theory)
# Jack Santucci
# 2023-02-27
# Our key goals are to introduce the Law of Large Numbers (LLN) and Central Limit Theorem (CLT). The overarching research question is: if we are interested in some summary statistic, how close can we get with a random sample to the full-population value of that statistic?
##################################################

##################################################
# Load packages
##################################################

library(reshape) # for melt()

library(ggplot2) # for ggplot-style plots

library(gridExtra) # for multi-panel plots

##################################################
# Illustrate LLN
##################################################

### SANTUCCI: Key idea here is that a sample statistic (like the mean) gets closer to the true statistic (mu below) as the number of samples increases. What is the sample statistic? It is some quantity of interest that we compute from the sample (mean, standard deviation, whatever).

### For LLN, we are holding constant the size of each sample (n=10).

# chunk 8-1

set.seed(3376)
mu=100; sigma=10; n=10

# chunk 8-2 (with slight Santucci modification... because this is a great way to illustrate the idea of the for-loop)

xbar1=rep(0,5) # create place to put data from for-loop. Note second argument: 5. See help file for rep() if necessary by passing "?rep" to the console.
for (i in 1:5) { # do the same thing 5 times
	xbar1[i]= mean(rnorm(n, mean=mu, sd=sigma)) # draw n=10 random values from a normal distribution centered on mean=mu=100 and sd=sigma=10. Do this five times. Take the mean each time, and store it to one of the five 'spots' in xbar1.
}

mu=100; sigma = 10; n=10
xbar2=rep(0,10)
for (i in 1:10) {xbar2[i]=mean(rnorm(n, mean=mu, sd=sigma))}

mu=100; sigma = 10; n=10
xbar3=rep(0,100)
for (i in 1:100) {xbar3[i]=mean(rnorm(n, mean=mu, sd=sigma))}

# chunk 8-4 -- SANTUCCI: this will show us how the 'mean of means' gets closer to the true mean (which is mu=100) as the number of random samples increases.

x <- list(v1=xbar1,v2=xbar2,v3=xbar3) # combine the vectors of sample means into a list... of vectors of sample means
data <- melt(x) # flatten that list into a single vector (really, a data frame with two variables. Why two? Because things get messy when we use melt.)
xb1 <- mean(xbar1)
 xb2 <- mean(xbar2)
 xb3 <- mean(xbar3)
ggplot(data,aes(x=value, fill=L1)) +
  geom_density(alpha=0.50) +
  ggtitle("Figure 8-2: Increased Draws, Increased Accuracy") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  geom_vline(xintercept=xb1, col="#bf0000") +
  geom_vline(xintercept=xb2, col="#008b00") +
  geom_vline(xintercept=xb3, col="#0000ff") +
  annotate("text", x = 95, y = .15, label = "Mean with 5 draws = 98.895", col="#bf0000") +
  annotate("text", x = 95, y = .175, label = "Mean with 10 draws = 100.481", col="#008b00") +
  annotate("text", x = 95, y = .20, label = "Mean with 100 draws = 100.071", col="#0000ff") +
  theme(legend.position = "none")

##################################################
# Illustrate CLT
##################################################

### SANTUCCI: Now we will examine how the size of the sample affects the resulting distribution . We are holding constant the number of samples (10 for each sample size)... but increasing the size of each sample (n). Key idea here: as sample size increases, shape of distribution becomes more normal.

set.seed(3376)

# 10 means of 10 random draws of 5 values

mu=100; sigma=10; n=5
xbar1=rep(0,10)
for (i in 1:10) {xbar1[i]=mean(rnorm(n, mean=mu, sd=sigma))}

# 10 means of 10 random draws of 10 values

mu=100; sigma = 10; n=10
xbar2=rep(0,10)
for (i in 1:10) {xbar2[i]=mean(rnorm(n, mean=mu, sd=sigma))}

# 10 means of 10 random draws of 100 values

mu=100; sigma = 10; n=100 # 100 
xbar3=rep(0,10)
for (i in 1:10) {xbar3[i]=mean(rnorm(n, mean=mu, sd=sigma))}

x <- list(v1=xbar1,v2=xbar2,v3=xbar3)
data <- melt(x)
ggplot(data,aes(x=value, fill=L1)) +
  geom_density(alpha=.70) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  ggtitle("Figure 8-3: Increasing Observations Increases
Normality") +
  annotate("text", x = 95, y = .15, label = "5 Observations per Draw", col="#bf0000") +
  annotate("text", x = 95, y = .175, label = "10 Observations per Draw", col="#008b00") +
annotate("text", x = 95, y = .20, label = "100 Observations per Draw", col="#0000ff") +
  theme(legend.position = "none") +
  xlab("Mean of Sample Means") + #Santucci addition: mean of means
  geom_vline(xintercept=mean(data$value), lty=3) +
  annotate("text", x=mean(data$value), y=0.1, label=paste0(round(mean(data$value), digits=1)))
  
##################################################
# Even if the population distribution is not normal, the sampling distribution of the parameter will be (p. 252).
##################################################

# chunk 8-10: 100k draws from a uniform distribution

xunif <- runif(100000, min = 50, max = 150)
x <- data.frame(xunif)
p <- ggplot(x, aes(xunif)) +
  geom_density(alpha=.7, fill="#0000ff") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  annotate("text", x = 80, y = .012, label = "Mean = 100", col="#bf0000") +
  geom_vline(xintercept=100, col="#bf0000") +
  ggtitle("Figure 8-6a: Uniform Distribution") +
  xlab("Uniform Distribution")

# chunk 8-11: mean of each of 500 samples from the same uniform distribution

set.seed(3376)
min=50; max=150; n=50
xbar1=rep(0,500)
for (i in 1:500) {xbar1[i]=mean(runif(n, min, max))}
x <- data.frame(v1=xbar1)
xb1 <- mean(x$v1)
p1 <- ggplot(x,aes(x=v1)) +
  geom_density(fill="#0000ff", alpha=.7) +
  geom_vline(xintercept=xb1, col="#bf0000") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  annotate("text", x = 107, y = .10, label = "Mean = 99.689", col="#bf0000") +
  ggtitle("Figure 8-6b: Sampling Distribution From Uniform Distribution") +
  xlab("Mean of Sample") +
  theme(legend.position = "none")
grid.arrange(p, p1, ncol=2)

##################################################
# Research question: how close is our sample mean to the population mean? CLT lets us say... because the shape of the sampling distribution approaches normalitly as sample size (N) increases. NOTE: again, the "sampling distribution" refers to the distribution of some sample statistic (quantity of interest). So, if we know things about the normal distribution, we can say how uncertain we are about the difference between our sample statistic and the same from the population (from which we drew our sample).
##################################################

# chunk 8-16

mean=0; sd=1 # distibution parameters
lb= -1; ub=1 # lower and upper bounds of shaded area (one standard deviation). Brown advises playing with these bounds (p. 261).
x <- seq(-4,4,length=10000)*sd + mean # some variable x that is (looks?) uniformly distributed
hx <- dnorm(x,mean,sd) # density of x
plot(x, hx, type="n", xlab="", ylab="", main="", axes=FALSE) # to see what this would produce if we did not suppress "type," run plot(x, hx).
i <- x >= lb & x <= ub # logical variable indicating whether each value of x is within or equal to the lower and upper bounds
lines(x, hx) # draw distribution
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="#0000bf") # fill area within one standard deviation of mean
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd) # calculate that area
result <- paste("P(",lb,"< 0 <",ub,") =", signif(area, digits=3)) # probability that a value draw from x is within one SD of the mean
mtext(result, 3)
axis(1, at=seq(-5, 5, 1), pos=0)

## IQ of Norwegian 8th graders (p. 263). Santucci code.

library(MASS)
data(nlschools)

z.score <- (15-mean(nlschools$IQ))/sd(nlschools$IQ)

pnorm(z.score)

# chunk 8-17 -- only 6% of students scored higher than the one above

v1 <- seq(-4, 4, length = 10000)
v2<- dnorm(v1, 0, 1)
df <- data.frame(v1, v2)
ggplot(df, aes(v1, v2)) +
  geom_line() +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  geom_ribbon(data=subset(df ,v1>1.51 & v1<4),aes(ymax=v2),ymin=0, fill="#0000bf",colour=NA) + # Santucci note: we could replace 1.51 with the z.score object I created above.
  scale_fill_brewer(guide="none") +
  xlab("Standard Deviations From Mean") +
  ylab("Density") +
  geom_vline(xintercept = 1.51, linetype=2) +
  scale_x_continuous(sec.axis=sec_axis(~.*2.06+11.88))