##################################################
# Chapter 5 in-class exercises, part II: bivariate (two-variable) description
# Note: please read chapter 4 on inspection of univariate (one-variable) distributions. The key idea here is how to identify outliers... or errors in the data (typos).
# Overarching issue: why it's better to plot (and know) the data than to rely on a single number (such as correlation... or standard deviation from last class, etc.)
# Jack Santucci
# 2023-02-15
##################################################

##################################################
# Load packages and data
##################################################

load('../data/Art_and_Practice_NEW.RData') # pull in all the data: nes, states, world, all of it....

library(ggplot2) # needed for plotting

library(ggrepel) # needed for some aspect of plotting that I don't yet understand. Seems to relate to the placement of text in scatterplots... or maybe a larger set of plots. I don't know.

library(ggmosaic) # needed for mosaic plots

##################################################
# Lesson 1: Correlation hides information (a nonlinear relationship in this case)
##################################################

###### One number to describe the entire relationship?

cor(world$ethfrac, log(world$gdppc), method = "pearson", use = "complete.obs")

###### The relationship is nonlinear, and the figure shows us that certain countries (denoted by a different variable) might be responsible for the relationship. Note the geom_smooth layer below where industrial countries are omitted (with != or "not equals").

# Figure 5-14
ggplot(world, aes(ethfrac, log(gdppc))) +
  geom_point(color=ifelse(world$aclpregion=="Industrial Countries", "#0000bf", "#bf0000")) +
  geom_text_repel(size = 2.8, aes(label=ifelse(iso3c=="COM" | iso3c=="HTI" | iso3c=="KIR" | iso3c=="SLB", as.character(iso3c),''))) +
  geom_smooth(method="lm", col="grey", se=FALSE,
              linetype = "dashed") +
  ggtitle("Figure 5-14: The Relationship Could Be Nonlinear") +
  ylab("GDP per Capita (logged)") +
  xlab("Ethnolinguistic Fractionalization (ELF)") +
  theme_minimal() +
  geom_smooth(data=subset(world, aclpregion != "Industrial Countries"), color="black", se=FALSE) +
  annotate("text", x = .15, y = 11.5, size = 3, label = "Industrial Countries", col="#0000bf") +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold"))

##################################################
# Lesson 2: How do I describe (look at) the relationship between a continuous and categorical variable?
##################################################

##### BOXPLOT of feeling thermometer toward gays (not my word) by party ID. Interquartile ranges (boxes) tell a different story than the medians (heavy line segments inside boxes). What to make of this? Discuss.

# Figure 5-15
ggplot(subset(nes, pid7!="NA" & pid7!="Not sure"), aes(pid7, ftgay)) +
  geom_boxplot(col="#0000ff") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=8, angle=0, vjust=.7)) +
  ggtitle("Figure 5-15: Partisanship Shapes Attitudes") +
  ylab("Gay Thermometer") +
  xlab("Party Identification") +
  coord_flip() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold"))

##################################################
# Lesson 3: How do I describe (look at) the relationship between two categorical variables?
##################################################

##### MOSAIC PLOT of worry about terrorism by party identification

# Code Chunk 5-22
nes$pid3.new <- ifelse(nes$pid3 == "Other", NA,
                       ifelse(nes$pid3 == "Not sure", NA, nes$pid3))

# Code Chunk 5-23
nes$pid3.new <- as.factor(nes$pid3.new)

levels(nes$pid3.new)=c("Democrat", "Republican", "Independent")

nes$pid3.new = factor(nes$pid3.new,levels(nes$pid3.new)
                      [c(1,3,2)])

table(nes$pid3)
table(nes$pid3.new)

#Code Chunk 5-26
nes$var2 <- droplevels(nes$terror_worry, "Not asked")


# Figure 5-20
ggplot(data = subset(nes, pid3.new!="NA" & var2 !="NA")) +
  geom_mosaic(aes(x = product(var2, pid3.new), fill=var2, na.rm=TRUE)) +
  xlab("") +
  ylab("") +
  ggtitle("Figure 5-20: Democrats Are Less Worried About Terrorism") +
  theme_minimal() +
  scale_fill_brewer(palette="Blues") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold"))

##################################################
# Lesson 4: Does a third factor matter? (Or, more precisely, do I need to learn more about my data?)
# Santucci note: I don't see much when we add percent of population that is evangelical.
##################################################

##### BUBBLE PLOT of share of women in state legislatures BY percent of state population that has HS education. Scale the data points (dots) by percentage of the state that is evangelical.

# Figure 5-21
ggplot(states, aes(hsdiploma, femleg)) +
  geom_point(aes(size=evangel), col="#0000bf") +
  theme_minimal() +
  ggtitle("Figure 5-21: Women Legislators, Education, and Evangelism") +
  ylab("Percentage of Legislature That Are Women") +
  xlab("Percentage of the State’s Population With High School Diploma") +
  scale_size(range = c(.1, 12), name="Evangelism") +
  geom_text(size = 2.8,
            aes(label=ifelse(st=="CA" |
                               st=="LA" |
                               st=="UT" |
                               st=="SC" |
                               st=="NV" |
                               st=="PA" |
                               st=="MD", as.character(st),''),
                hjust = 2.1, vjust=-1), show.legend=FALSE) +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) #+
  # geom_smooth(method="lm", se=T)

# santucci note: the geom_smooth call above was me messing around. I commented it out.

###### Santucci bonus track: how to do something similar in base R

attach(states) # makes it so that we don't have to use the dollar sign

## estimate a linear model

lm.femdip <- lm(femleg ~ hsdiploma)

plot(hsdiploma, femleg, cex=evangel/10)
text(hsdiploma, femleg, labels=ifelse(st %in% c("CA", "LA", "UT", "SC", "NV", "PA", "MD"), st, NA), pos=4) # pos=4 here means 'to the right of the data point'

abline(lm.femdip)

detach(states)

###### Santucci trolling... or asking scientific questions?

summary(lm.femdip) # Look! A statistically significant relationship. But... check out the size of the intercept. And then check out the magnitude of the coefficient on the variable of interest (diploma attainment). This is not a very strong relationship. Why? Discuss.

cor(states$hsdiploma, states$femleg) # Big correlation (ish), but so what?
