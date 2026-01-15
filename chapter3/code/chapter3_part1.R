##################################################
# Chapter 3 in-class exercises: selected code from the textbook on the importance of describing (and knowing) our data. This replication packet is not a substitute for learning concepts from the textbook.
# Author: Jack Santucci
# Date: 2023-01-31
##################################################

##### Load all packages needed to do what is said on line 2 above

library(ggplot2)

library(gridExtra) # Needed to put two plots side-by-side in one figure. NOTE: you can just install all packages from the textbook at once, using the installD() and libraries() commands from the file Art_and_Practice.Rdata.

library(grid) # Needed for the textGrob() function. Otherwise see NOTE above.

library(tibble)

library(dplyr)

library(knitr)

library(kableExtra)

library(stringi)

library(memisc) # Needed to run the codebook() command.

##### Load necessary data

nes <- read.csv("../data/nes.csv")

world <- read.csv("../data/world.csv")

states <- read.csv("../data/states.csv")

##################################################
# Reproduce Figure 3-3 from Brown (2021)
##################################################

##### First, we need to convert the "marstat" variable to a factor-type variable (from R's perspective). Why? If you use the "getting started" files, you don't need to do this. When you use the "get started" materials, "character" vectors in CSV files are loaded as "factor" variables. However, if you load the data in the way I've been teaching (on an as-needed basis), the read.csv() command does not convert character vectors to factor variables. (A factor-type variable is a special data type in R for storing "categorical" and/or "ordered categorical" variables. All of this is odd to me because, when I use RGui, read.csv() imports text variables as factor-type by default.)

nes$marstat <- as.factor(nes$marstat)

# Code Chunk 3-3a
levels(nes$marstat)[levels(nes$marstat)=="Domestic Partnership"] <- "Dom. Part."

# Code Chunk 3-3b
nes$g <- factor(nes$marstat,levels(nes$marstat)[c(5, 1, 6, 2, 3, 4)])

# Code Chunk 3-3c
fig1 <- ggplot(nes, aes(x=marstat, y=ftgay)) +
  stat_summary(fun.y=mean, geom="bar", fill="#0000ff",
               aes(group=1)) +
  ylab("Not Ordered") +
  xlab("") +
  theme_minimal() +
  coord_flip(ylim=c(45,70)) +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))


fig2 <- ggplot(nes, aes(x=g, y=ftgay)) +
  stat_summary(fun.y=mean, geom="bar", fill="#0000ff",
               aes(group=1)) +
  ylab("Ordered") +
  xlab("") +
  theme_minimal() +
  coord_flip(ylim=c(45,70)) +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

# Figure 3-3

#pdf(file='../output/figure3-3.pdf') # As usual, if we wanted, we could store the figure to PDF (or JPG or whatever).

grid.arrange(fig1, fig2, ncol=2, 
             top = textGrob("Figure 3-3: Unordered and Ordered View of 
                            Marital Status and Feeling Thermometer Toward Gays",
                            gp=gpar(fontsize=10,fontface = "bold")))

#dev.off()

##################################################
# Reproduce Figures 3-5 and 3-6 (lessons on the shape of the distribution: normal versus skewed)
##################################################

# Figure 3-5
ggplot(world,aes(x = turnout)) +
  geom_histogram(aes(y = ..ncount..), fill = "#0000ff") +
  geom_density(aes(y = ..scaled..)) +
  theme_minimal() +
  ylab("Scaled Density and Observation Count") +
  xlab("Voter Turnout") +
  ggtitle("Figure 3-5: Voter Turnout Is Normally Distributed") +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

# Figure 3-6
ggplot(world, aes(x = inf)) +
  geom_histogram(aes(y = ..ncount..), fill = "#0000ff") +
  geom_density(aes(y = ..scaled..)) +
  theme_minimal() +
  ylab("Scaled Density and Observation Count") +
  xlab("Infant Mortality Rate") +
  ggtitle("Figure 3-6: Infant Mortality Is Not Normally Distributed") +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

##################################################
# Reproduce Figure 3-7 (lessons on the range of the distribution: not enough to make analysis/research 'interesting' in this case)
##################################################

# Figure 3-7
ggplot(states, aes(x = hsdiploma)) +
  geom_histogram(aes(y = ..ncount..), fill = "#0000ff") +
  geom_density(aes(y = ..scaled..)) +
  theme_minimal() +
  ylab("Scaled Density and Observation Count") +
  xlab("Percentage of Population With High School Diploma") +
  ggtitle("Figure 3-7: Not Much Separation Between States") +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

##################################################
# Reproduce Table 3-8 in two different ways
##################################################

##### First, change the ordering of the categories in pid7 (seven-point party identification). We also need to turn pid7 into a factor variable. When we load it from the nes.csv file, it comes into R as a character variable. We can do both at the same time.

unique(nes$pid7) # shows the unique values of the pid7 variable, which is currently a character vector

nes$pid7[nes$pid7=="Not sure"] <- NA # converts "not sure" answers to NA, which R understands as a missing value

nes$pid7 <- factor(x=nes$pid7, levels=c("Strong Democrat", "Weak Democrat", "Lean Democrat", "Independent", "Lean Republican", "Weak Republican", "Strong Republican")) # converts pid7 to a factor variable (ordered categorical) with  levels in the right/conventional order

##### Brown's approach (using tidyverse packages)

# Code Chunk 3-8a
avgaytherm <- as_tibble(subset(nes, pid7!="NA" & pid7!="Not sure"))

# Code Chunk 3-8b
dt <- avgaytherm %>%
  group_by(pid7) %>%
  summarise(ftpolice.mean = mean(ftpolice)) %>%
  mutate_if(is.numeric, format, digits=4)

# Table 3-2
kable(dt, format = "pandoc",
      caption = "Table 3-2: Democrats Show Less Support for Police")

##### Santucci using base R

for.table.3.2 <- subset(nes, select=c("pid7", "ftpolice"))

str(for.table.3.2) # see what kind of object we created
head(for.table.3.2) # have a look at the first six rows of what we did

### calculate conditional mean of ftpolice for each level of pid7

ftpolice.means <- c(
  mean(nes$ftpolice[nes$pid7=="Strong Democrat"], na.rm=T),
  mean(nes$ftpolice[nes$pid7=="Weak Democrat"], na.rm=T),
  mean(nes$ftpolice[nes$pid7=="Lean Democrat"], na.rm=T),
  mean(nes$ftpolice[nes$pid7=="Independent"], na.rm=T),
  mean(nes$ftpolice[nes$pid7=="Lean Republican"], na.rm=T),
  mean(nes$ftpolice[nes$pid7=="Weak Republican"], na.rm=T),
  mean(nes$ftpolice[nes$pid7=="Strong Republican"], na.rm=T)
  )

### combine categories (levels of pid7) with respective conditional means into a small data frame

table3.2.santucci <- cbind.data.frame(
  'PID7'=levels(nes$pid7),
  'ftpolice.mean'=ftpolice.means,
  'color'=c('darkblue','blue','lightblue','gray','pink','red','darkred')
  )

### round conditional means to 2 decimal places (as Brown does)

table3.2.santucci$ftpolice.mean <- round(table3.2.santucci$ftpolice.mean, digits=2)

##################################################
# Bonus track: visualize conditional means of police feeling thermometer over the range of party ID (seven-point)
##################################################

#table3.2.santucci <- as.matrix(table3.2.santucci)

barplot(table3.2.santucci$ftpolice.mean, names.arg=table3.2.santucci$PID7, col=table3.2.santucci$col)
