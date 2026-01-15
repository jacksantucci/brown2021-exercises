##################################################
# Chapter 2 in-class exercises: doing the same thing in ggplot and base R
# Author: Jack Santucci
# Date: 2023-01-19
##################################################

##### Load all packages needed to do what is said on line 2 above

library(ggplot2)

library(ggrepel)

# Note: if you do not have a package, you can install it by running the install.packages() command. For example, if I need the package ggrepel, I can run: install.packages("ggrepel").

##### Load necessary data

world <- read.csv("../data/world.csv")

##################################################
# Reproduce Figure 2-1 from Brown (2021) using the ggplot2 package
##################################################

pdf("../output/fig2-2-ggplot.pdf")

ggplot(world, aes(ethfrac, log(gdppc), label = iso3c)) + geom_point(color="#0000ff") + geom_smooth(method="lm", se=FALSE, color="#00ff00") + ggtitle("Figure 2-1: ELF Lowers Income") + ylab("GDP per Capita (logged)") + xlab("Ethnolinguistic Fractionalization (ELF)") + theme_minimal() + theme(plot.title = element_text(size = 8, face = "bold"), axis.title = element_text(size = 8, face = "bold")) + geom_text_repel(size = 2.8, aes(label=ifelse(iso3c=="MDG" | iso3c=="LUX", as.character(iso3c),''), hjust = 0, vjust=-1), show.legend=FALSE)

dev.off()

##################################################
# Reproduce Figure 2-1 (or get as close as possible) without using ggplot() in the ggplot2 package
##################################################

### First, fit the linear model used to draw the line of best fit

lm.out <- lm(log(gdppc) ~ ethfrac, data=world) # a linear model predicting GDP per capita from ethnolinguistic fractionalization

### Plotting

pdf("../output/fig2-2-base.pdf") # initializes graphics device

plot(x=world$ethfrac, y=log(world$gdppc), xlab="Ethnolingustic fractionalization (ELF)", ylab="GDP per capita (logged)", pch=20, col="blue", main="Fig. 2-1: ELF lowers income", axes=F) # makes the basic plot

axis(1, tick=F, at=c(0, 0.25, 0.5, 0.75)) # adds the x-axis without "ticks," at locations specified in the "at=" argument.

axis(2, tick=F, las=1) # adds the y-axis without "ticks," rotating the numbers so that the reader does not need to turn their head to the side

## Now add the ISO 3-letter country codes for Luxembourg and Madegascar, as in the textbook. There are many different ways to do this!

text(x=world$ethfrac[world$iso3c=="LUX"], y=log(world$gdppc[world$iso3c=="LUX"]), labels="LUX", pos=4, cex=2/3, font=2) # pos=4 tells R to put the text to the right of the data point. cex=2/3 says to make the font size 2/3 of default. font=2 says to boldface it.

text(x=world$ethfrac[world$iso3c=="MDG"], y=log(world$gdppc[world$iso3c=="MDG"]), labels="MDG", pos=4, cex=2/3, font=2)

## Now add the line of best fit, in green, as in the textbook.

abline(lm.out, col="green", lwd=2) # lwd=2 says to make the line segment 2x as thick (or heavy) as default.

## If we wanted, we could add the gray lines that run vertically and horizontally through the plot. We probably should do this BEFORE we add the data points -- you'll see that some of the lines are above the data points -- but this would complicate the code. Namely, we'd need to draw the lines before the points. We can talk about how to do that if you are interested...

abline(v=c(0, 0.25, 0.5, 0.75), col="lightgray") # see if you can figure out what's going on here

abline(h=c(7, 8, 9, 10, 11), col="lightgray")

dev.off()

##################################################
# A simpler plot, done in base R, that minimizes "ink" and plots the country codes. Good for thinking about the data.
##################################################

pdf("../output/fig2-2_base_santucci.pdf")

plot(x=world$ethfrac, y=log(world$gdppc), main="Fig. 2-1: ELF lowers income", xlab="Ethnolingustic Fractionalization (ELF)", ylab="GDP per capita (logged)", pch=NA, axes=F, xlim=c(0,1), ylim=c(6, 12)) # pch=NA here suppresses the data points so that text can be added instead. xlim=c(0,1) sets the x-dimension of the plot to run from 0 to 1, which is the range of possible values of ELF. ylim=c(6,12) sets the y-dimension of the plot to run from 6 to 12, which are values I chose because I felt they made things look cleaner.

text(x=world$ethfrac, y=log(world$gdppc), labels=world$iso3c, cex=2/3) # add text instead of data points. This can be done with the ggplot2 package as well.

axis(1, tick=F)

axis(2, las=1, tick=F)

abline(lm.out)

dev.off()