##################################################
# Chapter 1 exercises
# by Jack Santucci
# Written: 2023-01-12
# Modified on: NA
##################################################

##### Set working directory to the "code" folder in the "chapter1" folder.

## There are three ways to do that.

# 1. You can put the file path in the following command: setwd('[insert file path here]')
# 2. In RStudio, you can follow these directions: https://www.youtube.com/watch?v=htwrxk3wYOw
# 3. In RGui, you can go to the "Misc" menu and choose "Change Working Directory."

##### Load data and packages

## We are not using any packages for this exercise.

states <- read.csv(file='../data/states.csv') # This creates a data frame called "states"

##### Plot slightly different version of figure 1.1 from Brown (2021).

plot(x=states$medinc, y=states$stuspend, main='Median income by per capita student spending (U.S. states)', xlab="Income", ylab="Spending")

##### Save the figure to your "output" folder.

pdf(file='../output/income_by_student-spending_us-states.pdf') # tells R that we are going to create a PDF (using something called the "graphics device")

plot(x=states$medinc, y=states$stuspend, main='Median income by per capita student spending (U.S. states)', xlab="Income", ylab="Spending")

dev.off() # exits the graphics device