##################################################
# Extract codebooks and variable descriptions from data sets included with Brown (2021).
# Jack Santucci
# 2023-02-02
##################################################

library(memisc) # This package includes the codebook() command, which lets you see what variables exist in each data set.

load('../Brown_1e_Code&Datasets/Art_and_Practice_NEW.RData') # This is a new version of the Art_and_Practice file that comes with the textbook. The old version DOES NOT contain variable information.

### Print descriptions of all variables in each data set (provided the description for some variable exists.)

codebook(states)

codebook(nes)

codebook(world)

### Print information for a single variable in a data set.

codebook(world$ethfrac)