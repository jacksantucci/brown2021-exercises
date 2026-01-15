##################################################
# Chapter 10 exercises: exploring relationships between different kinds of variables
# NOTE: This code is not a substitute for reading the chapter. No code is, but the chapter is especially useful for thinking about different kinds of research questions, as well as the plots (and numeric tables!) you might use to explore them.
# Jack Santucci
# March 8, 2023
##################################################

##################################################
# Data and packages
##################################################

nes <- read.csv('../data/nes.csv')

world <- read.csv('../data/world.csv')

states <- read.csv('../data/states.csv')

library(ggplot2)

library(ggmosaic)

library(descr)

library(pander)

library(ggrepel)

##################################################
# Comparing two categorical variables
# Useful for asking, e.g., "are X more/less likely to do/say Y?"
##################################################

# Code Chunk 10-1
nes$pid3.new <- ifelse(nes$pid3 == "Other", NA, ifelse(nes$pid3 == "Not sure", NA, nes$pid3))

nes$pid3.new <- as.factor(nes$pid3.new)
levels(nes$pid3.new)=c("Democrat", "Rep.", "Indep.")

nes$pid3.new = factor(nes$pid3.new,levels(nes$pid3.new)[c(1,3,2)])
table(nes$pid3.new)

### Santucci approach

nes$pid3.new <- nes$pid3
nes$pid3.new[nes$pid3.new=="Indepent"] <- "Independent"
table(nes$pid3.new, exclude=F)
nes$pid3.new[nes$pid3.new %in% c("Other", "Not sure")] <- NA
nes$pid3.new <- factor(nes$pid3.new, levels=c("Democrat", "Independent", "Republican"))

# The code for constructing 'nes$dpolic.new' was not included in code chunk 10-1

nes$dpolice.new <- ifelse(nes$disc_police=="Treats blacks a little better" | nes$disc_police=="Treats blacks both the same" | nes$disc_police=="Treats blacks moderately better" | nes$disc_police=="Treats Blacks much better" | nes$disc_police=="Skipped", NA, nes$disc_police)
nes$dpolice.new <- as.factor(nes$dpolice.new)
levels(nes$dpolice.new)=c("Treats whites much better", "Treats whites moderately better", "Treats whites a little better", "Treats both the same")

# Figure 10-1
ggplot(data = na.omit(nes)) +
  geom_mosaic(aes(x = product(pid3.new, dpolice.new), fill=pid3.new, na.rm=TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold")) +
  xlab("") +
  ylab("") +
  coord_flip() +
  ggtitle("Figure 10-1: How Do Police Treat Whites and Blacks?") +
  scale_fill_brewer(palette="Blues") +
  theme(legend.position="none")

# Table 10-1

CrossTable(nes$dpolice.new, nes$pid3.new, prop.chisq=FALSE)

##################################################
# Comparing continuous with categorical variables
# Useful for asking, e.g., "is being in category X more/less associated with Y?"
##################################################

# create new pid7 with appropriate order (Santucci)
is(nes$pid7) # what kind of storage mode? (character vector in this case)
table(nes$pid7) # check existing values
nes$pid7.order <- factor(nes$pid7, levels=c("Strong Democrat", "Weak Democrat", "Lean Democrat", "Independent", "Lean Republican", "Weak Republican", "Strong Republican", "Not sure"))
head(cbind(nes$pid7, nes$pid7.order)) # check work

# Figure 10-3 -- Santucci note: I am using a re-ordered version of pid7 called pid7.order. This is not in the text.
ggplot(subset(nes, !is.na(pid7.order)), aes(pid7.order, ftobama)) +
  geom_boxplot(col="#0000ff") + theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold")) +
  ggtitle("Figure 10-3: Partisanship Determines Obama’s Popularity") +
  ylab("Obama Thermometer") +
  xlab("Party Identification") +
  coord_flip()

# Figure 10-4 -- Santucci note: this is a 'jitter plot.' Jitter means adding random noise to data points. (See help file for the jitter() command if you want.) Note how the data points in the plot below, for any given category of region, are NOT in the same place along the y-axis.
ggplot(world, aes(region, polity2, col=region)) +
  geom_jitter() +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold")) +
  theme(axis.text.x = element_text(size=8, vjust=.7),
        legend.position="none") +
  ggtitle("Figure 10-4: Most Authoritarian Regimes Reside in Africa") +
  ylab("Democracy (Polity2)") +
  xlab("") +
  scale_color_manual(values=c("#0000ff", "#008b00", "#bf0000", "#ff7300", "#00e6e6", "#263333", "#73e600")) +
  coord_flip()

##################################################
# Comparing two continuous variables
# Useful for asking, e.g., "does the amount of X help us predict the amount of Y?" Note that "predict" is not the same as "cause."
##################################################

ggplot(world, aes(gtbeduc, inf)) +
  geom_point(col="#bf0000") +
  geom_text_repel(size=3, vjust=2, col="black",
                  aes(label= ifelse(iso3c=="PAK" |
                                      iso3c=="YEM" |
                                      iso3c=="AFG" |
                                      iso3c=="SDN" |
                                      iso3c=="SWZ" |
                                      iso3c=="ETH",
                                    as.character(iso3c), ''))) +
  ggtitle("Figure 10-5: Infant Mortality and Women’s Education") +
  ylab("Infant Mortality: Deaths per 1,000 Live Births") +
  xlab("Ratio of Girls to Boys Educational Attainment") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"))

##### END OF EXERCISE.
##### I HIGHLY recommend working through the "exploratory data analysis" example in pp. 322-329. Here is a taste:

# Figure 10-8
ggplot(states, aes(democrat, abort)) +
  geom_point(aes(size=density), col="#0000ff") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold")) +
  theme(legend.position = c(.8, .2)) +
  ggtitle("Figure 10-8: Population Density Matters") +
  ylab("Abortions per 1,000 Women") +
  xlab("Percent of State That Are Democrat") +
  geom_text(size=3, vjust=-1.5, col="#04183d",
            aes(label= ifelse(st=="NY" |
                                st=="CA" |
                                st=="NJ" |
                                st=="WY" |
                                st=="UT" |
                                st=="DE" |
                                st=="ID", as.character(st), ''))) +
  guides(size=guide_legend("Population Density"))
