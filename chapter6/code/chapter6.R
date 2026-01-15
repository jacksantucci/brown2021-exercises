##################################################
# Ch. 6 Exercises: Transforming Data
# Jack Santucci
# 2023-02-21
# To 'transform' data means to change the way information is stored in a variable. Broadly, we do this because most data sets are built to test other people's hypotheses. So, we do this with an eye to eventual hypothesis testing. Some transformations (e.g., from continuous to categorical variables) facilitate visualization. Others (such as the natural logarithm) are designed to make skewed distributions more normal (which is a core assumption of linear regression -- that variables are normally distributed). We might even transform variables to test nonlinear hypotheses (e.g., squaring a continuous variable to test for 'diminishing returns').
##################################################

##################################################
# DATA AND PACKAGES
##################################################

load('../data/Art_and_Practice_NEW.RData') # pull in all the data: nes, states, world, all of it....

library(ggplot2)

library(grid)

library(gridExtra)

library(ggrepel)

##################################################
# Continuous-to-categorical transformations: when our *theory* of the world holds that categories are relevant
##################################################

# Figure 6-3 -- Santucci note: this plot is hard to read
ggplot(nes, aes(birthyr, fttrump)) +
  geom_point(col="#bf0000") +
  ggtitle("Figure 6-3: Feelings Toward Trump and Year of Birth") +
  ylab("Trump Thermometer") +
  xlab("Year of Birth") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold"))

# Code Chunk 6-4
nes$age <- 2016 - nes$birthyr
nes$age5 = cut(nes$age, c(17,31,41,51,61,99,120)) # Santucci note: read the help file on the cut() command. The c() command here is creating a vector of 'breaks.' You can control whether your interval is right-open/left-closed (doesn't include right endpoint) or vice-versa. Right-open is the default. So, in Figure 6-4 below, the first bin DOES NOT include 31-year-olds.

# Figure 6-4
ggplot(nes, aes(age5, fttrump)) +
  geom_boxplot(col="#0000bf") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  ggtitle("Figure 6-4: Trump Thermometer by Age") +
  ylab("Trump Thermometer") +
  xlab("Age of Respondent")

##################################################
# Reducing the number of categories in a categorical variable: when the data come with too many to be useful/informative.
##################################################

# Code Chunk 6-9
nes$faminc.f <- as.factor(nes$faminc)
nes$nfam <- nes$faminc
nes$nfam[nes$nfam > 16] <- NA

#Code Chunk 6-10
nes$faminc4=cut(nes$nfam,breaks=c(0,3,7,10,16))
levels(nes$faminc4)=c("Low", "MedLow", "MedHigh", "High")

# Figure 6-7
ggplot(nes, aes(faminc.f, fttrump)) +
  geom_boxplot(col="#0000bf") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  ggtitle("Figure 6-7: Boxplot Has Too Many Categories") +
  ylab("Trump Thermometer") +
  xlab("Family Income") +
  coord_flip()

# Figure 6-8
ggplot(subset(nes, faminc4!="NA"), aes(faminc4, fttrump)) +
  geom_boxplot(col="#0000bf") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  ggtitle("Figure 6-8: Trump Is a Middle-Class Phenomenon") +
  ylab("Trump Thermometer") +
  xlab("Family Income") +
  coord_flip()

##################################################
# Box-Cox transformations: when the data are very skewed (because linear regression presumes a normal distribution) OR when your hypothesis calls for some other transformation. Santucci note: be careful with these, as they make statistical results (which we only briefly looked at) more difficult to interpret.
##################################################

# Figure 6-13
p1 <- ggplot(world, aes(gdppc)) +
  geom_histogram(bins=10, fill = "#0000bf") +
  labs(title = paste("A")) +
  xlab("GDP per Capita") +
  ylab("Count") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold"))

p2 <- ggplot(world, aes(sqrt(gdppc))) +
  geom_histogram(bins=10, fill = "#0000bf") +
  labs(title = paste("B")) +
  xlab("GDP per Capita (square root)") +
  ylab("Count") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold"))

p3 <- ggplot(world, aes(log(gdppc))) +
  geom_histogram(bins=10, fill = "#0000bf") +
  labs(title = paste("C")) +
  xlab("GDP per Capita (logged)") +
  ylab("Count") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold"))

p4 <- ggplot(world, aes(1/gdppc)) +
  geom_histogram(bins=10, fill = "#0000bf") +
  labs(title = paste("D")) +
  xlab("GDP per Capita (inverse)") +
  ylab("Count") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold"))

grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2, 
             top = textGrob("Figure 6-13: Panel A, Panel B, Panel C, and Panel D", 
                            gp=gpar(fontsize=10, fontface = "bold")))

# Figure 6-14
p1 <- ggplot(world, aes(gdppc, pwthc, label=iso3c)) +
  geom_point(col="#bf0000", size=0.5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  ggtitle("Figure 6-14: Four Transformations") +
  ylab("") +
  xlab("\n Raw GDP per Capita") +
  geom_text(size=3, data=subset(world, iso3c=="QAT"),
            aes(label=iso3c), hjust=1, vjust=2,
            show.legend=FALSE)

p2 <- ggplot(world, aes(sqrt(gdppc), pwthc)) +
  geom_point(col="#bf0000", size=0.5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  ylab("") +
  xlab("\n Square Root") +
  geom_text(size=3, data=subset(world, iso3c=="QAT"),
            aes(label=iso3c), hjust=1, vjust=2,
            show.legend=FALSE)

p3 <- ggplot(world, aes(log(gdppc), pwthc)) +
  geom_point(col="#bf0000", size=0.5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  ylab("") +
  xlab("\n Logged (base 10)") +
  geom_text(size=3, data=subset(world, iso3c=="QAT"),
            aes(label=iso3c), hjust=.5, vjust=2,
            show.legend=FALSE)

p4 <- ggplot(world, aes(1/gdppc, pwthc)) +
  geom_point(col="#bf0000", size=0.5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.title = element_text(size = 8, face = "bold")) +
  ylab("") +
  xlab("\n Inverse") +
  geom_text_repel(size=3, data=subset(world, iso3c=="QAT"),
                  aes(label=iso3c), hjust=-2, vjust=4,
                  show.legend=FALSE)

gridExtra::grid.arrange(p1,p2,p3,p4)
