##################################################
# Chapter 11 exercises: 'controlled' comparisons (that hold some third variable constant... or 'account for' it)
# NOTE: "Arguments that focus on one cause to the exclusion of others are often wrong" (Brown 2021, p. 369).
# Jack Santucci
# March 14, 2023
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

library(gridExtra)

library(grid)

##################################################
# Comparing two categorical variables, controlling for a third
# Note from Ch. 10: Useful for asking, e.g., "are X more/less likely to do/say Y?"
##################################################

### Santucci approach

nes$pid3.new <- nes$pid3
nes$pid3.new[nes$pid3.new=="Indepent"] <- "Independent"
table(nes$pid3.new, exclude=F)
nes$pid3.new[nes$pid3.new %in% c("Other", "Not sure")] <- NA
nes$pid3.new <- factor(nes$pid3.new, levels=c("Democrat", "Independent", "Republican"))

# Figure 11-4: "relationship between views toward immigration and party identification, controlling for employment status" (p. 354)

nes$immig_numb2 <- nes$immig_numb
nes$immig_numb2[nes$immig_numb=="Increased a lot"] <- "Big increase"
nes$immig_numb2[nes$immig_numb=="Increased a moderate amount"] <- "Moderate increase"
nes$immig_numb2[nes$immig_numb=="Increased a little"] <- "Small increase"
nes$immig_numb2[nes$immig_numb=="Kept the same"] <- "Same"
nes$immig_numb2[nes$immig_numb=="Decreased a little"] <- "Small decrease"
nes$immig_numb2[nes$immig_numb=="Decreased a moderate amount"] <- "Moderate decrease"
nes$immig_numb2[nes$immig_numb=="Decreased a lot"] <- "Big decrease"

nes$immig_numb2 <- factor(nes$immig_numb2, levels=c("Big increase", "Moderate increase", "Small increase", "Same", "Small decrease", "Moderate decrease", "Large decrease"))

#levels(nes$immig_numb2)=c("Big increase", "Moderate increase", "Small increase", "Same", "Small decrease", "Moderate decrease", "Large decrease") # This is the code from the textbook.

## Santucci: check work

head(cbind(as.character(nes$immig_numb2), as.numeric(nes$immig_numb2)))

# Figure 11-6 -- SANTUCCI NOTE: see use of immig_numb2

p1 <- ggplot(data = subset(nes, nes$pid3.new!="NA" & nes$employ=="Full-time")) +
  geom_mosaic(aes(x = product(pid3.new, immig_numb2), fill=pid3.new,na.rm=TRUE)) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle("Employed") +
  xlab(label=NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face="bold"),
        axis.title = element_text(size = 8, face="bold")) +
  scale_fill_brewer(palette="Blues") +
  theme(axis.text.x = element_text(size=8, vjust=.5),
        axis.text.y = element_text(size=8, vjust=.5),
        legend.position = "none") +
  xlab("") +
  ylab("") +
  coord_flip()


p2 <- ggplot(data = subset(nes, nes$pid3.new!="NA" & nes$employ!="Full-time")) +
  geom_mosaic(aes(x = product(pid3.new, immig_numb2), fill=pid3.new,na.rm=TRUE)) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle("Unemployed") +
  xlab(label=NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face="bold"),
        axis.title = element_text(size = 8, face="bold")) +
  scale_fill_brewer(palette="Blues") +
  theme(axis.text.x = element_text(size=8, vjust=.5),
        axis.text.y = element_text(size=8, vjust=.5),
        legend.position = "none") +
  xlab("") +
  ylab("") +
  coord_flip()

grid.arrange(p1, p2, nrow=2, top=textGrob("Figure 11-6: 
            Employment Doesn’t Change the Relationship", gp=gpar(fontsize=8)))

## From textbook (p. 359): "Interestingly, Independents seem to be the most influenced by changes in job status. If employed full-time, Independents are a relatively larger proportion of those favoring increased immigration compared to their unemployed counterparts—compare the bottom three rows of the mosaic plot for the employed and the unemployed. Controlling for employment status does seem to make a slight difference for Independents."

# Figure 11-7 (SANTUCCI: again note immig_numb2)


p1 <- ggplot(data = subset(nes, nes$pid3.new!="NA" &
                             nes$pew_churatd=="More than once a week" & nes$pew_churatd!="NA")) +
  geom_mosaic(aes(x = product(pid3.new, immig_numb2),
                  fill=pid3.new,na.rm=TRUE)) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle("More than once a week") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face="bold"),
        axis.title = element_text(size = 8, face="bold")) +
  scale_fill_brewer(palette="Blues") +
  theme(axis.text.x = element_text(size=8, vjust=.5),
        axis.text.y = element_text(size=8, vjust=.5),
        legend.position = "none") + coord_flip()


p2 <- ggplot(data = subset(nes, nes$pid3.new!="NA" &
                             nes$pew_churatd!="More than once a week")) +
  geom_mosaic(aes(x = product(pid3.new, immig_numb2),
                  fill=pid3.new,na.rm=TRUE)) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle("Once a week or less") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face="bold"),
        axis.title = element_text(size = 8, face="bold")) +
  scale_fill_brewer(palette="Blues") +
  theme(axis.text.x = element_text(size=8, vjust=.5),
        axis.text.y = element_text(size=8, vjust=.5),
        legend.position = "none") +
  coord_flip()


grid.arrange(p1, p2, nrow=2, top=textGrob("Figure 11-7: 
            Church Attendance Changes the Relationship Between Party and Immigration", 
                                          gp=gpar(fontsize=8)))

##################################################
# Comparing two continuous variables, controlling for a third
# Note from Ch. 10: Useful for asking, e.g., "are X more/less likely to do/say Y?"
##################################################

# Code Chunk 11-13
world$democ <- ifelse(world$polity2 > 7, 1, 0)
world$democ <- as.factor(world$democ)
levels(world$democ)=c("Dictatorship", "Democracy")

# Figure 11-11
ggplot(subset(world, democ!="NA"), aes(log(gdppc), log(homicide),
                                       col=democ)) +
  geom_point() +
  ggtitle("Figure 11-11: Democracy Matters With Murder and Income") +
  ylab("Murder Rate (logged)") +
  xlab("GDP per Capita (logged)") +
  geom_smooth(method="lm", se=FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"),
        legend.title = element_text(size = 8, face = "bold")) +
  theme(legend.position = c(.3, .2)) +
  guides(col=guide_legend("Regime Type")) +
  scale_color_manual(breaks = c("Democracy", "Dictatorship"),
                     values=c("#bf0000", "#0000ff"))
