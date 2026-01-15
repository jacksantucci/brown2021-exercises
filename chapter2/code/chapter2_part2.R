##################################################
# Chapter 2 in-class exercises, part 2: Reproduce selected examples including regression tables and diagnostic plots
# Author: Jack Santucci
# Date: 2023-01-24
##################################################

##### Load all packages needed to do what is said on line 2 above

library(ggplot2)

library(ggrepel)

library(stargazer) # Needed for tables of regression output

library(visreg) # Needed for plotting predicted values (of regression model) against the key independent variable.

# Note: if you do not have a package, you can install it by running the install.packages() command. For example, if I need the package ggrepel, I can run: install.packages("ggrepel").

##### Load necessary data

world <- read.csv("../data/world.csv")

##################################################
# Reproduce Figures 2-3, 2-4, and 2-9 from Brown (2021) using the ggplot2 package
##################################################

##### Figure 2-3. We are not storing this to PDF because we are just looking at the data.

ggplot(world, aes(ethfrac, log(gdppc))) +
  geom_point(color=ifelse(world$aclpregion=="Industrial Countries", "#0000bf", "#bf0000")) +
  geom_smooth(method="lm", col="grey", se=FALSE, linetype = "dashed") +
  ggtitle("Figure 2-3: The Relationship Could Be Nonlinear") +
  ylab("GDP per Capita (logged)") +
  xlab("Ethnolinguistic Fractionalization") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold")) +
  geom_smooth(data=subset(world, aclpregion != "Industrial Countries"), color="black", se=FALSE) +
  annotate("text", x = .15, y = 11.5, label = "Industrial Countries", col="blue")

##### Figure 2-4. No PDF for the same reason.

ggplot(world, aes(ethfrac, log(gdppc))) +
  geom_point(color=ifelse(world$iso3c=="COM" |
                            world$iso3c=="HTI" |
                            world$iso3c=="KIR" |
                            world$iso3c=="SLB", "#0000bf", "grey")) +
  geom_smooth(method="lm", col="grey", se=FALSE, linetype = "dashed") +
  ggtitle("Figure 2-4: Small Countries Have Low GDP per Capita") +
  ylab("GDP per Capita (logged)") +
  xlab("Ethnolinguistic Fractionalization") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold")) +
  geom_smooth(data=subset(world, aclpregion != "Industrial Countries"), color="black", se=FALSE) +
  geom_text_repel(size = 2.8, aes(label=ifelse(iso3c=="COM" |
                                                 iso3c=="HTI" |
                                                 iso3c=="KIR" |
                                                 iso3c=="SLB",
                                               as.character(iso3c),'')))

##### Figure 2-9. Note how he creates a "dummy" or "indicator" variable for Sub-Saharan Africa (SSA). This is then used to draw a separate line of best fit for the respective countries.

world$SSA <- ifelse(world$region=="Sub-Saharan Africa", 1, 0)
world$SSA <- as.factor(world$SSA)

ggplot(world, aes(womyear, log(gdppc), col=SSA)) +
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold")) +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("Figure 2-9: No Relationship in Sub-Saharan Africa") +
  ylab("GDP per Capita (logged)") +
  xlab("Year Women Gained Suffrage") +
  scale_color_manual(values=c("grey", "#0000bf")) +
  theme(legend.position = "none") +
  annotate("text", x = 1928, y = 6.8,
           label = "Sub-Saharan Africa", col="#0000bf")

##################################################
# Reproduce Tables 2-1 and 2-2, which demonstrate the "instability" of a regression coefficient. Also put the two sets of coefficients side-by-side in the same table.
##################################################

##### Table 2-1

mod1 <- lm(log(gdppc)~womyear, data=world) # bivariate linear regression of log GDP (y or DV) on year women got the vote (x or IV)

stargazer(mod1, header=FALSE, 
          title = "Table 2-1: Estimates for a Bivariate Model",
          type = "html", out = "../output/table3.htm") # Note how I added the file path for the "out" parameter.

##### Table 2-2

mod2 <- lm(log(gdppc)~womyear + ethfrac + durable + pwthc,
           data=world)

stargazer(mod2, header=FALSE,
          title = "Table 2-2: Estimates Are Unstable",
          type = "html", out = "../output/table4.htm")

##### Santucci bonus track: put coefficients into same table. 

stargazer(mod1, mod2, out='../output/table_3-4_combined.htm')

##################################################
# Regression diagnostics. May pick up on Thursday.
##################################################

##### Figure 2-13. Plot predicted values against IV of interest. Santucci note: at first glance, we have a bad model. Why? Its *predictions* are uncorrelated with the IV of interest (independent variable, thing that is supposed to help us predict). How can we improve the model? Remove "womyear." How can we do even better? Read about the causes of economic growth/development, then build a model accordingly.

visreg(mod2, "womyear", ylab="Predicted Values GDP (logged)",
       xlab="Human capital",
       main="Figure 2-13: Women’s Suffrage Has No Effect",
       band=FALSE)

##### Figure 2-14. Plot residuals against predicted/fitted values.

### NOTE: Brown (2021) re-estimates mod2 with "na.action=na.exclude." Why? If you try to do the next two steps (extract residuals and predictions fromthe *original* mod2, storing them to the same data frame), R will return an error. This is because the residuals and predictions have a different length than the number of observations in the data set. Why? The lm() command for the original mod2 deleted observations with 'missing data' before it estimated the model. So, to solve this problem, we re-estimate mod2 with the "na.action=na.exclude" argument included.

mod2 <- lm(log(gdppc)~womyear + ethfrac + durable + pwthc, na.action=na.exclude,
           data=world)

world$res <- resid(mod2) # extracts vector of residuals (one for each observation in the data set) from the model object; stores it as "res" in the "world" data frame
world$pred <- predict(mod2) # extracts vector of predicted values (one for each observation in the data set) from the model object; stores it as "res" in the "world" data frame

ggplot(world, aes(pred, res)) +
  geom_point(col="#bf0000") +
  geom_text_repel(size = 2.8,
                  aes(label=ifelse(iso3c=="SGP" |
                                     iso3c=="TJK" |
                                     iso3c=="ZWE" |
                                     iso3c=="USA" |
                                     iso3c=="LUX",
                                   as.character(iso3c),''),
                      hjust = 0, vjust=-1), show.legend=FALSE) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8, face = "bold")) +
  xlab("Predicted or 'Fitted' Values") +
  ylab("Residuals") +
  ggtitle("Figure 2-14: Some Possible Outliers")