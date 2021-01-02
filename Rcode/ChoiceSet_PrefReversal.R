##  All options are on the table? Manuscript  ##
##  Rotem Dvir  ##
##  June 2020  ##


# Packages
library(foreign)
library(ggplot2)
library(gplots)
library(ggthemes)
library(plyr)
library(devtools)
library(dplyr)
library(ggpubr)

# Set Randomizer
set.seed(2020)

##########################################################################################
###  Preference reversal plot: values based on ANOVA repeated measure models (SPSS output)
##########################################################################################

# Create datasets 
dat.set <- data.frame(d.set=c("Sets A&B", "Sets C&D"), TwoOptionSet=c(0.436, 0.931), ThreeOptionSet=c(0.352, 0.463))
dat.set2 <- melt(dat.set, id.vars = 'd.set')

# Create plot
rev.plot <- ggplot(dat.set2, aes(d.set, value)) + 
  geom_bar(aes(fill = variable), width = 0.5, position = position_dodge(width=0.5), stat="identity") +  
  xlab("") +
  ylab("Proportion Support for Policy 1") +
  ggtitle("Preference Reversal") +
  theme(legend.position="bottom", legend.title = 
          element_blank(),axis.title.x=element_text(colour = "black", size = 10, face = "bold"), 
        axis.title.y=element_text(colour = "black", size = 10, face = "bold"),
        legend.background = element_rect(size = 0.5, linetype = "solid", colour = "darkblue"),
        panel.background = element_rect(fill = "white",
                                        colour = "grey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', lineend = "butt",
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', lineend = "butt",
                                        colour = "grey"),
        plot.title = element_text(size = 18, face = "bold.italic")) 

rev.plot + scale_fill_brewer(palette="Set1") 


