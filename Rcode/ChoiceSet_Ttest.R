##  All options are on the table? Manuscript  ##
##  Rotem Dvir  ##
##  June 2020  ##

# Clear memory
rm(list=ls())

# Set Working Directory
setwd("~/Dropbox/TAMU/Extra_Projects/Git_edits/Choice_set/Data")

# Packages
library(psych)  ###needed for describe()
library(car)    ###needed for Anova()
library(xtable)     ###needed for ANOVA tables
options(xtable.floating=FALSE)
options(xtable.timestampt="")
library(foreign)
library(ggplot2)
library(gplots)
library(ggthemes)
library(plyr)
library(devtools)
library(dotwhisker)
library(broom)
library(dplyr)
library(tidyverse)

# Set Randomizer
set.seed(2020)

# Upload reduced data file (only baseline conditions)
MyData2 <- read.csv("ChoiceSetData_Baseline.csv", header=TRUE, sep = ",", strip.white = T, na.strings = "")

## Create variables for choice-set size
MyData2$set.all <- MyData2$P1+MyData2$P2+MyData2$P3+MyData2$P4+MyData2$P5+MyData2$P6+MyData2$P7
MyData2$set.all[MyData2$set.all==0] <- NA
MyData2$set.all[MyData2$set.all==1] <- NA

## Set conditions IV's as factorial

MyData2$horizon<-as.factor(MyData2$horizon)
MyData2$casualties<-as.factor(MyData2$casualties)
MyData2$other<-as.factor(MyData2$other)

# Setting the contrasts for the sum of squares
options(contrasts=c("contr.helmert","contr.poly"))

#checking if the above command worked properly
options()

### T-test for baseline conditions only
### I compare the mean choice-set size 
### 2 groups, vary in short/long term chances of success for policy options

# ANOVA model of reduced sample
summary(m.aov <- aov(set.all ~ horizon + Gender + Age + party + Edu_cat + FP_Know, data = MyData2))

# Test for difference in mean size 
tst.m <- t.test(set.all ~ horizon, data = MyData2, var.equal = TRUE, alternative="less")
tst.m


## Visaualize differences in means for reduced sample
library(egg)
mean(MyData2$set.all, na.rm = T)

# Plot point distribution of responses
pts.plot1 <- ggplot(MyData2, aes(x = horizon, y = set.all)) +
  geom_jitter(width = 0.03) +
  geom_hline(yintercept = 4.1485, color = "grey", linetype = "dashed") +
  geom_text(aes(0.6,4.1,label = c("Mean"), vjust = -1)) +
  xlab("Factor: Time Horizon") +
  ylab("Choice Set size") 

pts.plot1 <- pts.plot1 + stat_summary(fun.y=mean, geom="point", size = 3, color="red") +
  theme_bw() +
  scale_x_discrete(breaks = c("0", "1"), labels = c("ST", "LT")) 

# Model 
model <- lm(set.all ~ horizon + Gender + Age + Edu_cat + FP_Know, data = MyData2)
t.dat <- tidy(model)

t.dat <- t.dat[-c(1), ]
t.dat <- t.dat %>%
  relabel_predictors(c('horizon1' = "Horizon"))

model.plot <- ggplot(t.dat, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error), data = t.dat, size = 0.7) +
  ylim(-0.25, 0.5) +
  ylab("Coefficient Value") +
  xlab("") +
  geom_hline(yintercept = 0, linetype = 2) 

model.plot <- model.plot + theme_bw() 

### Errorbar plot of means for the t-test analysis
test.dat <- data.frame(d1 = c("Long term", "Short term"),
                       dm1 = c(4.316, 3.98),
                       se1 = c(0.144, 0.144))

test.plot <- ggplot(test.dat, aes(x=d1, y=dm1)) +
  geom_bar(stat = "identity", width = 0.65, fill="lightgrey") +
  geom_errorbar(aes(ymin=dm1-se1, ymax=dm1+se1), colour="black", width = .15, size = 1) +
  #  geom_point(colour = "blue") +
  ylim(0,5) +
  ggtitle("") +
  xlab("Time Horizon") + ylab("Mean Set Size")

test.plot <- test.plot+theme_bw()

## all plots together
grid.arrange(arrangeGrob(test.plot, pts.plot1, ncol = 2), model.plot, nrow = 2)


