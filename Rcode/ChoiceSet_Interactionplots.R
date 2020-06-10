##  All options are on the table? Manuscript  ##
##  Rotem Dvir  ##
##  June 2020  ##

# Clear memory
rm(list=ls())

# Set Working Directory
setwd("~/Dropbox/TAMU/Extra_Projects/Git_edits/Choice_set")

# Packages
library(foreign)
library(ggplot2)
library(gplots)
library(MASS)
library(Hmisc)
library(ggthemes)
library(plyr)
library(devtools)
library(dplyr)
library(ggpubr)

# Set Randomizer
set.seed(2020)

# Upload master data file
MyData <- read.csv("ChoiceSetData_June2019.csv", header=TRUE, sep = ",", strip.white = T, na.strings = "")

## Create reduced sample (no baseline conditions)
MyData$cas <- NA
MyData$cas[MyData$casualties==1] <- 0
MyData$cas[MyData$casualties==2] <- 1

MyData$oth <- NA
MyData$oth[MyData$other==1] <- 0
MyData$oth[MyData$other==2] <- 1

# Set conditions IV's as factorial

MyData$horizon<-as.factor(MyData$horizon)
MyData$casualties<-as.factor(MyData$casualties)
MyData$other<-as.factor(MyData$other)

MyData2$horizon<-as.factor(MyData2$horizon)
MyData2$casualties<-as.factor(MyData2$casualties)
MyData2$other<-as.factor(MyData2$other)

MyData$cas<-as.factor(MyData$cas)
MyData$oth<-as.factor(MyData$oth)

# Setting the contrasts for the sum of squares
options(contrasts=c("contr.helmert","contr.poly"))

#checking if the above command worked properly
options()

#########################################################
### Reduced sample: Interaction plots for probit models
#########################################################

## Probit interaction models
## The probability of accepting policy 1 & 2 into the choice-set

summary(m.red1 <- glm(P1 ~ horizon + cas + oth + horizon * oth +
                        Gender + Age + party + Edu_cat + FP_Know, data = MyData, family=binomial(link="probit")))

summary(m.red2 <- glm(P2 ~ horizon + cas + oth + horizon * oth +
                        Gender + Age + party + Edu_cat + FP_Know, data = MyData, family=binomial(link="probit")))

## Plot interaction policy 1
library(FSA)

inter.data1 = Summarize(P1 ~ horizon + oth, data = MyData)
  inter.data1$se = inter.data1$sd / sqrt(inter.data1$n)
  inter.data1$se = signif(inter.data1$se, digits=3)
  pd = position_dodge(.2)

inter.plot1 <- ggplot(inter.data1, aes(x = oth, y = mean, color = horizon)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, size=0.7, position=position_dodge(0.2)) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +
  xlab("Reciprocal Outcomes") + ylab("Probability of accepting P1") +
  ggtitle("Accepting policy 1 (80% chances of success)")

inter.plot1 + theme_pubr() + labs(color = "Time Horizon") + 
  theme(plot.title = element_text(size=16, face="bold.italic")) +
  scale_color_discrete(breaks=c("0", "1"),
                      labels=c("ST", "LT")) +
  scale_x_discrete(labels = c("Positive", "Negative"))

## Plot interaction policy 2

inter.data2 = Summarize(P2 ~ horizon + oth, data = MyData)
inter.data2$se = inter.data2$sd / sqrt(inter.data2$n)
inter.data2$se = signif(inter.data2$se, digits=3)

inter.plot2 <- ggplot(inter.data2, aes(x = oth, y = mean, color = horizon)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, size=0.7, position=position_dodge(0.2)) +
  geom_point(size=2, position=position_dodge(0.2)) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +
  ylim(0.3, 1) +
  xlab("Reciprocal Outcomes") + ylab("Probability of accepting P2") +
  ggtitle("Accepting policy 2 (75% chances of success)")

inter.plot2 + theme_pubr() + labs(color = "Time Horizon") + 
  theme(plot.title = element_text(size=16, face="bold.italic")) +
  scale_color_discrete(breaks=c("0", "1"),
                       labels=c("ST", "LT")) +
  scale_x_discrete(labels = c("Negative", "Positive"))

