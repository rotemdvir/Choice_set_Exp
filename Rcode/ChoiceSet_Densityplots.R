##  All options are on the table? Manuscript  ##
##  Rotem Dvir  ##
##  June 2020  ##

# Packages
library(psych)  ###needed for describe()
library(foreign)
library(ggplot2)
library(gplots)
library(MASS)
library(Hmisc)
library(ggthemes)
library(plyr)
library(devtools)
library(dplyr)
library(reshape2)

# Set Randomizer
set.seed(2020)


# Upload master data file
MyData <- read.csv("ChoiceSetData_June2019.csv", header=TRUE, sep = ",", strip.white = T, na.strings = "")

# Upload reduced data file (only baseline conditions)
MyData2 <- read.csv("ChoiceSetData_Baseline.csv", header=TRUE, sep = ",", strip.white = T, na.strings = "")

## Create variables for choice-set size
MyData$set.all <- MyData$P1+MyData$P2+MyData$P3+MyData$P4+MyData$P5+MyData$P6+MyData$P7
MyData$set.all[MyData$set.all==0] <- NA

MyData2$set.all <- MyData2$P1+MyData2$P2+MyData2$P3+MyData2$P4+MyData2$P5+MyData2$P6+MyData2$P7
MyData2$set.all[MyData2$set.all==0] <- NA
MyData2$set.all[MyData2$set.all==1] <- NA

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

################################################################################
### Density plots for choice-set size means and main time horizon conditions 
################################################################################

# Full sample: Time horizon (short/long conditions)
B=2500
n =nrow(MyData)
newSamp <- with(MyData, MyData[!is.na(set.all), ])

c1a.sample <- with(newSamp, matrix(sample(set.all[horizon == 0], size = n[1]*B, replace = T), B, n[1]))
c1a.means <- apply(c1a.sample, 1, mean)
ggplot(data.frame(MeanChoice = c1a.means),aes(x=MeanChoice)) + ylim(0,10) + xlim(0,7) +
  geom_density(color="blue", fill="blue", alpha=0.7)

c1b.sample <- with(newSamp, matrix(sample(set.all[horizon == 1], size = n[1]*B, replace = T), B, n[1]))
c1b.means <- apply(c1b.sample, 1, mean)
ggplot(data.frame(MeanChoice = c1b.means),aes(x=MeanChoice)) + ylim(0,10) + xlim(0,7) +
  geom_density(color="red", fill="red", alpha=0.7)

df <- data.frame(x=c1a.means, y=c1b.means)
df.m <- melt(df)
names(df.m)[names(df.m)=="variable"] <- "Time_Horizon"
df.m$Time_Horizon <- as.character(df.m$Time_Horizon)
df.m$Time_Horizon[df.m$Time_Horizon=="x"] <- "ST"
df.m$Time_Horizon[df.m$Time_Horizon=="y"] <- "LT"
plot1 <- ggplot(df.m) + geom_freqpoly(aes(x=value, y=..density.., colour=Time_Horizon)) +
  labs(x='Means: Choice-Set Size') + theme_bw() 
plot1

pp <- ggplot(df.m, aes(x=value)) + geom_density(aes(group=Time_Horizon, colour=Time_Horizon, fill=Time_Horizon), alpha=0.8) +
  labs(x='Means: Choice-Set Size') + theme_bw() + ggtitle("Panel A: Full sample")
pp <- pp + theme(legend.position="bottom") + scale_fill_brewer(palette="Set1") 


# Reduce (baseline) sample: : Time horizon (short/long conditions)
n =nrow(MyData2)
newSamp2 <- with(MyData2, MyData2[!is.na(set.all), ])

c2a.sample <- with(newSamp2, matrix(sample(set.all[horizon == 0], size = n[1]*B, replace = T), B, n[1]))
c2a.means <- apply(c2a.sample, 1, mean)
ggplot(data.frame(MeanChoice = c2a.means),aes(x=MeanChoice)) + ylim(0,10) + xlim(0,7) +
  geom_density(color="purple", fill="purple", alpha=0.7)

c2b.sample <- with(newSamp2, matrix(sample(set.all[horizon == 1], size = n[1]*B, replace = T), B, n[1]))
c2b.means <- apply(c2b.sample, 1, mean)
ggplot(data.frame(MeanChoice = c2b.means),aes(x=MeanChoice)) + ylim(0,10) + xlim(0,7) +
  geom_density(color="red", fill="red", alpha=0.7)

df2 <- data.frame(x=c2a.means, y=c2b.means)
df.m2 <- melt(df2)
names(df.m2)[names(df.m2)=="variable"] <- "Time_Horizon"
df.m2$Time_Horizon <- as.character(df.m2$Time_Horizon)
df.m2$Time_Horizon[df.m2$Time_Horizon=="x"] <- "ST"
df.m2$Time_Horizon[df.m2$Time_Horizon=="y"] <- "LT"
plot2 <- ggplot(df.m2) + geom_freqpoly(aes(x=value, y=..density.., colour=Time_Horizon)) +
  labs(x='Means: Choice-Set Size') + theme_bw() 
plot2

pp2 <- ggplot(df.m2, aes(x=value)) + geom_density(aes(group=Time_Horizon, colour=Time_Horizon, fill=Time_Horizon), alpha=0.8) +
  labs(x='Means: Choice-Set Size') + theme_bw() + ggtitle("Panel B: Baseline") 
pp2 <- pp2 + scale_fill_brewer(palette="Set1") + theme(legend.position="bottom") 


library(ggpubr)
ggarrange(pp, pp2, common.legend = T, legend = "bottom",
          ncol = 2, nrow = 1)


