library(ggplot2) # for graphics
library(MASS)
library(dplyr)
library(rlang)
library(tidyverse)
# for maximum likelihood estimation
# quick and dirty, a truncated normal distribution to work on the solution set
z<-PlantGrowth
str(z$group)
summary(z)

#dividing the subset groups
ControlData<- filter(z, group=="ctrl") #10 obs of 2 variables
mean(ControlData$weight) #5.032
sd(ControlData$weight) #0.583

Treatm1<-filter(z,group=="trt1") #10 obs of 2 variables
mean(Treatm1$weight) #4.661
sd(Treatm1$weight) #0.794

Treatm2<-filter(z,group=="trt2") #10 obs of 2 variables
mean(Treatm2$weight) #5.526
sd(Treatm2$weight) #0.446

#now we generate a fake dataset based on my parameters taking for granted that we have a normal distribution 

NewControl <-rnorm(n=10, mean=5.032,sd=0.583)
NewTreatm1 <-rnorm(n=10, mean=4.661,sd=0.794)
NewTreatm2 <-rnorm(n=10, mean=5.526,sd=0.446)
dFrame <- data.frame(NewControl,NewTreatm1,NewTreatm2,stringsAsFactors=FALSE)
dFrame<-pivot_longer(dFrame, cols=c(NewControl,NewTreatm1,NewTreatm2), names_to="Treatment",values_to="weight")
print(dFrame)
str(dFrame)
#now we have our fake dataset and we want to do a graph with that and analyze some data 
ggplot(dFrame, aes(x=Treatment,y=weight))+geom_boxplot()
#this is a useful graph 
anova<-aov(weight~Treatment, data=dFrame)
summary(anova)
#my anova is significant but if I run multiple times it changes--> results are very variable! 
#1st ANOVA=0.0415
#2nd ANOVA=0.00112
#3rd ANOVA=0.000467
# So now I play a bit around my parameters to understand if and how my results change as well. Let's see what happens if I change the sample size, making it 100 instead of 10 
NewControl <-rnorm(n=100, mean=5.032,sd=0.583)
NewTreatm1 <-rnorm(n=100, mean=4.661,sd=0.794)
NewTreatm2 <-rnorm(n=100, mean=5.526,sd=0.446)
dFrame <- data.frame(NewControl,NewTreatm1,NewTreatm2,stringsAsFactors=FALSE)
dFrame<-pivot_longer(dFrame, cols=c(NewControl,NewTreatm1,NewTreatm2), names_to="Treatment",values_to="weight")
anova<-aov(weight~Treatment, data=dFrame)
summary(anova)
ggplot(dFrame, aes(x=Treatment,y=weight))+geom_boxplot()
#So now, my ANOVA gives me a p-value (<2e-16) that is very significant. I know that I should probably extend the sample size and collect more data to have a stronger effect to be shown! 
#But what about the mean? Changing the mean and leaving the sample size as before. I will low down the mean of the NewControl by 2 units
NewControl <-rnorm(n=10, mean=3.032,sd=0.583)
NewTreatm1 <-rnorm(n=10, mean=4.661,sd=0.794)
NewTreatm2 <-rnorm(n=10, mean=5.526,sd=0.446)
dFrame <- data.frame(NewControl,NewTreatm1,NewTreatm2,stringsAsFactors=FALSE)
dFrame<-pivot_longer(dFrame, cols=c(NewControl,NewTreatm1,NewTreatm2), names_to="Treatment",values_to="weight")
ggplot(dFrame, aes(x=Treatment,y=weight))+geom_boxplot()
anova<-aov(weight~Treatment, data=dFrame)
summary(anova)
#the p-value in this case is very significant as well! P-value=5.99e-08
#So, how small can the differences between the groups be (the “effect size”) for you to still detect a significant pattern (p < 0.05)? Adding 1.1 unit to the control group and lowering down the mean of the 2nd treatment group by one
NewControl <-rnorm(n=10, mean=4.132,sd=0.583)#+1.1
NewTreatm1 <-rnorm(n=10, mean=4.661,sd=0.794)
NewTreatm2 <-rnorm(n=10, mean=4.526,sd=0.446) #-1
dFrame <- data.frame(NewControl,NewTreatm1,NewTreatm2,stringsAsFactors=FALSE)
dFrame<-pivot_longer(dFrame, cols=c(NewControl,NewTreatm1,NewTreatm2), names_to="Treatment",values_to="weight")
ggplot(dFrame, aes(x=Treatment,y=weight))+geom_boxplot()
anova<-aov(weight~Treatment, data=dFrame)
summary(anova)
#1st ANOVA: p-value= 0.0141
#2nd ANOVA: p-value= 0.0884
#3rd ANOVA: p-value= 0.0227 

#Alternatively, for the effect sizes you originally hypothesized, what is the minimum sample size you would need in order to detect a statistically significant effect? Again, run the model a few times with the same parameter set to get a feeling for the effect of random variation in the data
#lowering down of 2 units the sample size can become not significant, for example 
NewControl <-rnorm(n=8, mean=5.032,sd=0.583)
NewTreatm1 <-rnorm(n=8, mean=4.661,sd=0.794)
NewTreatm2 <-rnorm(n=8, mean=5.526,sd=0.446)
dFrame <- data.frame(NewControl,NewTreatm1,NewTreatm2,stringsAsFactors=FALSE)
dFrame<-pivot_longer(dFrame, cols=c(NewControl,NewTreatm1,NewTreatm2), names_to="Treatment",values_to="weight")
anova<-aov(weight~Treatment, data=dFrame)
summary(anova)
#1st ANOVA: 0.0637 
#2nd ANOVA: 0.117
#3rANOVA: 0.0295 
