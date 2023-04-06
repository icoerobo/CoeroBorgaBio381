#packages required 
library(ggplot2) # for graphics
library(MASS)
library(dplyr)
library(rlang)
library(tidyverse)


#source 
source("FunctionsHomework9.R")

#global variables
z<-PlantGrowth

#Pull mean and sd from datasets
#dataframe<-data.frame(Mean=c(c[1],x[1], y[1]), sd=c(c[2], x[2],y[2]), stringsAsFactors=FALSE)
#print(dataframe)
df<-gstats(z)

#create a fake dataset
s<-data.frame(value=fakedataset(df$mean[1], df$mean[2], df$mean[3],df$sd[1], df$sd[2], df$sd[3]), Group=rep(c("NewControl","NewTreatm1","NewTreatm2"),each=10))

#Do summary statistics and ANOVA 
aov<-anova(s)


#Plot as a boxplot
creatingplot(s)

#Plot 2 as an additional modification 
creatingplot2(s)

