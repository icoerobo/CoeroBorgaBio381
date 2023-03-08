#I will use Rebecca Dunlop Data (2019) from Dryad: https://datadryad.org/stash/dataset/doi:10.5061%2Fdryad.65j00m3 

library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation
# quick and dirty, a truncated normal distribution to work on the solution set
z <- read.table("MyDataFile.csv",header=TRUE,sep=";")
str(z)
summary(z)
z <- z[z$NLp>0,]
str(z)
summary(z$NLp)
#Plot a histogram of the data
p1 <- ggplot(data=z, aes(x=NLp, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2)
print(p1)
#Now modify the code to add in a kernel density plot of the data
p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)
#Next, fit a normal distribution to your data and grab the maximum likelihood estimators of the two parameters of the normal, the mean and the variance
normPars <- fitdistr(z$NLp,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute
#probability density for the distribution
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]
xval <- seq(0,max(z$NLp),len=length(z$NLp))
stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$NLp), args = list(mean = meanML, sd = sdML))
p1 + stat
#now plot exponential probability density 
expoPars <- fitdistr(z$NLp,"exponential")
rateML <- expoPars$estimate["rate"]
stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$NLp), args = list(rate=rateML))
p1 + stat + stat2
#plot uniform probability density 
stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$NLp), args = list(min=min(z$NLp), max=max(z$NLp)))
p1 + stat + stat2 + stat3
#plot gamma probability density 
gammaPars <- fitdistr(z$NLp,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]
stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$NLp), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4
#beta probability density 
pSpecial <- ggplot(data=z, aes(x=NLp/(max(NLp + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")
betaPars <- fitdistr(x=z$NLp/max(z$NLp + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]
statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$NLp), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial
#So what's the best fitting model? The normal and gamma ones are fitting either way 

#Now Basing on my parameters (Mean=124, sd=6.19) from the normal distribution, I will generate another dataset generated from my computer 

#-------------------------------------------------
# estimating parameters from data
# maximum likelihood estimator theta versus P(data|theta)

# use fitdistr function, feeding it data and a distribution type)
x <- rnorm(3245,mean=124,sd=6.19)
qplot(x,color=I("black"),fill=I("goldenrod"))
fitdistr(x,"normal")
z <- rnorm(n=3245,mean=124)
z <- data.frame(1:3245,z)
names(z) <- list("ID","myVar")
z <- z[z$myVar>0,]
str(z)
summary(z$myVar)
p1 <- ggplot(data=z, aes(x=myVar, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2)
print(p1)
p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)
normPars <- fitdistr(z$myVar,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(120,max(z$myVar),len=length(z$myVar))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$myVar), args = list(mean = meanML, sd = sdML))
p1 + stat
#it fits the data and the model is doing a good job of simulating realistic data 