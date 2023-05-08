###############################
#FUNCTION: getmean()
#packages: none 
#purpose 
#input:
#output:
#------------------------------

gstats<-function(z){
  gstats<-z%>%
  group_by(group)%>%
  summarize(mean=mean(weight), sd=sd(weight))
  return(gstats)
  #mutate(mean=mean(weight), sd=sd(weight))
}
  
fakedataset<-function(m1, m2, m3 , s1, s2, s3){
  NewControl <-rnorm(n=10, mean=m1,sd=s1)
  NewTreatm1 <-rnorm(n=10, mean=m2,sd=s2)
  NewTreatm2 <-rnorm(n=10, mean=m3,sd=s3)
  return(c(NewControl,NewTreatm1,NewTreatm2))
}
###############################
#FUNCTION: myplot
#packages: none 
#purpose: create a plot
#input:my fake dataframe 
#output:boxplot
#------------------------------
creatingplot<-function(s){
  myplot<-ggplot(data=s, aes(x=Group, y=value))+geom_boxplot()
  return(myplot)
  }

###############################
#Additional change for the last part of the homework!
#A scatterplot instead of a boxplot 
creatingplot2<-function(s){
  myplot<-ggplot(data=s)+geom_violin(aes(x=Group, y=value, color=Group, fill=Group), alpha=0.35)+xlab("Group")+ylab("Value")
  return(myplot)
}

###############################
#FUNCTION: anova
#packages: none 
#purpose : my homework 
#input: my fake dataset 
#output: p values from ANOVA
#------------------------------
anova<-function(s){
  anova<-aov(value~Group, data=s)
return(anova)
  }

