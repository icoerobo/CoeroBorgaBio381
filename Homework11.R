library(tidyverse)
x<-read.csv("root_biomass.csv")

for(i in 1:3){
  y<-x%>%
    filter(layer==i)
  output<-paste0(i)
  write.csv(y,file=output,sep=",",row.names=FALSE)
}


x<-read.table("1", sep=",",header = TRUE)
y<-read.table("2",sep=",",header=TRUE)
z<-read.table("3",sep=",",header=TRUE)

###############################
#FUNCTION: reg_stats
#packages: none 
#purpose: fits linear model, extracts statistics
#input:2-column data frame (x and y)
#output:slope, p-value, and r2
#------------------------------
reg_stats <- function(d=NULL) {
  if(is.null(d)) {
    x_var <- d[,1]
    y_var <- d[,3]
    d <- data.frame(x_var,y_var)
  }
  . <- lm(data=d,d[,3]~d[,1])
  . <- summary(.)
  stats_list <- list(slope=.$coefficients[2,1],
                     p_val=.$coefficients[2,4],
                     r2=.$r.squared)
  return(stats_list)
  
}
reg_stats(x)
reg_stats(y)
reg_stats(z)
print(x)
# Create scatterplot 
creatingplot<-function(d=NULL){
  myplot<-ggplot(data=d, aes(x=d[,1], y=d[,3]))+geom_point()
  return(myplot)
}

creatingplot(x)
creatingplot(y)
creatingplot(z)

