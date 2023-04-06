
#Exercise 1
z <- c(10,0,1,3,0,2,4)
counter<-0
for (i in 1:length(z)) {{
  if(z[i]==0){
  counter<-counter+1}
}

#Exercise 2 
counter<-sum(z==0)
print(counter)

#Exercise 3 
###############################
#FUNCTION: matrixfun
#packages: none 
#purpose: Write a function that takes as input two integers representing the number of rows and columns in a matrix. The output is a matrix of these dimensions in which each element is the product of the row number x the column number
#input: number of rows and columns
#output:matrix output
#------------------------------

matrixfun<-function(x=2,y=3)
  {
  m1<-matrix(nrow=x,ncol=y)
  
  for (i in 1:nrow(m1)){
    for(j in 1:ncol(m1)){
      m1[i,j]<-(i*j)
    }
  }
  return(m1)
       }
matrixfun()


#Exercise 4 

group <- c(rep("Control",10),rep("Treatment1",10),rep("Treatment2",10))
var <- c(rnorm(n=10, mean=5,sd=0.25), rnorm(n=10, mean=15,sd=0.75),rnorm(n=10, mean=12,sd=0.85))
df <- data.frame(trt=group,res=var)
print(df)
#created my dataframe 

shuffle_data <- function(z) {
    z[,2] <- sample(z[,2]) 
   v1 <- mean(z[1:10,2])
   v2<- mean(z[11:20,2])
   v3<- mean(z[21:30,2])
   vector<-c(v1,v2,v3)
  }

b<-shuffle_data(df)
print(b)

#do that again for 100 times 
shuffle_data <- function(z) {
  for (i in 1:100){
    
    
  
}