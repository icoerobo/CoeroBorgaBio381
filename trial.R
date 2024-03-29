#first exercise

x<-1.1
a<-2.2
b<-3.3
z<-(x^(a^b))
print(z)
z<-((x^a)^b)
print(z)
z<-(3*x^3+2*x^2+1)
print(z)

#second exercise 

z<-c(seq(1,8),seq(7,1))
print(z)
#or also
z<-rep(1:8,2)
print(z)

y<-rep(1:5,c(1,2,3,4,5))#telling it to repeat for all these times using concatenate function
print(y)

s<-rep(5:1,c(1,2,3,4,5))
print(s)
# same here 

# Third exercise 
x<-runif(2,min=0,max=10)
set.seed(0)
print(x)
r<-((x[1]^2+x[2]^2)^1/2)
print(r)
q<-atan(x[2]/x[1])
print(q)
#polar coordinates (r,q) --> (43.72804,0.2878715)

#Fourth exercise 
queue <- c("sheep", "fox", "owl", "ant")
queue<-append(queue, "serpent")
print(queue)
queue<-c(queue[2:5]) #to remove the first animal that entered the boat
print(queue)
queue<-c("donkey",queue)
print(queue)
#now we have also a donkey on the boat 
queue<-c(queue[1:4])
print(queue)
#no more serpent
queue<-c(queue[1],queue[2],queue[4])
print(queue)
#no more owl
queue<-c(queue[1:2],"aphid","ant") #adding aphid in 3rd position
print(queue)
which(queue == "aphid")
#to know where aphid is 

#Fifth exercise 

x<-seq(1:100)
vec<-which(! x%%2== 0 & ! x%%3== 0 & ! x%%7==0)
#to be able to have a list of numbers from 1 to 100 NOT divisible for 2,3 or 7 
print(vec)

