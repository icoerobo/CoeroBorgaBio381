# Exercise 1 

n_dims<-(sample(3:10,1))
vector<-seq(1:n_dims^2)
print(vector)
sample(vector,n_dims^2)
m<-matrix(data=vector,ncol=n_dims)
print(m)
w<-t(m) #to transpose the matrix 
print(w)
sum1<-sum(m[1,])
sum2<-sum(m[n_dims,])
mean1<-mean(m[1,])
mean2<-mean(m[n_dims,])
print(mean1)
print(mean2)
print(sum1)
print(sum2)
e<-eigen(m)

#Computes eigenvalues and eigenvectors of numeric (double, integer, logical) or complex matrices.
#0i --> complex numbers that have an imaginary part and a real part 

print(e)
typeof(e$vectors)
typeof(e$values)

# Exercise 2
my_matrix<-matrix(data=runif(16), ncol=4)
print(my_matrix)
my_logical<-sample(1:100,100)<50
print(my_logical)
my_letters<-sample(LETTERS, 26)
print(my_letters)
list<-list(my_matrix[2,2],my_logical[2],my_letters[2])
typeof(list[[1]])
typeof(list[[2]])
typeof(list[[3]])
newlist<-c(my_matrix[2,2],my_logical[2],my_letters[2])
typeof(newlist[1:3])
#now they are all characters

# Exercise 3 
my_unis <- runif(26,min=0,max=10)
my_letters <- sample(LETTERS)
dFrame <- data.frame(my_unis,my_letters)
print(dFrame)
str(dFrame)
dFrame[sample(my_unis,4),1]<-NA
print(sample(my_unis,4))
print(dFrame)
which(!complete.cases(dFrame))
order(dFrame$my_letters)
#so we have a vector with all the position orders of the rows 
dFrame[order(dFrame$my_letters),]
mean(dFrame$my_unis, na.rm=TRUE)
