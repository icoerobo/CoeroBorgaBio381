library(tidyverse)
library(dplyr)
data(iris)
class(iris) 
head(iris)
iris1<-filter(iris, Species==c("virginica", "versicolor")&Sepal.Length>6&Sepal.Width>2.5)
#28 observations now
iris2<-select(iris1,c(Species,Sepal.Length,Sepal.Width))
iris3<-arrange(iris2,by=desc(Sepal.Length))
head(iris3)
#adding a column more 
iris4<-mutate(iris3,Sepal.Area=Sepal.Length*Sepal.Width)
print(iris4)

iris5<-summarize(iris4,meanSepal.Length=mean(Sepal.Length, na.rm=TRUE),meanSepal.Width=mean(Sepal.Width, na.rm=TRUE),TotalNumber=n())
print(iris5)

irisSpecies<-group_by(iris4,Species)
iris6<-summarize(irisSpecies, meanSepal.Length=mean(Sepal.Length, na.rm=TRUE),meanSepal.Width=mean(Sepal.Width, na.rm=TRUE),Sample=n())
print(iris6)

#Using %>%, N. 8
irisfinal<-iris %>%
filter(Species==c("virginica","versicolor"),
Sepal.Length>6,Sepal.Width>2.5)%>% select(Species, Sepal.Width, Sepal.Length) %>% 
arrange(desc(Sepal.Length)) %>% 
mutate(Sepal.Area=Sepal.Length*Sepal.Width) %>% 
group_by(Species) %>% 
summarise(meanSepal.Length=mean(Sepal.Length),
meanSepal.Width=mean(Sepal.Width),
Sample=n())
print(irisfinal)
colnames(irisfinal)            
#last exercise N. 9
iris_longer <- iris %>% 
pivot_longer(cols=c(Sepal.Length:Sepal.Width,
Petal.Length:Petal.Width),
names_to = "Measure",
values_to = "Value")
print(iris_longer)
head(iris_longer)
