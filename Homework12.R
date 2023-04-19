library(ggplot2)
library(ggthemes)
library(patchwork)
library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(viridis)
# access a dataset available in a package
dataset<-iris
anova(lm(Petal.Length ~ Species, data = iris))
#p-value stat significant! 
#Let's try some graphs 
boxplot(Petal.Length ~ Species, data = iris, main = "Iris data",
        ylab = "Length of Petals", col = "black",
        notch = FALSE, varwidth = FALSE)
anova(lm(weight ~ group, data = PlantGrowth))
#Changing some features of the boxplots 
boxplot(Petal.Length ~ Species, data = iris, main = "Iris data",
        ylab = "Length of Petals", col = "blue",
        notch = TRUE, varwidth =TRUE)
anova(lm(weight ~ group, data = PlantGrowth))
#Making more decent one with ggplot
ggplot(data = iris) +
  aes(x = Species, y = Petal.Length, color = Species) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2))
# Now a scatterplot
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
  geom_point(size=6) +
  theme_ipsum()
#Now I'll try to do a density plot (used this as resource https://gexijin.github.io/learnR/visualizing-the-iris-flower-data-set.html) but this time, I'm doing that with Petal.Width and adding the labels 
ggplot(data = iris) +
  aes(x = Petal.Width, fill = Species) + xlab("The Petal Width") + ylab("Density")+
  geom_density(alpha = 0.3) 
#I want to do a violin plot now 
myviolin<-ggplot(data=iris) + 
  geom_violin(aes(x = Species, y = Petal.Length, color = Species, fill=Species), alpha = 0.35)
print(myviolin)
#Last plot! This time I want to see the data distribution with the variable Sepal Length
ggplot(data = iris) + 
  geom_jitter(aes(x = Sepal.Length, y = Species, color=Species),
              width=0.05, height=0.15, alpha=0.7)
