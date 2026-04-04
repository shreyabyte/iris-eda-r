#loading 
#load built in iris dataset (150 observations, 4 measurements, 3 species)
data(iris)

#Preview structure and distribution by checking shape, ranges, and class balance before analysis
head(iris)
summary(iris)


#data cleaning 
sum(is.na(iris))
sum(duplicated(iris))

#complete unique observations
iris_clean<-iris[!duplicated(iris) & complete.cases(iris),]

#statistical summaries
sapply(iris[,1:4],mean)
sapply(iris[,1:4],sd)

#hints at species differences
aggregate(.~Species,data=iris,FUN=mean)

#visualizations

#Histogram: Distribution of Sepal Length
#Observation: roughly normal distribution with slight right skew
hist(iris$Sepal.Length,
     main= "Histogram of Sepal Length",
     xlab= "Sepal Length",
     col= "lightblue")


#Boxplot: Petal Length across species
#Observation: clear separation between species
boxplot(Petal.Length ~ Species, data = iris,
        main = "Petal Length by Species",
        xlab = "Species",
        ylab = "Petal Length",
        col = c("pink","lightgreen","lightyellow"))


#Scatter Plot: Sepal Length vs Sepal Width
#Observation: partial overlap between species
plot(iris$Sepal.Length, iris$Sepal.Width,
     col=as.integer(iris$Species),
     pch=19, #plotting character
     xlab="Sepal Length",
     ylab="Sepal Width",
     main = "Sepal Length vs Sepal Width (colored by Species)")
legend("topright", legend=levels(iris$Species), pch=19, col=1:3)


#Pair Plot: Relationship between all variables
#Observation: strong linear correlation between petal length and width
pairs(iris[1:4],
      main = "Pairs plot of iris measurements",
      pch = 21,
      bg=c("red","purple","green")[unclass(iris$Species)]) #bg(background)-both fill and border color, unclass allows to assign numbers to 'class of species'


#Correlation analysis
#correlation matrix
cor(iris[,1:4])

#Statistical Test
#one way anova(analysis of variance) test- f->between group variance vs within group variance
aov_res<-aov(Sepal.Length~Species,data=iris)
summary(aov_res)

#Custom Functions
#Flower Index= Sum of Sepal + Petal Length
flower_index<-function(sepal_len, petal_len){
  return (sepal_len+petal_len)
}
print(flower_index(iris$Sepal.Length[1:6], iris$Petal.Length[1:6]))

#Categorize Petal size
petal_size_cat<-function(petal_len){
  ifelse(petal_len<2,"small",
         ifelse(petal_len<5,"medium","large"))
}
print(table(petal_size_cat(iris$Petal.Length)))