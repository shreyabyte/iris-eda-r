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

#Sepal.Length is roughly normally distributed with a slight right skew
hist(iris$Sepal.Length,
     main= "Histogram of Sepal Length",
     xlab= "Sepal Length",
     col= "lightblue")

#clear separation between species by petal length
boxplot(Petal.Length ~ Species, data = iris,
        main = "Petal Length by Species",
        xlab = "Species",
        ylab = "Petal Length",
        col = c("pink","lightgreen","lightyellow"))

#partial overlap between species wrt sepals
plot(iris$Sepal.Length,iris$Sepal.Width,
     col=as.integer(iris$Species),
     pch=19,
     xlab="Sepal Length",
     ylab="Sepal Width",
     main = "Sepal Length vs Sepal Width (colored by Species)")
legend("topright", legend=levels(iris$Species),pch=19,col=1:3)

#shows strong linear correlation between petal length and petal width
pairs(iris[1:4],
      main = "Pairs plot of iris measurements",
      pch = 21,
      bg=c("red","purple","green")[unclass(iris$Species)])

#correlation matrix
cor(iris[,1:4])

#one way anova test
aov_res<-aov(Sepal.Length~Species,data=iris)
summary(aov_res)

flower_index<-function(sepal_len, petal_len){
  return (sepal_len+petal_len)
}
print(flower_index(iris$Sepal.Length[1:6],iris$Petal.Length[1:6]))

petal_size_cat<-function(petal_len){
  ifelse(petal_len<2,"small",
         ifelse(petal_len<5,"medium","large"))
}
print(table(petal_size_cat(iris$Petal.Length)))