#load dataset
data(iris)

#data overview
head(iris)
summary(iris)


#data cleaning 
sum(is.na(iris))
sum(duplicated(iris))
iris_clean<-iris[!duplicated(iris) & complete.cases(iris),]

#statistical summaries
sapply(iris[,1:4],mean)
sapply(iris[,1:4],sd)
aggregate(.~Species,data=iris,FUN=mean)

hist(iris$Sepal.Length,
     main= "Histogram of Sepal Length",
     xlab= "Sepal Length",
     col= "lightblue")

boxplot(Petal.Length ~ Species, data = iris,
        main = "Petal Length by Species",
        xlab = "Species",
        ylab = "Petal Length",
        col = c("pink","lightgreen","lightyellow"))

plot(iris$Sepal.Length,iris$Sepal.Width,
     col=as.integer(iris$Species),
     pch=19,
     xlab="Sepal Length",
     ylab="Sepal Width",
     main = "Sepal Length vs Sepal Width (colored by Species)")
legend("topright", legend=levels(iris$Species),pch=19,col=1:3)

pairs(iris[1:4],
      main = "Pairs plot of iris measurements",
      pch = 21,
      bg=c("red","purple","green")[unclass(iris$Species)])

cor(iris[,1:4])
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