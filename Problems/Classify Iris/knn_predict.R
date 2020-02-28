library(ggplot2)
library(class)
library(caTools)

# /* --------------------------------------------------------------------------------------- */
# /* Set working directory to this problem and read the csv file                             */
# /* --------------------------------------------------------------------------------------- */
setwd("/home/jardel/learning-ml/Problems/Classify Iris")
df <- read.csv("data/Iris.csv")

# /* --------------------------------------------------------------------------------------- */
# /* Split dataset into a training and a test set                                            */
# /* --------------------------------------------------------------------------------------- */
split <- sample.split(df, SplitRatio = 0.7)
train <- subset(df, split==TRUE)
test <- subset(df, split==FALSE)

# /* --------------------------------------------------------------------------------------- */
# /* Exploratory                                                                             */
# /* --------------------------------------------------------------------------------------- */

#Sepal Length vs Sepal Width
ggplot(train, aes(SepalLengthCm,SepalWidthCm, color=Species)) + geom_point()

#Sepal Length vs Petal Length
ggplot(train, aes(SepalLengthCm,PetalLengthCm, color=Species)) + geom_point()

#Sepal Length vs Petal Width
ggplot(train, aes(SepalLengthCm,PetalWidthCm, color=Species)) + geom_point()

#Sepal Width vs Petal Length
ggplot(train, aes(SepalWidthCm,PetalLengthCm, color=Species)) + geom_point()

#Sepal Width vs Petal width
ggplot(train, aes(SepalWidthCm,PetalWidthCm, color=Species)) + geom_point()

#Petal Length vs Petal Width
ggplot(train, aes(PetalLengthCm,PetalWidthCm, color=Species)) + geom_point()

# /* --------------------------------------------------------------------------------------- */
# /* KNN Model                                                                               */
# /* --------------------------------------------------------------------------------------- */

###### k=1
predicted.species <- knn(train[2:5],test[2:5],train$Species, k=1)
#misclassification error
m.error <- mean(predicted.species != test$Species)
m.error

###### choosing k value (1~10)
predicted.species <- NULL
m.error <- NULL
for (i in 1:10) {
  predicted.species <- knn(train[2:5], test[2:5], train$Species, k=i)
  m.error[i] <- mean(predicted.species != test$Species)
}

###### plotting misclassification errors with different k values
k.values <- 1:10
m.error <- data.frame(k.values,m.error)
ggplot(m.error,aes(k.values,m.error)) + geom_point() + geom_line()


#we can see in the plot that 3 is the best value
predicted.species <- knn(train[2:5], test[2:5], train$Species, k=3)
accuracy <- mean(predicted.species == test$Species)
accuracy
