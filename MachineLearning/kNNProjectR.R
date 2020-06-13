# Data from: the iris flower dataset

# The main purpose of this script is to build a simple kNN model to cluster
# the three different species based on the petal and sepal length and width of
# each observation.

library(ISLR)
head(iris)

# SCALE THE DATA

stand.features <- scale(iris[1:4])
#var(stand.features[,1])

final.data <- cbind(stand.features, iris[5])

# TRAIN TEST SPLIT

set.seed(101)
library(caTools)

sample <- sample.split(final.data$Species, SplitRatio = 0.7)

train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)

# KNN MODEL

library(class)

predicted.species <- knn(train[1:4], test[1:4], train$Species, k=1)

mean(test$Species != predicted.species)

# CHOSING A K VALUE

# INITIALIZE EMPTY VECTORS
predicted.species <- NULL 
error.rate <- NULL

# LOOP THROUGH DIFFERENT K VALUES

for (i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4], test[1:4], train$Species, k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}

# PLOT THE ERROR RATE VS K

library(ggplot2)

k.values <- 1:10
error.df <- data.frame(error.rate, k.values)

pl <- ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(lty='dotted', color='red')

print(pl)

# The plot show that k values from 3 to 5 present the lowest error rate.

# END