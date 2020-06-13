# Data fromthe College dataset: https://cran.r-project.org/web/packages/ISLR/ISLR.pdf

# The main purpose of this script is to build a decision tree model as well as
# a random forest model to determine if a college is public or private.


library(ISLR)
library(ggplot2)

df <- College

ggplot(df,aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private))

ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50) + theme_bw()

ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50) + theme_bw()

subset(df, Grad.Rate > 100)

df['Cazenovia College','Grad.Rate'] <- 100

# TRAIN TEST SPLIT

library(caTools)

set.seed(101) 

sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

# DECISION TREE MODEL

library(rpart)
tree <- rpart(Private ~.,method='class',data = train)
tree.preds <- predict(tree,test)

head(tree.preds)

tree.preds <- as.data.frame(tree.preds)

joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}

tree.preds$Private <- sapply(tree.preds$Yes,joiner)
head(tree.preds)

table(tree.preds$Private,test$Private)

library(rpart.plot)
prp(tree)

# RANDOM FOREST

library(randomForest)

rf.model <- randomForest(Private ~ . , data = train,importance = TRUE)

rf.model$confusion

rf.model$importance

p <- predict(rf.model,test)

table(p,test$Private)

# END