# Data from: https://archive.ics.uci.edu/ml/datasets/banknote+authentication

# The main purpose of this script is to build an artificial neural network model 
#to determine if bank notes are authentic or not.

df <- read.csv('CSV files for ML Projects/bank_note_data.csv')

library(caTools)
set.seed(101)
split <- sample.split(df$Class, SplitRatio = 0.7)

train <- subset(df, split = TRUE)
test <- subset(df, split = FALSE)

library(neuralnet)
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, data = train, 
                hidden = c(5,3), linear.output = FALSE)

predicted.nn.values <- compute(nn, test[1:4])

head(predicted.nn.values$net.result)

predictions <- sapply(predicted.nn.values$net.result, round)

head(predictions)

table(predictions, test$Class)

# COMPARING RESULTS TO A RANDOM FOREST

library(randomForest)

df$Class <- factor(df$Class)

library(caTools)
set.seed(101)
split <- sample.split(df$Class, SplitRatio = 0.7)

train <- subset(df, split = TRUE)
test <- subset(df, split = FALSE)

rf.model <- randomForest(Class ~ ., data = train)

rf.pred <- predict(rf.model, test)

table(rf.pred, test$Class)

# END