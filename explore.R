library(ggplot2)
library(party)

loadScripts <- function(){
  source("preProcess.R")
}

loadData <- function(){
  data <- read.csv("train.csv")
  data$Survived = as.factor(data$Survived)
  data
}

loadScripts();
data <- loadData();
data <- preProcess(data)

# train a model, consider Pclass, Sex, Age as features
library(randomForest)
fit <- randomForest(Survived ~ Pclass + Sex + Age + Fare + Parch + SibSp + Embarked + FamilySize, data = data)
print(fit)
importance(fit) 

# test against test data set
testData <- read.csv("test.csv")
testData <- preProcess(testData)

spredicted <- predict(fit, newdata = testData)
testData$Predicted <- predicted
write.csv(testData[c('PassengerId', 'Predicted')], 
          file="predicted-rf.csv", 
          col.names = c('PassengerId','Survived'), 
          row.names = FALSE)

# use a conditional forest prediction algo
set.seed(415)
fit <- cforest(Survived ~ Pclass + Sex + Age + Fare + Parch + SibSp + Embarked,
               data = data, controls=cforest_unbiased(ntree=2000, mtry=3)) 
print(fit)
importance(fit)

Prediction <- predict(fit, testData, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = testData$PassengerId, Survived = Prediction)
write.csv(submit, file = "prediction_cf.csv", row.names = FALSE)
