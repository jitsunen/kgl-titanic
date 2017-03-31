library(ggplot2)

data <- read.csv("train.csv")
data$Survived = as.factor(data$Survived)

data$Age[is.na(data$Age)] <- 0

# plot class survived
ggplot(data = data, aes(x = Pclass, color = Survived)) + geom_histogram()
  
# plot sex survived
ggplot(data = data, aes(x = Sex, color = Survived)) + geom_histogram(stat = "count")

# plot siblings survived
ggplot(data = data, aes(x = SibSp, color = Survived)) + geom_histogram()

# plot parents survived
ggplot(data = data, aes(x = Parch, color = Survived)) + geom_histogram()

# plot fare survived
ggplot(data = data, aes(x = Fare, color = Survived)) + geom_histogram()

# plot station embarked survived
ggplot(data = data, aes(x = Embarked, color = Survived)) + geom_histogram(stat="count")

# train a model, consider Pclass, Sex, Age as features
library(randomForest)
fit <- randomForest(Survived ~ Pclass + Sex + Age, data = data)
print(fit)
importance(fit)

# test against test data set
testData <- read.csv("test.csv")
testData$Age[is.na(testData$Age)] <- 0

predicted <- predict(fit, newdata = testData)
testData$Predicted <- predicted
write.csv(testData[c('PassengerId', 'Predicted')], file="predicted.csv")
