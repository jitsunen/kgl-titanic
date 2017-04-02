library(dplyr)

preProcess <- function(data)
{
  # fill in missing values for age
  data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)
  
  # set missing fares to mean of fare grouped by Pclass
  #   find mean fare per class
  farePerClass <- data %>%
    group_by(Pclass) %>%
    select(Fare,Pclass) %>%
    summarise(FareMean = mean(Fare, na.rm = TRUE))
  
  # set missing fares to the mean fare of its class
  data$Fare[data$Pclass == 1] <- farePerClass$FareMean[farePerClass$Pclass == 1]
  data$Fare[data$Pclass == 2] <- farePerClass$FareMean[farePerClass$Pclass == 2]
  data$Fare[data$Pclass == 3] <- farePerClass$FareMean[farePerClass$Pclass == 3]
  
  # set embarked to most common station 'S'
  data$Embarked[data$Embarked == ''] <- as.factor('S')
  
  data$Embarked <- droplevels(data$Embarked)
  
  # Family Size feature
  data$FamilySize <- data$SibSp + data$Parch + 1

  # Family Id feature
  data$Surname <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
  data$FamilyID <- paste(as.character(data$FamilySize), data$Surname, sep="")
  data$FamilyID[data$FamilySize <= 2] <- 'Small'
  
  data
}