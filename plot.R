plot <- function(data) {
  # plot age survived
  ggplot(data = data, aes(x = Age, color = Survived)) + geom_histogram(binwidth = 1)
  
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
}