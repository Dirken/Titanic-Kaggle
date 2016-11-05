library(cclust)
library(MASS)
library(stringr)
library(Rmixmod)
setwd("C:/Users/Ricard/Downloads/titanic/titanic/csvs")

train <- read.csv("parsed/train_clean.csv", stringsAsFactors = FALSE)
test <- read.csv("parsed/test_clean.csv", stringsAsFactors = FALSE)

#train <- train[, c("PassengerId","Survived","Sex","Age","SibSp","Parch","Fare")]
#train <- train[, c("PassengerId","Survived","Age","SibSp","Parch","Fare")]
#train <- train[, c("PassengerId","Survived","SibSp","Parch")]
#train <- train[, c("PassengerId","Survived","Fare")]


train <- scale(train)

dataC = matrix(ncol=length(train[1,]), nrow=length(train[,1])) 
for (i in seq(1, length(train[1,]), by=1)) {
  dataC[,i] = train[,i] 
}

K <- 10
kmeans <- cclust (dataC,K,method="kmeans",dist="euclidean")


data <- cbind(kmeans$cluster,dataC)
data <- as.data.frame(data)
names(data)[1]<-paste("Target")
data$Target <- factor(data$Target)

data.lda <- lda(Target ~ ., data, prior = seq(1,1,length.out=K)/K)
data.lda
plot(data.lda, col=as.numeric(data$Target))



