###############################################
## Non-Linear Quadratics Methods
###############################################

#setwd("/home/dirken/Downloads/APA/titanic2/titanic/csvs/")
setwd("D:/Usuarios/alex2132/Escritorio/Titanic-Kaggle-master/Titanic-Kaggle-master/csvs")

test <- read.csv("parsed/test_clean.csv", stringsAsFactors = TRUE)
train <- read.csv("parsed/train_clean.csv", stringsAsFactors = TRUE)

# 
# 
# train$Embarked <- as.numeric(factor(train$Embarked))
# train$Sex <- as.numeric(factor(train$Sex))
# train$Title <- as.numeric(factor(train$Title))
# 
# test$Embarked <- as.numeric(factor(test$Embarked))
# test$Sex <- as.numeric(factor(test$Sex))
# test$Title <- as.numeric(factor(test$Title))
# 
# train <- scale(train)
# test <- scale(test)


#########################
##Neural Net
#########################
library(plyr)
library(nnet)
library(caret)

net3 <- nnet(as.factor(Survived) ~ Pclass + Sex + AgeGroup + Fare + Embarked + Title + FamilySize, size = 2,
             data = train, linout = FALSE, maxit = 10000)

# train$Survived_pred <- predict(net3, train, type = "class")
# (tab <- table(train$Survived_pred, train$Survived))
# 1 - sum(tab[row(tab)==col(tab)])/sum(tab)

test$Survived_pred <- predict(net3, test, type = "class")
(tab <- table(test$Survived_pred, test$Survived))
1 - sum(tab[row(tab)==col(tab)])/sum(tab)


confusionMatrix(test$Survived, predict(net3, test, type = "class"))


###############################################
## SVM
###############################################
library(kernlab)

svm.model <- ksvm(as.factor(Survived) ~ Pclass + Sex + AgeGroup + Fare + Embarked + Title + FamilySize, data = train, type="C-svc", kernel="rbfdot", kpar=list(sigma=1), C=80, cross=2, scaled = c(TRUE))
train$Survived_pred <- predict(svm.model, train, type = "response")
test$Survived_pred <- predict(svm.model, test, type = "response")

# Make our prediction on the TRAIN data set [For calculating error]
# (tab <- table(train$Survived_pred, train$Survived))
# 1 - sum(tab[row(tab)==col(tab)])/sum(tab)


# Make our prediction on the TEST data set

test$Survived_pred <- predict(svm.model, test)
(tab <- table(test$Survived_pred, test$Survived))
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

confusionMatrix(test$Survived, predict(svm.model, test))


###############################################
## Random Forest
###############################################
library(randomForest)
set.seed(123)



library(TunePareto)
# Prepare a crossvalidation 10x10 method to get the best model with
# several classifiers
k <- 100
CV.folds <- generateCVRuns(train$Survived, ntimes=1, nfold=k, stratified=TRUE)
cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")
cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k
resp.var <- which(colnames(train)=='Survived')

train$Survived <- factor(train$Survived)
train$Sex <- as.numeric(train$Sex)
train$Embarked <- as.numeric(train$Embarked)
train$Title <- as.numeric(train$Title)


for (j in 1:k) {
  va <- unlist(CV.folds[[1]][[j]])
  tr <- train[-va,]
  rf <- randomForest(as.factor(Survived) ~ Pclass + Sex + AgeGroup + Fare + Embarked + Title + FamilySize, 
                     data = tr, xtest = train[va,-resp.var], ytest = train[va,resp.var], scaled = c(TRUE), 
                     importance=TRUE)
  cv.results[j,"TR error"] <- 1 - sum(diag(rf$confusion[,-11]) / sum(rf$confusion[,-11]))
  cv.results[j,"VA error"] <- 1 - sum(diag(rf$test$confusion[,-11]) / sum(rf$test$confusion[,-11]))
  cv.results[j,"fold"] <- j
}
cv.results
mean(cv.results[,'VA error'])

train <- read.csv("parsed/train_clean.csv", stringsAsFactors = TRUE)
# Create random forest based on PCLASS, SEX, FARE, and AGE
forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + AgeGroup + Fare + Embarked + Title + FamilySize,
                       data=train, importance=TRUE, ntree=15000, scaled = c(TRUE))

summary(forest)

# Extract the importance of each variable
importance(forest)


# Make our prediction on the TRAIN data set [For calculating error]
# train$Survived_pred <- predict(forest, train)
# (tab <- table(train$Survived_pred, train$Survived))
# 1 - sum(tab[row(tab)==col(tab)])/sum(tab)


# Make our prediction on the TEST data set
test$Survived_pred <- predict(forest, test)
(tab <- table(test$Survived_pred, test$Survived))
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

confusionMatrix(test$Survived, predict(forest, test))

