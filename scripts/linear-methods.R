###############################################
## Linear Quadratics Methods
###############################################

#setwd("/home/dirken/Downloads/APA/titanic2/titanic/csvs/")
setwd("D:/Usuarios/alex2132/Escritorio/Titanic-Kaggle-master/Titanic-Kaggle-master/csvs")

test <- read.csv("parsed/test_clean.csv", stringsAsFactors = TRUE)
trainning <- read.csv("parsed/train_clean.csv", stringsAsFactors = TRUE)


###############################################
## QDA
###############################################
library(MASS)
# train QDA MODEL

qda_model <- train(as.factor(Survived) ~ Pclass + Sex + AgeGroup + Fare + Embarked + FamilySize ,
                   data=trainning, method="qda")

test$Survived_pred <- predict(qda_model, test)
(tab <- table(test$Survived_pred, test$Survived))
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

confusionMatrix(test$Survived, predict(qda_model, test))


###############################################
## Naive Bayes
###############################################
library(e1071)

naive_model <- naiveBayes(as.factor(Survived) ~ Pclass + Sex + AgeGroup + Fare + Embarked + FamilySize ,
                    data=trainning)

test$Survived_pred <- predict(naive_model, test)
(tab <- table(test$Survived_pred, test$Survived))
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

confusionMatrix(test$Survived, predict(naive_model, test))


###############################################
## Logistic regression
###############################################


logit <- glm(as.factor(Survived) ~ Pclass + Sex + AgeGroup + Fare + Embarked + Title + FamilySize, data = trainning,
             family = binomial(link = "logit"))

summary(logit)


test$Survived_pred <- predict(logit, test, type = "response")
test$Survived_pred[test$Survived_pred >= 0.5] <- 1
test$Survived_pred[test$Survived_pred < 0.5] <- 0

(tab <- table(test$Survived_pred, test$Survived))
1 - sum(tab[row(tab)==col(tab)])/sum(tab)


