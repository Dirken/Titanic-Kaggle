library(plyr)
library(foreign)


setwd("C:/Users/Ricard/Downloads/titanic/titanic/csvs")

train <- read.csv("originals/train.csv", stringsAsFactors = FALSE)
test <- read.csv("originals/test.csv", stringsAsFactors = FALSE)


train$Sex <- factor(train$Sex)
train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)



Age.mod.train <- lm(Age ~ Pclass + Sex + SibSp + Parch + Fare, data = train)
Fare.mod.train <- lm(Fare ~ Pclass + Sex + SibSp + Parch + Age, data = train)

Age.mod.test <- lm(Age ~ Pclass + Sex + SibSp + Parch + Fare, data = test)
Fare.mod.test <- lm(Fare ~ Pclass + Sex + SibSp + Parch + Age, data = test)

#Afegim les prediccions de edat i passatge a les variables que no en tenen:
train$Age[is.na(train$Age)] <- predict(Age.mod.train, train)[is.na(train$Age)]
train$Fare[is.na(train$Fare)] <- predict(Fare.mod.train, train)[is.na(train$Fare)]

test$Age[is.na(test$Age)] <- predict(Age.mod.test, test)[is.na(test$Age)]
test$Fare[is.na(test$Fare)] <- predict(Fare.mod.test, test)[is.na(test$Fare)]

#comptem el nombre d aparicions de cada lletra
length(which(train$Embarked == "S"))
length(which(train$Embarked == "C"))
length(which(train$Embarked == "Q"))

#afegim la S ja que es la mes comuna
train$Embarked[train$Embarked == ""] <- "S"
train$Embarked <- factor(train$Embarked)

#length(which(train$Age <= 0))
#train$Age[train$Age < 0] <- predict(Age.mod.train, train)

write.csv(test, "parsed/test_clean.csv", row.names = FALSE)
write.csv(train, "parsed/train_clean.csv", row.names = FALSE)


