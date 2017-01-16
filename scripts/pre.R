#######################################################################
# Libraries                                                           #                                                                                                    
#######################################################################
library(plyr)
library(foreign)
library(ggplot2)

#######################################################################
# Read                                                                #                                                                                                    
#######################################################################
# We read the data. Data is split in two, a train model that we're going
# to use for analysis the data and a test one where we will try how our
# predictions work. Data is split at a proportion of 2/3, 1/3.

setwd("/home/dirken/Downloads/APA/titanic2/titanic/csvs/")
train <- read.csv("originals/train.csv", stringsAsFactors = FALSE)
test <- read.csv("originals/test.csv", stringsAsFactors = FALSE)

#######################################################################
# Data Analysis                                                       #                                                                                                    
#######################################################################
# To do a good prediction, we need to have a good data analysis where we
# clean our data and see which variables will be more or less useful.
# Also we have to think about how we will treat data with missing values.


#train$Sex <- factor(train$Sex)
#train$Survived <- factor(train$Survived)
#train$Pclass <- factor(train$Pclass)

full <- join(test, train, type = "full")

test$Survived <- 0

Age.mod <- lm(Age ~ Pclass + Sex + SibSp + Parch + Fare, data = full)
Fare.mod <- lm(Fare ~ Pclass + Sex + SibSp + Parch + Age, data = full)

#Afegim les prediccions de edat i passatge a les variables que no en tenen:
train$Age[is.na(train$Age)] <- predict(Age.mod, train)[is.na(train$Age)]
train$Fare[is.na(train$Fare)] <- predict(Fare.mod, train)[is.na(train$Fare)]

test$Age[is.na(test$Age)] <- predict(Age.mod, test)[is.na(test$Age)]
test$Fare[is.na(test$Fare)] <- predict(Fare.mod, test)[is.na(test$Fare)]

#comptem el nombre d aparicions de cada lletra
length(which(full$Embarked == "S"))
length(which(full$Embarked == "C"))
length(which(full$Embarked == "Q"))

#afegim la S ja que es la mes comuna
train$Embarked[train$Embarked == ""] <- "S"

train$Embarked[train$Embarked == "C"] <- 0
train$Embarked[train$Embarked == "S"] <- 1
train$Embarked[train$Embarked == "Q"] <- 2

test$Embarked[test$Embarked == "C"] <- 0
test$Embarked[test$Embarked == "S"] <- 1
test$Embarked[test$Embarked == "Q"] <- 2


#survived yes/no
# train$Survived[train$Survived == 0] <- "N"
# train$Survived[train$Survived == 1] <- "Y"

#male female
train$Sex[train$Sex == "male"] <- 0
train$Sex[train$Sex == "female"] <- 1

# creem la variable family size per agrupar els SibSp i Parch
train$FamilySize <- train$SibSp + train$Parch + 1
test$FamilySize <- test$SibSp + test$Parch + 1

train$Age[train$Age < 0] <- 0
test$Age[test$Age < 0] <- 0


## agrupem per edats, el millor valor que hem trobat es 7 clusters
k <- kmeans(train$Age, 7)
train$AgeGroup <- k$cluster

k <- kmeans(test$Age, 7)
test$AgeGroup <- k$cluster



#######################################################################
# Write                                                               #                                                                                                    
#######################################################################
# We write the results of the preprocessing in test_clean.csv and 
# train_clean.csv, this csv's will be used in the following sections
# where we will try to visualize and predict the data (script.r).

train <- train[, c("Survived", "Pclass", "Name","Sex","Fare","AgeGroup","Embarked","FamilySize")]
test <- test[, c("Survived", "Pclass", "Name","Sex","Fare","AgeGroup","Embarked","FamilySize")]
write.csv(test, "parsed/test_clean.csv", row.names = FALSE)
write.csv(train, "parsed/train_clean.csv", row.names = FALSE)


