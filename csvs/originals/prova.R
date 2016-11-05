library(plyr)
library(foreign)

# Load the data sets
setwd("/Users/alex-mac/Documents/uni/3ro 2n cuatri/apa/practica/titanic/csvs")

train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

###
### Data structures
###

# Create a Survived variable in the test data set
# Set "0" (did not survive) as the default value
test$Survived <- 0

# Convert catagorical variables to factors
train$Survived <- factor(train$Survived)
train$Sex <- factor(train$Sex)
train$Pclass <- factor(train$Pclass)
test$Survived <- factor(test$Survived)
test$Sex <- factor(test$Sex)
test$Pclass <- factor(test$Pclass)
test$Embarked <- factor(test$Embarked)

###
### Fixing missing values
###
# 177 missing ages in TRAIN
# 86 missing ages in TEST
# 1 missing Fare in TEST
# 2 missing Embarked in TRAIN

# Combine the data sets for Age/Fare modeling
full <- join(test, train, type = "full")

# Multiple Imputation
#library(mi)
#inf <- mi.info(train)
#imp <- mi(train, info = inf, check.coef.convergence = FALSE, n.imp = 2, n.iter = 6, seed = 111)
#plot(imp)

# Create LM models for predicting missing values in AGE and FARE
Age.mod <- lm(Age ~ Pclass + Sex +
                SibSp + Parch + Fare, data = full)
Fare.mod<- lm(Fare ~ Pclass + Sex +
                SibSp + Parch + Age, data = full)

# Replace missing values in AGE and FARE with prediction
train$Age[is.na(train$Age)] <- predict(Age.mod, train)[is.na(train$Age)]
test$Age[is.na(test$Age)] <- predict(Age.mod, test)[is.na(test$Age)]
test$Fare[is.na(test$Fare)] <- predict(Fare.mod, test)[is.na(test$Fare)]
