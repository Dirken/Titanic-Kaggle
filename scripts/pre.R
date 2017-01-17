# Authors: Ricard Meyerhofer, Alejandro Martínez.
# Subject: Machine Learning, Universitat Politècnica de Catalunya, 2016-2017Q2.
# Even we did a documentation, we think it's important to comment what
# we are doing in the code. So for clear conclusions document is probably
# what will be more useful but here it can be seen the process and the 
# decisions we took while doing this problem.

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

# Our dataset is the following:
# Variable Name | Description
# --------------|-------------
# Survived      | Survived (1) or died (0)
# Pclass        | Passenger's class
# Name          | Passenger's name
# Sex           | Passenger's sex
# Age           | Passenger's age
# SibSp         | Number of siblings/spouses aboard
# Parch         | Number of parents/children aboard
# Ticket        | Ticket number
# Fare          | Fare
# Cabin         | Cabin
# Embarked      | Port of embarkation

setwd("/home/dirken/Downloads/APA/titanic2/titanic/csvs/")
train <- read.csv("originals/train.csv", stringsAsFactors = TRUE)
test <- read.csv("originals/test.csv", stringsAsFactors = TRUE)

#######################################################################
# Data Analysis                                                       #                                                                                                    
#######################################################################
# To do a good prediction, we need to have a good data analysis where we
# clean our data and see which variables will be more or less useful.
# Also we have to think about how we will treat data with missing values,
# how they interact with each other, their relevance in the problem etc

# We will use a package that allows us to see missings # but first we 
# need to substitute "" for NA

train[train == ""] <- NA
require(Amelia)
missmap(train, main="Titanic Training Data - Missings Values", 
        col=c("blue", "white"), legend=TRUE)

#So far we can see that we have missings at 3 variables:
# - Cabin
# - Age
# - Embarked
# As we can see, cabin has a lot of misses, age has some and embarked has few few ones.
# So it seems hard to have any prediction from cabin but so far, we will not eliminate it.

# Now we proceed to see relationship between variables that we have (
# so at this way we will understand better our data)
barplot(table(train$Survived), names.arg = c("Perished", "Survived"),
     main="Passenger Fate distribution", col="blue")

barplot(table(train$Pclass),  main="Pclass distribution", col="Blue")

barplot(table(train$Sex), main="Sex distribution", col="blue")

barplot(table(train$SibSp), main="SibSp distribution",col="blue")

barplot(table(train$Parch), main="Parch distribution", col="blue")

barplot(table(train$Embarked), names.arg = c("NA","Cherbourg", "Queenstown", "Southampton"),
        main="Embarked place", col="blue")

hist(train$Age, main="Age distribution", col="blue")

hist(train$Fare, main="Fare (fee paid for ticket[s])", col="blue")

# So as we can see there were way more males than females, more people perished than died
# and there is in 3rd class as many people as in first and second. Also more people embarked in 
# Southampton more than anywhere else and the average age of our training set is about 20-40.

#Age is a determinant factor? Seems not to be in adults
boxplot(train$Age ~ train$Survived, main="Passenger Fate by Age", 
        xlab="Sex", ylab="Survived")

barplot(table(train$Survived,train$Age), main="Passenger Fate by Age", 
        xlab="Sex", ylab="Survived",legend=TRUE)

#Embarking place is a determinant factor?
table(train$Survived,train$Embarked)
barplot(table(train$Survived,train$Embarked), main="Passenger Fate by Embarking place", 
        xlab="Embarked", ylab="Survived")

#Fare is determinant factor? We have to do more analysis
boxplot( train$Fare~ train$Survived , main="Passenger Fate by Fare price", 
        xlab="Survived", ylab="Fare price", legend=TRUE)

#######################################################################
# Cleaning data                                                       #                                                                                                    
#######################################################################

# If we see the variable Name we mainly have the following structure
# FamilyName, Title. Name Surname. So we can get useful information:
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
train$Family <- sub('\\s*,.*','', train$Name)

# if we pay attention to train$Title, we can see that can help us to get 
# the age of some of the missings. So if we take a look to the honorifics 
# we can try to fullfil some of the NA values that we have.
table(train$Title)

#######################################################################
# Imputations                                                         #                                                                                                    
#######################################################################

# We decided to do the following:

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


