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

#Our train dataset contains the following:
train2 <- train
summary(train2)

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

hist(train$Fare, main="Fare price", col="blue")

# as we could see in the histogram, we have some fare values 
# to 0 which should be treated and there are some prices that are extremely expensive
# that could be ones that have been bought in groups
summary(train$Fare)

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

#Relationship of all variables
require(corrgram)
corrgram.data <- train
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age","SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Training Data")

#######################################################################
# Cleaning data && new variable s                                     #                                                                                                    
#######################################################################

# If we see the variable Name we mainly have the following structure
# FamilyName, Title. Name Surname. So we can get useful information:
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
test$Title <- gsub('(.*, )|(\\..*)', '', test$Name)

train$Family <- sub('\\s*,.*','', train$Name)
test$Family <- sub('\\s*,.*','', test$Name)

# if we pay attention to train$Title, we can see that can help us to get 
# the age of some of the missings. So if we take a look to the honorifics 
# we can try to fullfil (better than just doing the average of all our data) 
# the NA values that we have.
table(train$Title)

#all titles we have
unique(train$Title)

#Titles that have missings:
unique(train$Title[is.na(train$Age)])

#We decided to organize them taking into account wikipedias article about english honorifics:
# https://en.wikipedia.org/wiki/English_honorifics and some googling.

#Mr
length(which(train$Title == "Mr"))

#Miss
length(which(train$Title == "Mrs"))
length(which(train$Title == "Miss"))
length(which(train$Title == "Ms"))
length(which(train$Title == "Mlle")) #mademoiselle
length(which(train$Title == "Mme")) #madame

# Nobelty, not all of them are but it's difficult to distinguish between a Reve
# and a Col than a certain real nobel.. (it's variable, would )
length(which(train$Title == "Master"))

length(which(train$Title == "Dr"))
length(which(train$Title == "Rev"))
length(which(train$Title == "Don"))
length(which(train$Title == "Sir"))
length(which(train$Title == "Capt"))
length(which(train$Title == "Jonkheer")) #dutch nobelty
length(which(train$Title == "Col"))
length(which(train$Title == "Major"))

length(which(train$Title == "Lady"))
length(which(train$Title == "the Countess"))

#let's see if it can be worth to consider Master - Adult in nobelty:
train2$Title <- factor(train$Title, c("Capt","Col","Major","Sir","Lady","Rev",
                                     "Dr","Don","Jonkheer","the Countess","Master"))

# it can be seen that the average age except in master and capt could be the same for all groups 
# (we have few data...), so it's hard to really know.
boxplot(train$Age ~ train2$Title, main="Passenger Age by Title", xlab="Title", ylab="Age")

#Global mean of everyone
summary(train$Survived)

#Mean of Master
trainaux <- (train$Survived[train$Title == "Master"])
summary(trainaux)

#Transformations of same classes
train$Title[train$Title == "Mrs"] <- "Miss"
train$Title[train$Title == "Ms"] <- "Miss"
train$Title[train$Title == "Mlle"] <- "Miss"
train$Title[train$Title == "Mme"] <- "Miss"
#We do also on test
test$Title[test$Title == "Mrs"] <- "Miss"
test$Title[test$Title == "Ms"] <- "Miss"
test$Title[test$Title == "Mlle"] <- "Miss"
test$Title[test$Title == "Mme"] <- "Miss"

train$Title[train$Title == "Dr"] <- "Nobelty"
train$Title[train$Title == "Rev"] <- "Nobelty"
train$Title[train$Title == "Don"] <- "Nobelty"
train$Title[train$Title == "Dona"] <- "Nobelty"
train$Title[train$Title == "Sir"] <- "Nobelty"
train$Title[train$Title == "Capt"] <- "Nobelty"
train$Title[train$Title == "Jonkheer"] <- "Nobelty"
train$Title[train$Title == "Col"] <- "Nobelty"
train$Title[train$Title == "Major"] <- "Nobelty"
train$Title[train$Title == "Lady"] <- "Nobelty"
train$Title[train$Title == "the Countess"] <- "Nobelty"
#Also on test
test$Title[test$Title == "Dr"] <- "Nobelty"
test$Title[test$Title == "Rev"] <- "Nobelty"
test$Title[test$Title == "Don"] <- "Nobelty"
test$Title[test$Title == "Dona"] <- "Nobelty"
test$Title[test$Title == "Sir"] <- "Nobelty"
test$Title[test$Title == "Capt"] <- "Nobelty"
test$Title[test$Title == "Jonkheer"] <- "Nobelty"
test$Title[test$Title == "Col"] <- "Nobelty"
test$Title[test$Title == "Major"] <- "Nobelty"
test$Title[test$Title == "Lady"] <- "Nobelty"
test$Title[test$Title == "the Countess"] <- "Nobelty"


trainaux <- (train$Survived[train$Title == "Nobelty"])
summary(trainaux)
#So it's not a good idea to mix them Master with our Nobelty group.

#We create a new variable family size
train$FamilySize <- train$SibSp + train$Parch + 1
test$FamilySize <- test$SibSp + test$Parch + 1


#######################################################################
# Imputations                                                         #                                                                                                    
#######################################################################

#Let's now imputate the missing ages of each group!
unique(train$Title[is.na(train$Age)])

Age.mod <- lm(Age ~ Pclass + Sex + SibSp + Parch + Fare + Title , data = train)
Fare.mod <- lm(Fare ~ Pclass + Sex + SibSp + Parch + Age +Title, data = train)


#Afegim les prediccions de edat i passatge a les variables que no en tenen:
train$Age[is.na(train$Age)] <- predict(Age.mod, train)[is.na(train$Age)]
#fare we should make a bit more of research about it...
train$Fare[is.na(train$Fare)] <- predict(Fare.mod, train)[is.na(train$Fare)]
test$Age[is.na(test$Age)] <- predict(Age.mod, test)[is.na(test$Age)]
test$Fare[is.na(test$Fare)] <- predict(Fare.mod, test)[is.na(test$Fare)]


# missing the embarkation is quite sure that that person will be from Southampton (by probability)
train$Embarked[which(is.na(train$Embarked))] <- 'S'

#We group by age, best group is with 7 clusters
k <- kmeans(train$Age, 7)
train$AgeGroup <- k$cluster

k <- kmeans(test$Age, 7)
test$AgeGroup <- k$cluster
#######################################################################
# Write                                                               #                                                                                                    
#######################################################################

# Before writing it, let's compare it with the one we had at first 
summary(train2)
summary(train)

# We write the results of the preprocessing in test_clean.csv and 
# train_clean.csv, this csv's will be used in the following sections
# where we will try to visualize and predict the data (script.r).

train <- train[, c("Survived", "Pclass", "Name","Sex","Fare","AgeGroup","Embarked","FamilySize","Title","FamilySize")]
test <- test[, c("Survived", "Pclass", "Name","Sex","Fare","AgeGroup","Embarked","FamilySize","Title","FamilySize")]
write.csv(test, "parsed/test_clean.csv", row.names = FALSE)
write.csv(train, "parsed/train_clean.csv", row.names = FALSE)


