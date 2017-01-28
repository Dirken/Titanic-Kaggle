# Authors: Ricard Meyerhofer, Alejandro Martinez.
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

                    
setwd("D:/Usuarios/alex2132/Escritorio/Titanic-Kaggle-master/Titanic-Kaggle-master/csvs")

combi <- read.delim2("originals/titanic.txt",
                  header = TRUE,
                  sep = "\t",
                  dec = ",",
                  fill = TRUE,
                  strip.white = TRUE,
                  na.strings = "EMPTY")



#######################################################################
# Data Analysis                                                       #                                                                                                    
#######################################################################
# To do a good prediction, we need to have a good data analysis where we
# clean our data and see which variables will be more or less useful.
# Also we have to think about how we will treat data with missing values,
# how they interact with each other, their relevance in the problem etc


# We will use a package that allows us to see missings # but first we 
# need to substitute "" for NA

combi[combi == ""] <- NA
require(Amelia)
missmap(combi, main="Titanic Training Data - Missings Values", 
        col=c("blue", "white"), legend=TRUE)

#So far we can see that we have missings at 3 variables:
# - Cabin
# - Age
# - Embarked
# As we can see, cabin has a lot of misses, age has some and embarked has few few ones.
# So it seems hard to have any prediction from cabin but so far, we will not eliminate it.

# Now we proceed to see relationship between variables that we have (
# so at this way we will understand better our data)
barplot(table(combi$Survived), names.arg = c("Perished", "Survived"),
        main="Passenger Fate distribution", col="blue")

barplot(table(combi$Pclass),  main="Pclass distribution", col="Blue")

barplot(table(combi$Sex), main="Sex distribution", col="blue")

barplot(table(combi$SibSp), main="SibSp distribution",col="blue")

barplot(table(combi$Parch), main="Parch distribution", col="blue")

barplot(table(combi$Embarked), names.arg = c("NA","Cherbourg", "Queenstown", "Southampton"),
        main="Embarked place", col="blue")

hist(combi$Age, main="Age distribution", col="blue")

hist(combi$Fare, main="Fare price", col="blue")

# as we could see in the histogram, we have some fare values 
# to 0 which should be treated and there are some prices that are extremely expensive
# that could be ones that have been bought in groups
summary(combi$Fare)

# So as we can see there were way more males than females, more people perished than died
# and there is in 3rd class as many people as in first and second. Also more people embarked in 
# Southampton more than anywhere else and the average age of our training set is about 20-40.

#Age is a determinant factor? Seems not to be in adults
boxplot(combi$Age ~ combi$Survived, main="Passenger Fate by Age", 
        xlab="Sex", ylab="Survived")

barplot(table(combi$Survived,combi$Age), main="Passenger Fate by Age", 
        xlab="Sex", ylab="Survived",legend=TRUE)

#Embarking place is a determinant factor?
table(combi$Survived,combi$Embarked)
barplot(table(combi$Survived,combi$Embarked), main="Passenger Fate by Embarking place", 
        xlab="Embarked", ylab="Survived")

#Fare is determinant factor? We have to do more analysis
boxplot( combi$Fare~ combi$Survived , main="Passenger Fate by Fare price", 
         xlab="Survived", ylab="Fare price", legend=TRUE)

#Relationship of all variables
require(corrgram)
corrgram.data <- combi
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

# Convert to a string
combi$Name <- as.character(combi$Name)

# If we see the variable Name we mainly have the following structure
# FamilyName, Title. Name Surname. So we can get useful information:
combi$Title <- gsub('(.*, )|(\\..*)', '', combi$Name)

combi$Family <- sub('\\s*,.*','', combi$Name)

# if we pay attention to combi$Title, we can see that can help us to get 
# the age of some of the missings. So if we take a look to the honorifics 
# we can try to fullfil (better than just doing the average of all our data) 
# the NA values that we have.
table(combi$Title)

#all titles we have
unique(combi$Title)

#Titles that have missings:
unique(combi$Title[is.na(combi$Age)])

#We decided to organize them taking into account wikipedias article about english honorifics:
# https://en.wikipedia.org/wiki/English_honorifics and some googling.

#Mr
length(which(combi$Title == "Mr"))

#Miss
length(which(combi$Title == "Mrs"))
length(which(combi$Title == "Miss"))
length(which(combi$Title == "Ms"))
length(which(combi$Title == "Mlle")) #mademoiselle
length(which(combi$Title == "Mme")) #madame

# Nobelty, not all of them are but it's difficult to distinguish between a Reve
# and a Col than a certain real nobel.. (it's variable, would )
length(which(combi$Title == "Master"))

length(which(combi$Title == "Dr"))
length(which(combi$Title == "Rev"))
length(which(combi$Title == "Don"))
length(which(combi$Title == "Sir"))
length(which(combi$Title == "Capt"))
length(which(combi$Title == "Jonkheer")) #dutch nobelty
length(which(combi$Title == "Col"))
length(which(combi$Title == "Major"))

length(which(combi$Title == "Lady"))
length(which(combi$Title == "the Countess"))




#Global mean of everyone
summary(combi$Survived)


#Transformations of same classes
combi$Title[combi$Title == "Mrs"] <- "Miss"
combi$Title[combi$Title == "Ms"] <- "Miss"
combi$Title[combi$Title == "Mlle"] <- "Miss"
combi$Title[combi$Title == "Mme"] <- "Miss"

combi$Title[combi$Title == "Dr"] <- "Nobelty"
combi$Title[combi$Title == "Rev"] <- "Nobelty"
combi$Title[combi$Title == "Don"] <- "Nobelty"
combi$Title[combi$Title == "Dona"] <- "Nobelty"
combi$Title[combi$Title == "Sir"] <- "Nobelty"
combi$Title[combi$Title == "Capt"] <- "Nobelty"
combi$Title[combi$Title == "Jonkheer"] <- "Nobelty"
combi$Title[combi$Title == "Col"] <- "Nobelty"
combi$Title[combi$Title == "Major"] <- "Nobelty"
combi$Title[combi$Title == "Lady"] <- "Nobelty"
combi$Title[combi$Title == "the Countess"] <- "Nobelty"

combi$Title <- factor(combi$Title)
combi$Sex <- factor(combi$Sex)
combi$Embarked <- factor(combi$Embarked)


#So it's not a good idea to mix them Master with our Nobelty group.

#We create a new variable family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1


#######################################################################
# Imputations                                                         #                                                                                                    
#######################################################################

#Let's now imputate the missing ages of each group!
unique(combi$Title[is.na(combi$Age)])

Age.mod <- lm(Age ~ Pclass + Sex + SibSp + Parch + Fare + Title , data = combi)
Fare.mod <- lm(Fare ~ Pclass + Sex + SibSp + Parch + Age +Title, data = combi)


#Afegim les prediccions de edat i passatge a les variables que no en tenen:
combi$Age[is.na(combi$Age)] <- predict(Age.mod, combi)[is.na(combi$Age)]
#fare we should make a bit more of research about it...
combi$Fare[is.na(combi$Fare)] <- predict(Fare.mod, combi)[is.na(combi$Fare)]


# missing the embarkation is quite sure that that person will be from Southampton (by probability)
prop.table(table(combi$Pclass, combi$Embarked),2)
combi$Pclass[is.na(combi$Embarked)] 
#but our passengers are first class so... we put a C
combi$Embarked[which(is.na(combi$Embarked))] <- 'C'

#We group by age, best group is with 7 clusters
k <- kmeans(combi$Age, 7)
combi$AgeGroup <- k$cluster
#######################################################################
# Write                                                               #                                                                                                    
#######################################################################

train <- combi[1:891,]
test <- combi[892:1309,]

# We write the results of the preprocessing in test_clean.csv and 
# train_clean.csv, this csv's will be used in the following sections
# where we will try to visualize and predict the data (script.r).

train <- train[, c("Survived", "Pclass", "Sex","Fare", "AgeGroup","Embarked","FamilySize","Title")]
test <- test[, c("Survived","Pclass","Sex","Fare", "AgeGroup","Embarked","FamilySize","Title")]
write.csv(test, "parsed/test_clean.csv", row.names = FALSE)
write.csv(train, "parsed/train_clean.csv", row.names = FALSE)


