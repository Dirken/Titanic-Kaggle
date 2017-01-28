library(MASS) 
library(cclust)
library(stringr)
library(cluster)
library(class)

#### LECTURA FITXERS #####

#setwd("/home/dirken/Downloads/APA/titanic2/titanic/csvs/")
setwd("D:/Usuarios/alex2132/Escritorio/Titanic-Kaggle-master/Titanic-Kaggle-master/csvs")


test <- read.csv("parsed/test_clean.csv", stringsAsFactors = TRUE)
train <- read.csv("parsed/train_clean.csv", stringsAsFactors = TRUE)

train.data <- train[, c("Sex", "AgeGroup", "Fare", "Pclass", "Embarked", "FamilySize", "Title")]
Survived.class <- factor(paste(train$Survived, sep=""))

##### DADES A MATRIU #####
train.data$Embarked <- as.numeric(factor(train.data$Embarked))
train.data$Sex <- as.numeric(factor(train.data$Sex))
train.data$Title <- as.numeric(factor(train.data$Title))

train.data <- scale(train.data)

dataC = matrix(ncol=length(train.data[1,]),nrow=length(train.data[,1])) 
for (i in seq(1,length(train.data[1,]), by=1)) {
  dataC[,i] = train.data[,i] 
}

##### CLUSTERING #####

K <- 10
kmeans <- cclust (dataC, K, iter.max=100, method="kmeans", dist="euclidean")
(CH <- clustIndex(kmeans,dataC, index="calinski"))

data <- cbind(kmeans$cluster,dataC)
data <- as.data.frame(data)
names(data)[1]<-paste("Target")
data$Target <- factor(data$Target)

##### VISUALITZAR DADES - LDA #####

(lda.model <- lda (Target ~ ., data))
plot(lda.model, col = as.numeric((data$Target)))

#### TRAINING - LDA ####

data.lda.cv <- lda(Target ~ ., data, CV=TRUE) 
summary(data.lda.cv$class)

tab <- table(data$Target, data.lda.cv$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

#### TESTING - LDA  ####

data.lda <- lda(Target ~ ., data)

pred <- predict(data.lda, data)$class
t_true <- data[,1]

table(pred,t_true)

#### PREDICTION ERROR - LDA ####

(sum(pred != t_true)/length(t_true))*100

