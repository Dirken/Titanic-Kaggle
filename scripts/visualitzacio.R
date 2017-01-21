library(MASS) 
library(cclust)
library(stringr)
library(cluster)
library(class)

#### LECTURA FITXERS #####

setwd("/home/dirken/Downloads/APA/titanic2/titanic/csvs/")

test <- read.csv("parsed/test_clean.csv", stringsAsFactors = FALSE)
train <- read.csv("parsed/train_clean.csv", stringsAsFactors = FALSE)

train.data <- train[, c("Sex", "AgeGroup", "Fare", "Pclass", "Embarked", "FamilySize")]
Survived.class <- factor(paste(train$Survived, sep=""))

#### PRIMERA APROXIMACIO A LES DADES ####

## mostra el index de supervivencia respecte la classe on s'allotjava cada passatger.
table(train$Survived, train$Sex)
prop.table(table(train$Survived, train$Sex),2)

# table(train$Survived, train$Sex, train$Pclass)
# prop.table(table(train$Survived, train$Sex, train$Pclass),2)


## indica la classe on estava cada passatger respecte el port on va embarcar
## es veu una interessant relacio, ja que a Cherbourg, la meitat de passatgers
## que van embarcar van ser de 1ra classe, o que en Queenstown (Irlanda del Sud), el 93% de 
## passatgers era de 3ra classe.
prop.table(table(train$Pclass, train$Embarked),2)

##### DADES A MATRIU #####
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

(lda.model <- lda (Target ~ ., data, prior = seq(1,1,length.out=K)/K))
plot(lda.model, col = as.numeric((data$Target)))

#### TRAINING - LDA ####

data.lda.cv <- lda(Target ~ ., data, prior = seq(1,1,length.out=K)/K, CV=TRUE) 
summary(data.lda.cv$class)

tab <- table(data$Target, data.lda.cv$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

#### TESTING - LDA  ####

data.lda <- lda(Target ~ ., data, prior = seq(1,1,length.out=K)/K)

pred <- predict(data.lda, data)$class
t_true <- data[,1]

table(pred,t_true)

#### PREDICTION ERROR - LDA ####

(sum(pred != t_true)/length(t_true))*100
