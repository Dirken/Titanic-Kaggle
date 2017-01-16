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


###############################################
## nn
###############################################
neighbours <- c(1:6)
errors<- matrix (nrow=length(neighbours), ncol=2)
colnames(errors) <- c("k","LOOCV error")

length(train)
length(test)
for (k in neighbours)
{
  myknn.cv <- knn.cv (train, test, k = factor(train[,6]))
  #errors[k, "k"] <- neighbours[k]
  #tab <- table(myknn.cv, test)
  #errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
}

errors
###############################################
##Logistic regression
###############################################
test <- read.csv("parsed/test_clean.csv", stringsAsFactors = TRUE)
train <- read.csv("parsed/train_clean.csv", stringsAsFactors = TRUE)
#de moment no tenim en compte el sexe
train <- subset(train,select=c(1,2,6,7,8))
test <-  subset(test,select=c(1,2,6,7,8))
test$Sex <-  unfactor(train$Sex)
# Model fitting
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

# Analysis of deviance
anova(model,test="Chisq")

# McFadden R^2
library(pscl)
pR2(model)

#-------------------------------------------------------------------------------
# MEASURING THE PREDICTIVE ABILITY OF THE MODEL

# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

