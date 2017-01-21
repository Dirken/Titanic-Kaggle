###############################################
## Linear Quadratics Methods
###############################################

setwd("/home/dirken/Downloads/APA/titanic2/titanic/csvs/")

test <- read.csv("parsed/test_clean.csv", stringsAsFactors = FALSE)
train <- read.csv("parsed/train_clean.csv", stringsAsFactors = FALSE)

###############################################
## Nearest Neightbours
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
## Logistic regression
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