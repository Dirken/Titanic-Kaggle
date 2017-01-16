library(MASS) 
library(cclust)
library(stringr)

set.seed(4)

#### LECTURA FICHEROS #####

dataMar <- read.csv("data_Mar_64.txt",header = FALSE)
dataSha <- read.csv("data_Sha_64.txt",header = FALSE)
dataTex <- read.csv("data_Tex_64.txt",header = FALSE)

#### TRATAMIENTO COLUMNAS #####

dataMar <- dataMar[,-17]
dataTex <- dataTex[,-22]
dataTex <- dataTex[,-61]
dataMar <- dataMar[,-17]
dataTex <- dataTex[,-22]
dataTex <- dataTex[,-61]

##### TRATAMIENTO FILAS #####

listTmp <- rep(length(dataTex))

for (i in seq(2,length(dataTex), by=1)) {
  listTmp[i-1] = mean(dataTex[2:15,i])
}

dataTex <- rbind(dataTex,listTmp)

##### RENOMBRAR FILAS #####

dataMar$V1<-as.character(lapply(dataMar$V1, function(x) {
  pos<-str_locate(x,' ')
  if (!is.na(pos[1,1]))
    (substr(x,0,pos))
  else x
}))
dataTex$V1<-as.character(lapply(dataTex$V1, function(x) {
  pos<-str_locate(x,' ')
  if (!is.na(pos[1,1]))
    (substr(x,0,pos))
  else x
}))
dataSha$V1<-as.character(lapply(dataSha$V1, function(x) {
  pos<-str_locate(x,' ')
  if (!is.na(pos[1,1]))
    (substr(x,0,pos))
  else x
}))
##### TRATAMIENTO MAGNITUD VALORES #####

dataTmp <- scale(dataTex[,-1])
dataTex <- cbind(as.factor(dataTex[,1]),dataTmp)

dataTmp <- scale(dataSha[,-1])
dataSha <- cbind(as.factor(dataSha[,1]),dataTmp)

dataTmp <- scale(dataMar[,-1])
dataMar <- cbind(as.factor(dataMar[,1]),dataTmp)

##### RENOMBRAR COLUMNAS #####

dataTex <- as.data.frame(dataTex)
dataSha <- as.data.frame(dataSha)
dataMar <- as.data.frame(dataMar)

names(dataTex)[1]<-paste("Target")
names(dataSha)[1]<-paste("Target")
names(dataMar)[1]<-paste("Target")

names(dataTex)[2:length(dataTex[1,])] <- paste("V", 2:length(dataTex[1,]), sep = "")
names(dataSha)[2:length(dataSha[1,])] <- paste("V", 2:length(dataSha[1,]), sep = "")
names(dataMar)[2:length(dataMar[1,])] <- paste("V", 2:length(dataMar[1,]), sep = "")

dataTex$Target<-as.factor(dataTex$Target)
dataSha$Target<-as.factor(dataSha$Target)
dataMar$Target<-as.factor(dataMar$Target)

##### REDUCCION VARIABLES #####

library(FSelector)
library(CORElearn)
library(ipred)

subsetTex.CFS <- cfs (Target~., dataTex)
dataTex.CFS <- cbind(dataTex[1],dataTex[subsetTex.CFS])

subsetSha.CFS <- cfs (Target~., dataSha)
dataSha.CFS <- cbind(dataSha[1],dataSha[subsetSha.CFS])

subsetMar.CFS <- cfs (Target~., dataMar)
dataMar.CFS <- cbind(dataMar[1],dataMar[subsetMar.CFS])

# Correlation filter + Random Forest (20 variables)

weights.randomForest <- random.forest.importance (Target~., dataTex.CFS, importance.type = 1)
dataTex.CRF <- cbind(dataTex[1],dataTex[cutoff.k(weights.randomForest, 20)])

weights.randomForest <- random.forest.importance (Target~., dataSha.CFS, importance.type = 1)
dataSha.CRF <- cbind(dataSha[1],dataSha[cutoff.k(weights.randomForest, 20)])

weights.randomForest <- random.forest.importance (Target~., dataMar.CFS, importance.type = 1)
dataMar.CRF <- cbind(dataMar[1],dataMar[cutoff.k(weights.randomForest, 20)])

##### TESTEO DE LAS VARIABLES #####
data.CRF <- cbind(dataTex.CRF[,1])

K <- 10
TIMES <- 10

#->INI

dataTest <- data.CRF

#If dataTest == data.CRF Ejecutar:
#names(data.CRF)[1]<-paste("Target")
#data.CRF$Target <- as.factor(data.CRF$Target)

mycontrol.10 <- control.errorest (k = K, strat = TRUE, random = FALSE, predictions = TRUE)
mypredict <- function(object, newdata)
  predict(object, newdata = newdata)$class

evaluator.accuracy <- function (subset) 
{
  cat(length(subset), subset)
  print(1 - mean(replicate(TIMES,errorest (as.simple.formula(subset, "Target"), 
                                           data=dataTest, 
                                           model=mymethod, 
                                           estimator = "cv", 
                                           predict = mypredict, 
                                           est.para=mycontrol.10)$error)))
}

mymethod <- lda

subset <- forward.search(names(dataTest)[-1], evaluator.accuracy) 
f.lda2 <- as.simple.formula(subset, "Target")
print(f.lda2)

data.CRF <- cbind(data.CRF,dataTest[,subset])

#->GO TO INI
