library(MASS)  
library(cclust)
library(stringr)

set.seed(4)

#### LECTURA FICHEROS #####

setwd("/Users/alex-mac/Documents/uni/3ro 2n cuatri/apa/practica/titanic/exemple/Scripts")

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
dataTex <- dataTex[,-1]
dataSha <- dataSha[,-1]
dataMar <- dataMar[,-1]

##### TRATAMIENTO FILAS #####

listTmp <- rep(length(dataTex))

for (i in seq(2,length(dataTex), by=1)) {
  listTmp[i-1] = mean(dataTex[1:15,i])
}

dataTex <- rbind(dataTex,listTmp)

##### TRATAMIENTO MAGNITUD VALORES #####

dataTmp <- cbind(dataMar,dataSha,dataTex)
dataTmp <- scale(dataTmp)

##### TO MATRIX #####

dataC = matrix(ncol=length(dataTmp[1,]),nrow=length(dataTmp[,1])) 
for (i in seq(1,length(dataTmp[1,]), by=1)) {
  dataC[,i] = dataTmp[,i] 
}

##### CLUSTERING #####

K <- 10
kmeans <- cclust (dataC,K,iter.max=100,method="kmeans",dist="euclidean")

data <- cbind(kmeans$cluster,dataC)
data <- as.data.frame(data)
names(data)[1]<-paste("Target")
data$Target <- factor(data$Target)

##### VISUALIDAR DATOS - LDA #####

data.lda <- lda(Target ~ ., data, prior = seq(1,1,length.out=K)/K)
data.lda
plot(data.lda, col = as.numeric(data$Target))
