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

##### TRATAMIENTO FILAS #####

listTmp <- rep(length(dataTex))

for (i in seq(2,length(dataTex), by=1)) {
  listTmp[i-1] = mean(dataTex[1:15,i])
}

dataTex <- rbind(dataTex,listTmp)

##### RENOMBRAR FILAS #####

dataMar$V1<-as.character(lapply(dataMar$V1, function(x) {
  pos<-str_locate(x,' ')
  if (!is.na(pos[1,1]))
    (substr(x,0,pos))
  else x
}))

##### TRATAMIENTO MAGNITUD VALORES #####

dataTmp <- cbind(dataMar[,-1],dataSha,dataTex)
dataTmp <- scale(dataTmp)
data <- cbind(as.factor(dataMar[,1]),dataTmp)

##### RENOMBRAR COLUMNAS #####

data <- as.data.frame(data)
names(data)[1]<-paste("Target")
names(data)[2:length(data[1,])] <- paste("V", 2:length(data[1,]), sep = "")
data$Target<-as.factor(data$Target)

