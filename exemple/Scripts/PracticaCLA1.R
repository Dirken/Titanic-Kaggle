
N <- length(data.CRF[,1])
learn <- sample(1:N, N/2)

data.learn <- data.CRF[learn, ]
data.test <- data.CRF[-learn, ]

#### LEARNING ####

data.lda.cv <- lda(Target ~ ., prior = c(1,1,1,1,1,1,1,1,1,1)/10, data = data.CRF, subset=learn, CV=TRUE) 
summary(data.lda.cv$class)

tab <- table(data.CRF$Target[learn], data.lda.cv$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

#### TESTING ####

data.lda <- lda(Target ~ ., prior = c(1,1,1,1,1,1,1,1,1,1)/10, data = data.learn)

pred <- predict(data.lda, data.test[,2:21])$class
t_true <- data.test[,1]

table(pred,t_true)

#### PREDICTION ERROR ####

(sum(pred != t_true)/length(t_true))
