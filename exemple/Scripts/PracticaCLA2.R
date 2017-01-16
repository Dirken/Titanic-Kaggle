library(e1071)

N<-length(data.CRF[,1])/2

data.learn <- data.CRF[1:N, ]
data.test <- data.CRF[(N+1):(N*2), ]

k <- 10 
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 

valid.error <- rep(0,k)

train.svm.kCV <- function (which.kernel, myC, kCV=10)
{
  for (i in 1:kCV) 
  {  
    train <- data.learn[folds!=i,]
    valid <- data.learn[folds==i,]
    
    x_train <- train[,2:21]
    t_train <- train[,1]
    
    switch(which.kernel,
           linear={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="linear", scale = FALSE)},
           poly.2={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="polynomial", degree=2, coef0=1, scale = FALSE)},
           poly.3={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="polynomial", degree=3, coef0=1, scale = FALSE)},      
           RBF={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="radial", scale = FALSE)},
           stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
    x_valid <- valid[,2:21]
    pred <- predict(model,x_valid)
    t_true <- valid[,1]
    
    valid.error[i] <- sum(pred != t_true)/length(t_true)
  }
  100*sum(valid.error)/length(valid.error)
}

#### TRAINING ####

C <- 1
  
(VA.error.linear <- train.svm.kCV ("linear", myC=C))
(VA.error.poly.2 <- train.svm.kCV ("poly.2", myC=C))
(VA.error.poly.3 <- train.svm.kCV ("poly.3", myC=C))
(VA.error.RBF <- train.svm.kCV ("RBF", myC=C))

#### REFIT ####

model <- svm(data.learn[,2:21], data.learn[,1], type="C-classification", cost=C, kernel="polynomial", degree=3, coef0=1, scale = FALSE)

#### TESTING ####

pred <- predict(model,data.test[,2:21])
t_true <- data.test[,1]

table(pred,t_true)

#### PREDICTION ERROR ####

(sum(pred != t_true)/length(t_true))
