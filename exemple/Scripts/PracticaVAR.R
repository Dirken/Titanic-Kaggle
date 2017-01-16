library(FSelector)
library(CORElearn)
library(ipred)

library(rJava)


##### REDUCCION VARIABLES #####

subset.CFS <- cfs (Target~., data)
data.CFS <- cbind(data[1],data[subset.CFS])

# 1. Correlation filter + Random Forest (20 variables)

weights.randomForest <- random.forest.importance (Target~., data.CFS, importance.type = 1)
data.CRF <- cbind(data[1],data[cutoff.k(weights.randomForest, 20)])

# 2. Correlation filter + ReliefF (20 variables)

estReliefF <- attrEval(Target ~ ., data.CFS, estimator="ReliefFequalK", ReliefIterations=500)
subset.relief <- sort(estReliefF,decreasing=TRUE)[1:20] 
data.CRelief <- cbind(data[1],data[names(subset.relief)])

# 3. Consistency filter

subset.Consistency <- consistency (Target~., data)
(subset.Consistency.formula <- as.simple.formula(subset.Consistency,"Target"))
data.Consistency <- cbind(data[1],data[subset.Consistency])

##### TESTEO DE LAS VARIABLES #####

K <- 10
TIMES <- 10

dataTest <- data.CRF

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

# 1. LDA + Backguard search

mymethod <- lda

subset <- backward.search(names(dataTest)[-1], evaluator.accuracy)
f.lda1 <- as.simple.formula(subset, "Target")
print(f.lda1)

# 2. LDA + Forward search

subset <- forward.search(names(dataTest)[-1], evaluator.accuracy) 
f.lda2 <- as.simple.formula(subset, "Target")
print(f.lda2)

# 3. QDA + Forward search

mymethod <- qda

subset <- forward.search(names(dataTest)[-1], evaluator.accuracy)
f.qda <- as.simple.formula(subset, "Target")
print(f.qda)