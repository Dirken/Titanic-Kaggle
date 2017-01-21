###############################################
## Non-Linear Quadratics Methods
###############################################

setwd("/home/dirken/Downloads/APA/titanic2/titanic/csvs/")

test <- read.csv("parsed/test_clean.csv", stringsAsFactors = FALSE)
train <- read.csv("parsed/train_clean.csv", stringsAsFactors = FALSE)

###############################################
## SVM
###############################################

library(kernlab)

svm.model <- ksvm(Survived ~ sex.name + pclass + age + fare + fare.distance, data = train)
train$survived_pred <- predict(svm.model, train, type = "response")
test$survived <- predict(svm.model, test, type = "response")

###############################################
## Random Forest
###############################################

# Create random forest based on PCLASS, SEX, FARE, and AGE
forest <- randomForest(survived ~ sex.name + pclass + age + fare + fare.distance, 
                       data = train, ntree = 15000, importance = TRUE)

# Use scaled variables
forest_scale <- randomForest(survived ~ sex.name + pclass + age_scale + fare_scale,
                             data = train, ntree = 15000, importance = TRUE)

summary(forest)

# Extract the importance of each variable
importance(forest)

# Save our model as a string
model <- "randomForest(survived ~ sex.name + pclass + age + fare + fare.distance, data = train, ntree = 5000, importance = TRUE)"
model_scale <- "randomForest(survived ~ sex.name + pclass + age_scale + fare_scale,
                             data = train, ntree = 15000, importance = TRUE)"

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(forest, train)
train$survived_pred2 <- predict(forest_scale, train)

# Make our prediction on the TEST data set
test$survived <- predict(forest, test)
test2 <- test
test2$survived <- predict(forest_scale, test2)
