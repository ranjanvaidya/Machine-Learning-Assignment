## dOWNLOADING the datasets as dataframe.
training <- as.data.frame(read.table("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", sep=",", header=T))
str(training)
testing <- as.data.frame(read.table("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", sep=",", header=T))
str(testing)
## Preprocessing the training dataset
library(caret)
nsv <- nearZeroVar(training,saveMetrics=TRUE)
training <- training[, nsv$nzv == FALSE]
training <- training[, -c(1:6)]
str(training)
library(dplyr)
training <- select(training, -c(max_roll_belt:var_yaw_belt))
training <- select(training, -c(max_picth_arm:amplitude_yaw_arm))
training <- select(training, -c(max_roll_dumbbell: amplitude_pitch_dumbbell))
training <- select(training, -c(var_accel_dumbbell: var_yaw_dumbbell))
training <- select(training, -c(max_picth_forearm: amplitude_pitch_forearm))
str(training)
training <- select(training, -c(var_accel_arm, var_accel_forearm))
str(training)
## Preprocessing the testing dataset
nsv <- nearZeroVar(testing,saveMetrics=TRUE)
testing <- testing[, nsv$nzv == FALSE]
testing <- testing[, -c(1:6)]
str(testing)
## Creating data partition (for validating the training model)
library(caret)
set.seed(10000)
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
training <- training[inTrain,]
validating <- training[-inTrain,]
dim(training); dim(validating)
## Predicting with Classification Tree (The accuracy is low i.e. to an extent of 0.4983)
modFit <- train(classe ~ .,method="rpart",data=training)
prediction_tree <- predict(modFit,newdata=validating)
confusionMatrix(prediction_tree, validating$classe)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
tree_based_prediction <- predict(modFit,newdata=testing)
tree_based_prediction
## running an linear discrment analysis (The accuracy increases to the level of 0.7051)
library(klaR)
set.seed(20000)
modlda = train(classe ~ .,data=training,method="lda")
validating_LDA <- predict(modlda,validating)
confusionMatrix(validating_LDA, validating$classe)
plda = predict(modlda,testing)
plda
## Predictions with Random Forest (Very high accuracy - to an extent of 1 in the validating data)
library(randomForest)
set.seed(30000)
modFit_RF <- randomForest(classe ~ ., data=training)
validating_RF <- predict(modFit_RF, validating)
confusionMatrix(validating_RF, validating$classe)
prediction_RF <- predict(modFit_RF, testing)
prediction_RF