library(readr)
library(dplyr)
library(lubridate)
library(randomForest)
library(caret)
library(xgboost)
library(pROC)
library(kernlab) # SVM
library(ROCR) # ROC Curve
library(e1071) # Naive Bayes

setwd('/Users/meganstiles/Desktop/Data_Science/Spring_2017/Machine Learning/Project_1/')
setwd("~/UVaMSDS/MachineLearning/Project1")

#Read in clean csv
shootings<- read.csv('Shootings_Data.csv')
str(shootings)

#Set Age as numeric
shootings$age<- as.numeric(shootings$age)

#Bin Armed Values into whether or not they had a gun
table(shootings$armed)
shootings$gun<- 0
shootings$gun[shootings$armed=='hatchet and gun' | shootings$armed=='machete and gun']<- 1
shootings$gun[shootings$armed == 'gun and knife' | shootings$armed =='guns and explosives']<- 1
shootings$gun[shootings$armed == 'gun'] <- 1

#drop Armed column
shootings<- shootings[, -1]

#Check Missingness
sapply(shootings, function(x) sum(is.na(x)))
sapply(shootings, function(x) sum(is.na(x))/length(x))

#Run Correlation Matrix to see if values are missing at random
naCheck <- shootings
naCheck$race <- abs(is.na(naCheck$race))
naCheck$age <- abs(is.na(naCheck$age))
naCheck$flee <- abs(is.na(naCheck$flee))
#all numeric for correlation matrix
#naCheck$manner_of_death <- as.numeric(naCheck$manner_of_death)
naCheck$gender <- as.numeric(naCheck$gender)
naCheck$signs_of_mental_illness <- as.numeric(naCheck$signs_of_mental_illness)
naCheck$threat_level <- as.numeric(naCheck$threat_level)
#naCheck$body_camera <- as.numeric(naCheck$body_camera)
cor(naCheck)
str(naCheck)
# there does not appear to be any patterns with missing values - evidence for missing at random
################
### We removed manner of death and body camera?
################


##Drop missing values from Race
shootings<- shootings[!is.na(shootings$race),]

#Drop missing values from flee
shootings<- shootings[!is.na(shootings$flee),]

#Perform Mean Imputation on Age Variable
shootings$age[is.na(shootings$age)]<- mean(shootings$age, na.rm=TRUE)
sum(is.na(shootings$age))
################ NO LONGER RELEVANT
#Remove Missing Armed Values
#shootings<- shootings[!is.na(shootings$armed),]
################

table(shootings$signs_of_mental_illness)/nrow(shootings)

####Set Training and Testing Sets
training.indices = sample(1:nrow(shootings), as.integer(nrow(shootings) * 0.80))
training.set = shootings[training.indices,]
testing.set = shootings[-training.indices,]

###############
#Random Forest#
###############
# Fit random forest
rf.fit <- randomForest(signs_of_mental_illness ~., data = training.set)

# Predict testing data.
predictions = predict(rf.fit, newdata = testing.set)

# Output raw accuracy.
sum(predictions == testing.set[,"signs_of_mental_illness"]) / nrow(testing.set) #72.43%

#10-Fold CV

#Create Folds
folds<- createFolds(shootings$attackGun, k=10, list = TRUE, returnTrain = FALSE)

raw_accuracy<- vector()
i=0
for (i in 1:10) {
  #Create testing indicies based on folds
  test.indices<- folds[[i]]
  
  #Create training and testing sets
  train = shootings[-test.indices,]
  test = shootings[test.indices,]
  
  #train Model
  model <- randomForest(signs_of_mental_illness ~., data = train)
  
  #Make predictions based on model for testing set
  predictions<- predict(rf.fit, newdata = test)
  
  #Calculate Accuracy
  
  accuracy<-sum(predictions == test[,"signs_of_mental_illness"]) / nrow(test)
  
  #Store accuracy for each run in vector
  raw_accuracy[i]= accuracy
}
raw_accuracy
mean(raw_accuracy) #75.911

#This accuracy is due to the model just predicting False for "signs of mental illness" each time.
#The data is approximately 75% False for mental illness. To account for this we will look at
# oversampling the number of True signs of mental illness for our training set

#####################
#Logistic Regression#
#####################
lm1 <- glm(signs_of_mental_illness ~ . , family=binomial, data=training.set)
yhat <- predict(lm1, newdata = testing.set, type="response")
glm.pred <- as.factor(ifelse(yhat>0.4, 'True', 'False'))

num.correct = sum(glm.pred == testing.set$signs_of_mental_illness)
accuracy = num.correct / nrow(testing.set) 
### Accuracy = 0.717
table(glm.pred, testing.set$signs_of_mental_illness)

#Create ROCR prediction object and extract ROC curve and AUC
pred <- prediction(yhat, testing.set$signs_of_mental_illness)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc = performance(pred, "auc")@y.values[[1]]
plot(perf, col=rainbow(7), main = paste("ROC Curve: Logistic (AUC=", round(auc,2), ")", sep=""), xlab="FPR", 
     ylab="TPR")    
abline(0, 1, lty="dashed") #add a 45 degree line
### AUC = 0.66

#############
#Naive Bayes#
#############
model <- naiveBayes(signs_of_mental_illness ~ ., training.set)
yhat <- predict(model, testing.set, type = "class")

num.correct = sum(yhat == testing.set$signs_of_mental_illness)
accuracy = num.correct / nrow(testing.set)
### Accuracy = 0.736
table(yhat, testing.set$signs_of_mental_illness)

#############
#    SVM    #
#############
# Fit SVM.
svm.fit <- ksvm(signs_of_mental_illness ~ ., data=training.set, type="C-svc", kernel="rbfdot", C=50)
# Predict testing data.
predictions = predict(svm.fit, newdata=testing.set)
# Output raw accuracy.
sum(predictions == testing.set$signs_of_mental_illness) / nrow(testing.set)
### Accuracy = 0.742
table(predictions, testing.set$signs_of_mental_illness)


#########################################
###Mental Illness Over sampling##########
#########################################

mental<- shootings

true.index<- sample(which(mental$signs_of_mental_illness == 'True'), 450)
false.index<- sample(which(mental$signs_of_mental_illness == 'False'),1000)
total.index<- c(true.index, false.index)

training.set = mental[total.index,]
testing.set = mental[-total.index,]

table(training.set$signs_of_mental_illness)/nrow(training.set)
table(testing.set$signs_of_mental_illness)/nrow(testing.set)

# Fit random forest
rf.fit <- randomForest(signs_of_mental_illness ~., data = training.set)

# Predict testing data.
predictions = predict(rf.fit, newdata = testing.set)

# Output raw accuracy.
sum(predictions == testing.set[,"signs_of_mental_illness"]) / nrow(testing.set) #86.14%

table(predictions,testing.set$signs_of_mental_illness)

#Validation
set.seed(8)
raw_accuracy<- vector()
i=0
for (i in 1:10) {
  #Create testing indicies 
  true.index<- sample(which(mental$signs_of_mental_illness == 'True'), 450)
  false.index<- sample(which(mental$signs_of_mental_illness == 'False'),1000)
  total.index<- c(true.index, false.index)
  
  training.set = mental[total.index,]
  testing.set = mental[-total.index,]
  
  # Fit random forest
  rf.fit <- randomForest(signs_of_mental_illness ~., data = training.set)
  
  # Predict testing data.
  predictions = predict(rf.fit, newdata = testing.set)
  
  #Calculate Accuracy
  accuracy<-sum(predictions == testing.set[,"signs_of_mental_illness"]) / nrow(testing.set)
  
  #Store accuracy for each run in vector
  raw_accuracy[i]= accuracy
}
raw_accuracy
mean(raw_accuracy) #85.56%

#####################
#Logistic Regression#
#####################
lm1 <- glm(signs_of_mental_illness ~ . , family=binomial, data=training.set)
yhat <- predict(lm1, newdata = testing.set, type="response")
glm.pred <- as.factor(ifelse(yhat>0.4, 'True', 'False'))
num.correct = sum(glm.pred == testing.set$signs_of_mental_illness)
accuracy = num.correct / nrow(testing.set)
### Accuracy = 0.771
table(glm.pred, testing.set$signs_of_mental_illness)

# create ROCR prediction object and extract ROC curve and AUC
pred <- prediction(yhat, testing.set$signs_of_mental_illness)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc = performance(pred, "auc")@y.values[[1]]
plot(perf, col=rainbow(7), main = paste("ROC Curve: Logistic (AUC=", round(auc,2), ")", sep=""), xlab="FPR", 
     ylab="TPR")    
abline(0, 1, lty="dashed") #add a 45 degree line
### AUC = 0.76

#############
#Naive Bayes#
#############
model <- naiveBayes(signs_of_mental_illness ~ ., training.set)
yhat <- predict(model, testing.set, type = "class")

table(yhat, testing.set$signs_of_mental_illness)
num.correct = sum(yhat == testing.set$signs_of_mental_illness)
accuracy = num.correct / nrow(testing.set)
### Accuracy = 0.815

#############
#    SVM    #
#############
# Fit SVM.
svm.fit <- ksvm(signs_of_mental_illness ~ ., data=training.set, type="C-svc", kernel="rbfdot", C=50)
# Predict testing data.
predictions = predict(svm.fit, newdata=testing.set)
# Output raw accuracy.
sum(predictions == testing.set$signs_of_mental_illness) / nrow(testing.set)
table(predictions, testing.set$signs_of_mental_illness)
### Accuracy = 0.774
