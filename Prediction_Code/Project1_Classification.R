##### Machine Learning Project 1 - Classification
##### Kaley Hanrahan (knh9yd), James Rogol (jr2ef), Aksheetha Srihdar (as8zb), Megan Stiles (mes5ac)

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

### Preliminary Cleaning
shootings <- read_csv('https://github.com/washingtonpost/data-police-shootings/raw/master/fatal-police-shootings-data.csv')
factors <- c(4,5,7:14)
shootings[, factors] <- lapply(shootings[, factors], as.factor)
shootings <- shootings[, -c(1:4,9,10,14)]

# Because the data was updated regularly, we cleaned the data and saved a csv file
# on February 23 so that our data and analysis would stay constant.
write_csv(shootings, 'Shootings_Data.csv')

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
naCheck$gender <- as.numeric(naCheck$gender)
naCheck$signs_of_mental_illness <- as.numeric(naCheck$signs_of_mental_illness)
naCheck$threat_level <- as.numeric(naCheck$threat_level)
cor(naCheck)
str(naCheck)
# there does not appear to be any patterns with missing values - evidence for missing at random

##Drop missing values from Race
shootings<- shootings[!is.na(shootings$race),]

#Drop missing values from flee
shootings<- shootings[!is.na(shootings$flee),]

#Perform Mean Imputation on Age Variable
shootings$age[is.na(shootings$age)]<- mean(shootings$age, na.rm=TRUE)
sum(is.na(shootings$age))

table(shootings$signs_of_mental_illness)/nrow(shootings)

####Set Training and Testing Sets
set.seed(7)
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
sum(predictions == testing.set[,"signs_of_mental_illness"]) / nrow(testing.set) #72.5%

# Confusion Matrix
table(predictions, testing.set$signs_of_mental_illness)
##  predictions False True
##  False       288  109
##  True        1    2

# Sensitivity
table(predictions,testing.set$signs_of_mental_illness)[2,2]/table(testing.set$signs_of_mental_illness)[2]
# 1.8%

#This accuracy is due to the model just predicting False for "signs of mental illness" each time.
#The data is approximately 75% False for mental illness. To account for this we will look at
# oversampling the number of True signs of mental illness for our training set


#10-Fold CV

#Create Folds
folds<- createFolds(shootings$signs_of_mental_illness, k=10, list = TRUE, returnTrain = FALSE)

raw_accuracy<- vector()
raw_sensitivity<- vector()
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
  
  #Calculate sensitivity
  sensitivity <- table(predictions,test$signs_of_mental_illness)[2,2]/table(test$signs_of_mental_illness)[2]
  #Store sensitivity for each run in vector
  raw_sensitivity[i]= sensitivity
}
raw_accuracy
mean(raw_accuracy) # 76.3%
raw_sensitivity
mean(raw_sensitivity) # 5.6%

#####################
#Logistic Regression#
#####################
lm1 <- glm(signs_of_mental_illness ~ . , family=binomial, data=training.set)
yhat <- predict(lm1, newdata = testing.set, type="response")
glm.pred <- as.factor(ifelse(yhat>0.5, 'True', 'False'))

num.correct = sum(glm.pred == testing.set$signs_of_mental_illness)
num.correct / nrow(testing.set) 
### Accuracy = 72.75%
table(glm.pred, testing.set$signs_of_mental_illness)

# Confusion Matrix
## glm.pred   False True
## False      286   106
## True       3     5

# Sensitivity
table(glm.pred,testing.set$signs_of_mental_illness)[2,2]/table(testing.set$signs_of_mental_illness)[2]
# = 4.5%

#Create ROCR prediction object and extract ROC curve and AUC
pred <- prediction(yhat, testing.set$signs_of_mental_illness)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc = performance(pred, "auc")@y.values[[1]]
plot(perf, col=rainbow(7), main = paste("ROC Curve: Logistic (AUC=", round(auc,2), ")", sep=""), xlab="FPR", 
     ylab="TPR")    
abline(0, 1, lty="dashed") #add a 45 degree line
### AUC = 0.73

#10-Fold CV

#Create Folds
folds<- createFolds(shootings$signs_of_mental_illness, k=10, list = TRUE, returnTrain = FALSE)

set.seed(8)
raw_accuracy<- vector()
raw_sensitivity<- vector()
i=0
for (i in 1:10) {
  #Create testing indicies based on folds
  test.indices<- folds[[i]]
  
  #Create training and testing sets
  train = shootings[-test.indices,]
  test = shootings[test.indices,]
  
  # Fit logistic regression
  lm1 <- glm(signs_of_mental_illness ~ . , family=binomial, data=train)
  # Predict testing data.
  yhat <- predict(lm1, newdata = test, type="response")
  glm.pred <- as.factor(ifelse(yhat>0.5, 'True', 'False'))
  
  #Calculate Accuracy
  accuracy<-sum(glm.pred == test[,"signs_of_mental_illness"]) / nrow(test)
  #Store accuracy for each run in vector
  raw_accuracy[i]= accuracy
  
  #Calculate sensitivity
  sensitivity <- table(glm.pred,test$signs_of_mental_illness)[2,2]/table(test$signs_of_mental_illness)[2]
  #Store sensitivity for each run in vector
  raw_sensitivity[i]= sensitivity
}
raw_accuracy
mean(raw_accuracy) # 75.3%
raw_sensitivity
mean(raw_sensitivity) # 6.4%

#############
#Naive Bayes#
#############
model <- naiveBayes(signs_of_mental_illness ~ ., training.set)
yhat <- predict(model, testing.set, type = "class")

num.correct = sum(yhat == testing.set$signs_of_mental_illness)
num.correct / nrow(testing.set)
### Accuracy = 73.5%
table(yhat, testing.set$signs_of_mental_illness)

# Confusion Matrix
##yhat    False  True
##False   274    91
##True    15     20

# Sensitivity
table(yhat,testing.set$signs_of_mental_illness)[2,2]/table(testing.set$signs_of_mental_illness)[2]
# Sensitivity = 18%

#10-Fold CV

#Create Folds
folds<- createFolds(shootings$signs_of_mental_illness, k=10, list = TRUE, returnTrain = FALSE)

raw_accuracy<- vector()
raw_sensitivity<- vector()
i=0
for (i in 1:10) {
  #Create testing indicies based on folds
  test.indices<- folds[[i]]
  
  #Create training and testing sets
  train = shootings[-test.indices,]
  test = shootings[test.indices,]
  
  #train Model
  model <- naiveBayes(signs_of_mental_illness ~ ., train)
  yhat <- predict(model, testing.set, type = "class")
  
  #Make predictions based on model for testing set
  predictions<- predict(rf.fit, newdata = test)
  
  #Calculate Accuracy
  
  accuracy<-sum(predictions == test[,"signs_of_mental_illness"]) / nrow(test)
  
  #Store accuracy for each run in vector
  raw_accuracy[i]= accuracy
  
  #Calculate sensitivity
  sensitivity <- table(predictions,test$signs_of_mental_illness)[2,2]/table(test$signs_of_mental_illness)[2]
  #Store sensitivity for each run in vector
  raw_sensitivity[i]= sensitivity
}
raw_accuracy
mean(raw_accuracy) # 76.3%
raw_sensitivity
mean(raw_sensitivity) # 5.6%




#########################################
###Mental Illness Over sampling##########
#########################################

mental<- shootings
table(mental$signs_of_mental_illness)

set.seed(7)
true.index <- which(mental$signs_of_mental_illness=='True')
false.index <- sample(which(mental$signs_of_mental_illness == 'False'), 700)
total.index<- c(true.index, false.index)
undersampled <- mental[total.index,]
  
training.indices = sample(1:nrow(undersampled), as.integer(nrow(undersampled) * 0.80))
training.set = undersampled[training.indices,]
testing.set = undersampled[-training.indices,]

## Distribution of new sample
table(training.set$signs_of_mental_illness)/nrow(training.set)
#False      True 
#0.5797706  0.4202294 
table(testing.set$signs_of_mental_illness)/nrow(testing.set)

# Fit random forest
rf.fit <- randomForest(signs_of_mental_illness ~., data = training.set)

# Predict testing data.
predictions = predict(rf.fit, newdata = testing.set)

# Output raw accuracy.
sum(predictions == testing.set[,"signs_of_mental_illness"]) / nrow(testing.set) #65.42%

table(predictions,testing.set$signs_of_mental_illness)
## predictions False  True
## False       94     43
## True        40     63

# Sensitivity
table(predictions,testing.set$signs_of_mental_illness)[2,2]/table(testing.set$signs_of_mental_illness)[2]
# = 59.4%

#Validation
set.seed(8)
raw_accuracy<- vector()
raw_sensitivity<- vector()
i=0
for (i in 1:10) {
  #Create testing indicies 
  true.index <- which(mental$signs_of_mental_illness=='True')
  false.index <- sample(which(mental$signs_of_mental_illness == 'False'), 700)
  total.index<- c(true.index, false.index)
  undersampled <- mental[total.index,]
  
  training.indices = sample(1:nrow(undersampled), as.integer(nrow(undersampled) * 0.80))
  training.set = undersampled[training.indices,]
  testing.set = undersampled[-training.indices,]
  
  # Fit random forest
  rf.fit <- randomForest(signs_of_mental_illness ~., data = training.set)
  
  # Predict testing data.
  predictions = predict(rf.fit, newdata = testing.set)
  
  #Calculate Accuracy
  accuracy<-sum(predictions == testing.set[,"signs_of_mental_illness"]) / nrow(testing.set)
  #Store accuracy for each run in vector
  raw_accuracy[i]= accuracy
  
  #Calculate sensitivity
  sensitivity <- table(predictions,testing.set$signs_of_mental_illness)[2,2]/table(testing.set$signs_of_mental_illness)[2]
  #Store sensitivity for each run in vector
  raw_sensitivity[i]= sensitivity
}
raw_accuracy
mean(raw_accuracy) # 63.7%
raw_sensitivity
mean(raw_sensitivity) # 58.4%

#####################
#Logistic Regression#
#####################
lm1 <- glm(signs_of_mental_illness ~ . , family=binomial, data=training.set)
yhat <- predict(lm1, newdata = testing.set, type="response")
glm.pred <- as.factor(ifelse(yhat>0.5, 'True', 'False'))
num.correct = sum(glm.pred == testing.set$signs_of_mental_illness)
num.correct / nrow(testing.set)
### Accuracy = 65%
table(glm.pred, testing.set$signs_of_mental_illness)

# Sensitivity
table(glm.pred,testing.set$signs_of_mental_illness)[2,2]/table(testing.set$signs_of_mental_illness)[2]
# = 58.7%

# create ROCR prediction object and extract ROC curve and AUC
pred <- prediction(yhat, testing.set$signs_of_mental_illness)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc = performance(pred, "auc")@y.values[[1]]
plot(perf, col=rainbow(7), main = paste("ROC Curve: Logistic (AUC=", round(auc,2), ")", sep=""), xlab="FPR", 
     ylab="TPR")    
abline(0, 1, lty="dashed") #add a 45 degree line
### AUC = 0.71

#Validation
set.seed(8)
raw_accuracy<- vector()
raw_sensitivity<- vector()
i=0
for (i in 1:10) {
  #Create testing indicies 
  true.index <- which(mental$signs_of_mental_illness=='True')
  false.index <- sample(which(mental$signs_of_mental_illness == 'False'), 700)
  total.index<- c(true.index, false.index)
  undersampled <- mental[total.index,]
  
  training.indices = sample(1:nrow(undersampled), as.integer(nrow(undersampled) * 0.80))
  training.set = undersampled[training.indices,]
  testing.set = undersampled[-training.indices,]
  
  # Fit model
  lm1 <- glm(signs_of_mental_illness ~ . , family=binomial, data=training.set)
  # Predict testing data.
  yhat <- predict(lm1, newdata = testing.set, type="response")
  glm.pred <- as.factor(ifelse(yhat>0.5, 'True', 'False'))
  
  #Calculate Accuracy
  accuracy<-sum(glm.pred == testing.set[,"signs_of_mental_illness"]) / nrow(testing.set)
  #Store accuracy for each run in vector
  raw_accuracy[i]= accuracy
  
  #Calculate sensitivity
  sensitivity <- table(glm.pred,testing.set$signs_of_mental_illness)[2,2]/table(testing.set$signs_of_mental_illness)[2]
  #Store sensitivity for each run in vector
  raw_sensitivity[i]= sensitivity
}
raw_accuracy
mean(raw_accuracy) # 63.6%
raw_sensitivity
mean(raw_sensitivity) # 56.9%


#############
#Naive Bayes#
#############
model <- naiveBayes(signs_of_mental_illness ~ ., training.set)
yhat <- predict(model, testing.set, type = "class")

table(yhat, testing.set$signs_of_mental_illness)
num.correct = sum(yhat == testing.set$signs_of_mental_illness)
num.correct / nrow(testing.set)
### Accuracy = 66.25%

# Confusion Matrix
## yhat    False  True
## False    92    42
## True     39    67

# Sensitivity
table(yhat,testing.set$signs_of_mental_illness)[2,2]/table(testing.set$signs_of_mental_illness)[2]
# =61.5%

#Validation
set.seed(8)
raw_accuracy<- vector()
raw_sensitivity<- vector()
i=0
for (i in 1:10) {
  #Create testing indicies 
  true.index <- which(mental$signs_of_mental_illness=='True')
  false.index <- sample(which(mental$signs_of_mental_illness == 'False'), 700)
  total.index<- c(true.index, false.index)
  undersampled <- mental[total.index,]
  
  training.indices = sample(1:nrow(undersampled), as.integer(nrow(undersampled) * 0.80))
  training.set = undersampled[training.indices,]
  testing.set = undersampled[-training.indices,]
  
  # Fit model
  model <- naiveBayes(signs_of_mental_illness ~ ., training.set)
  yhat <- predict(model, testing.set, type = "class")
  
  #Calculate Accuracy
  accuracy<-sum(yhat == testing.set[,"signs_of_mental_illness"]) / nrow(testing.set)
  #Store accuracy for each run in vector
  raw_accuracy[i]= accuracy
  
  #Calculate sensitivity
  sensitivity <- table(yhat,testing.set$signs_of_mental_illness)[2,2]/table(testing.set$signs_of_mental_illness)[2]
  #Store sensitivity for each run in vector
  raw_sensitivity[i]= sensitivity
}
raw_accuracy
mean(raw_accuracy) # 64.0%
raw_sensitivity
mean(raw_sensitivity) # 54.5%
