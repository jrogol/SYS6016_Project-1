library(readr)
library(dplyr)
library(lubridate)
library(randomForest)
library(caret)
library(xgboost)
library(pROC)
setwd('/Users/meganstiles/Desktop/Data_Science/Spring_2017/Machine Learning/Project_1/')
# From: https://github.com/washingtonpost/data-police-shootings
#shootings <- read_csv('https://github.com/washingtonpost/data-police-shootings/raw/master/fatal-police-shootings-data.csv')


# Columns to factor
#factors <- c(4,5,7:14)

#shootings[, factors] <- lapply(shootings[, factors], as.factor)

# Create Response Variable

#shootings$attackGun <- 0
#shootings$attackGun[shootings$armed=='gun' & shootings$threat_level=='attack'] <- 1
#shootings$attackGun[shootings$armed=='hatchet and gun' | shootings$armed=='machete and gun'] <- 1
#Drop Unneeded Columns

#clean<- shootings[, -c(1,2,5,9,10,12)]


#write_csv(clean, 'Shootings_Data.csv') #Accessed on February 22, 2016 at 5 :00 pm E.T.

#Read in clean csv

shootings<- read.csv('Shootings_Data.csv')
str(shootings)

#Set response variable as factor
shootings$attackGun<- as.factor(shootings$attackGun)
str(shootings)

#Set Date as Date

shootings$date<- as.Date(shootings$date)

#Set Age as numeric

shootings$age<- as.numeric(shootings$age)

##Drop missing values from Race
shootings<- shootings[!is.na(shootings$race),]

#Drop missing values from attack

shootings<- shootings[!is.na(shootings$flee),]

#Perform Mean Imputation on Age Variable
shootings$age[is.na(shootings$age)]<- mean(shootings$age, na.rm=TRUE)

#Random Forest

#Set Training and Testing Sets

training.indices = sample(1:nrow(shootings), as.integer(nrow(shootings) * 0.80))
training.set = shootings[training.indices,]
testing.set = shootings[-training.indices,]

# Fit random forest
rf.fit <- randomForest(attackGun ~., data = training.set)

# Predict testing data.
predictions = predict(rf.fit, newdata = testing.set)

# Output raw accuracy.
sum(predictions == testing.set[,"attackGun"]) / nrow(testing.set) #57.959%

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
  model <- randomForest(attackGun ~., data = train)
  
  #Make predictions based on model for testing set
  predictions<- predict(rf.fit, newdata = test)
  
  #Calculate Accuracy
  
  accuracy<-sum(predictions == test[,"attackGun"]) / nrow(test)
  
  #Store accuracy for each run in vector
  raw_accuracy[i]= accuracy
}
raw_accuracy
mean(raw_accuracy) #73.06%


############################
### Tree Boosting###########
############################

#10 fold CV

#Create Folds
folds<- createFolds(shootings$attackGun, k=10, list = TRUE, returnTrain = FALSE)

param <- list('booster' = 'gbtree',
              "objective" = "multi:softprob",    
              "num_class" = 2,
              'max_depth' = 6,
              'eval_metric' = 'merror')


#initialzie empty vector to store accuracy
raw_accuracy<- vector()
i=0

for (i in 1:10) {
  #Create testing indicies based on folds
  test.indices<- folds[[i]]
  
  #Create training and testing sets
  train = shootings[-test.indices,]
  test = shootings[test.indices,]
  
  #Convert to Matrix
  train_X<-data.matrix(train[,1:8])
  train_Y<- data.matrix(train$attackGun)
  test_X<- data.matrix(test[,1:8])
  test_Y<- data.matrix(test$attackGun)
  
  #train Model
  model <- xgboost(param=param, data=train_X, label=train_Y, nrounds=6)
  
  #Make predictions based on model for testing set
  predictions<- predict(model, test_X)
  
  # reshape it to a num_class-columns matrix
  pred <- matrix(predictions, ncol=2, byrow=TRUE)
  
  # convert the probabilities to softmax labels (we have to subtract  one)
  pred_labels <- max.col(pred) - 1
 
  #Find Accuracy
  accuracy<-sum(pred_labels == test[,"attackGun"]) / nrow(test)
  
  #Store accuracy for each run in vector
  raw_accuracy[i]= accuracy
}

Avg_Accuracy = mean(raw_accuracy)
Avg_Accuracy 
############################
### Logistic Regression ####
############################


#Set Training and Testing Sets

training.indices = sample(1:nrow(shootings), as.integer(nrow(shootings) * 0.75))
training.set = shootings[training.indices,]
testing.set = shootings[-training.indices,]

lr.model<- glm(attackGun ~., data = training.set, family="binomial")
summary(lr.model)


#ROC Curve
library(ROCR)
p <- predict(lr.model, newdata = testing.set, type="response")
pr <- prediction(p, testing.set$attackGun)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc<- performance(pr, "auc")@y.values[[1]] 
auc #0.6554
plot(prf)
library(pROC)

###############################
#LR Boosting#################
###############################


boosted <- xgboost(data = data.matrix(training.set[,1:8]), label = data.matrix(training.set$attackGun), 
                    max_depth = 4, eta = 1, nthread = 2, nrounds = 8, objective = "binary:logistic")
#10 fold CV

#Create Folds
folds<- createFolds(shootings$attackGun, k=10, list = TRUE, returnTrain = FALSE)

#initialzie empty vector to store accuracy
AUC_all<- vector()
i=0
for (i in 1:10) {
  #Create testing indicies based on folds
  test.indices<- folds[[i]]
  
  #Create training and testing sets
  train = shootings[-test.indices,]
  test = shootings[test.indices,]
  
  #Convert to Matrix
  train_X<-data.matrix(train[,1:8])
  train_Y<- data.matrix(train$attackGun)
  test_X<- data.matrix(test[,1:8])
  test_Y = data.matrix(test$attackGun)
  
  #train Model
  model <- xgboost(data = train_X, label = train_Y, 
                   max_depth = 2, eta = 1, nthread = 2, nrounds = 8, objective = "binary:logistic")
  
  #Make predictions based on model for testing set
  predictions<- predict(model, test_X)
  pr <- prediction(p, testing.set$attackGun)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  auc<- performance(pr, "auc")@y.values[[1]]
  
  #Store auc for each run in vector
  AUC_all[i]<-auc
}

Avg_AUC = mean(AUC_all)
Avg_AUC #0.6111445
