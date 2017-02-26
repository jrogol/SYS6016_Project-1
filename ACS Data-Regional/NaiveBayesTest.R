library(e1071)

test <- naiveBayes(age_bin~wday+armed+gender+signs_of_mental_illness+body_camera+month+state+
                     manner_of_death,
                   shootings, subset = which(shootings$year == 2015))
test

test_preds <- predict(test, shootings[shootings$year == 2016,])

table(test_preds,shootings$age_bin[shootings$year == 2016])


library(C50)

test <- C5.0(age_bin~wday+armed+gender+signs_of_mental_illness+body_camera+month+state+
                     manner_of_death, 
                   shootings, subset = which(shootings$year == 2015), rules = T)
test

test_preds <- predict(test, shootings[shootings$year == 2016,])

table(test_preds,shootings$age_bin[shootings$year == 2016])
