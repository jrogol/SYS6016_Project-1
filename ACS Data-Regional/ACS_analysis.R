#Looking at files 2 (Social) and 3 (Economic) from ACS
library(readr)
library(dplyr)
library(lubridate)

#### Read in the Initial Data ####

# Read in the WaPo fatal shootings data
# From: https://github.com/washingtonpost/data-police-shootings
shootings <- read_csv('https://github.com/washingtonpost/data-police-shootings/raw/master/fatal-police-shootings-data.csv')

# Turn character Columns to factors (where appropriate)
factors <- c(4,5,7:14)
shootings[, factors] <- lapply(shootings[, factors], as.factor)

# Extract Weekday, Month and Year from the date
shootings <- shootings %>%
  mutate(wday = wday(date, label = T), 
         month = month(date, label =T),
         year = year(date))

# Create factor levels for ages
shootings <- shootings %>%
  mutate(age_bin = cut(shootings$age,c(0,17,24,34,44,54,64,200),
                       include.lowest = F, right = T, ordered_result = T))

#collapse armed factor levels into armed, not armed and undermined
armed_levels <- as.list(levels(shootings$armed))
armed_levels[[61]] <- NULL #removing unarmed
armed_levels[[61]] <- NULL #removing undetermined
levels(shootings$armed)[levels(shootings$armed)%in%c(armed_levels)] <- "armed" #collapsed the factors into either armed or unarmed

#### Read in the American Communities Survey Data from 2015 ####

# ACS data obtained from:
# https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t
# Data was broken down regionally.

# Create vectors containing the states within each region, add a new variable to
# 'shootings' containing the appropriate region

# NE <- c("CT","ME","MA","NH","RI","VT",
#         "NJ","NY","PA")
# MW <- c("IL","IN","MI","OH","WI",
#         "IA","KS","MN","MS","NE", "ND","SD")
# So <- c("DE","FL","GA","MD","NC","SC","VA","DC","WV",
#         "AL","KY","MS","TN",
#         "AR","LA","OK","TX")
# We <-c("AZ","CO","ID","MT","NV","NM","UT", "WY",
#        "AK","CA","HI","OR","WA")
# 
# shootings$region <-
#   ifelse(shootings$state %in% NE,
#          'Northeast Region',
#          ifelse(shootings$state %in% MW,
#                 'Midwest Region',
#                 ifelse(shootings$state %in% So,
#                        'South Region',
#                        'West Region')))

#Reading in the ACS Data by state and looking at the social and economic files
ACS_Social <- read_csv('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP02_with_ann.csv',
                        na = c('','NA','(X)','N'),skip = 1)
ACS_Econ <- read_csv('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP03_with_ann.csv',
                     na = c('','NA','(X)','N'),skip = 1)

#The function below converts state names to abbreviations (add convert = T) and vice versa (convert = F))
library(datasets)
data(state)

#state to abbreviation conversion
abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}

#demo
abb2state('South Carolina', convert = T)

ACS_Social$Geography <- abb2state(ACS_Social$Geography, convert = T)
ACS_Econ$Geography <- abb2state(ACS_Econ$Geography, convert = T)

############################################################################################################
# ACS comes with a metadata CSV, listing all the included variables by name. 
# These files were amended in a text editor, indicating the files to keep with
# an 'x' in a new column, 'Keep'

# A Simple function reads these files, and returns the indicies of the variables to keep.

keeps <- function(file){
  temp <- read_csv(file)
  which(!is.na(temp$Keep))
}

Social_keeps <- keeps('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP02_metadata.csv')
Econ_keeps <- keeps('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP03_metadata.csv')


# Join Social and Economic features, by region. Simultaneously change
# 'Geography' to 'region' for merging with primary data set later.

ACS_combined <- inner_join(ACS_Social[,Social_keeps],
                           ACS_Econ[,Econ_keeps],
                           by ="Geography" ) %>%
  rename(state = Geography)

#convert state column to a factor
ACS_combined$state <- as.factor(ACS_combined$state)

# Join the ACS data with the shooting data (on state)
shootings_joined <- inner_join(shootings, ACS_combined,
                               by = 'state')

#get summary statistics for each of the variables in social and economic files 
#and calculate summary statistics on each of the percentages to get the bottom 25%, the middle 50% and the top 25%

#for the numeric variables in shootings_joined, segment the range of values into n bins
for (i in 19:ncol(shootings_joined)) {
  shootings_joined[[i]] <- ntile(shootings_joined[[i]], 4)
}

#filter shootings_joined df to limit to years 2015 and 2016
shootings_filter <- shootings_joined %>% filter(year <= 2016)

#aggregate states by number of shootings
state_shoot <- shootings_filter %>%
  group_by(state) %>%
  count() %>%
  arrange(desc(n))

cities <- shootings_filter %>%
  group_by(city, state) %>%
  count() %>%
  arrange(desc(n))

state_2sd <- (state_shoot %>% filter(n >= mean(n)+2*sd(n)))$state

#find the states with 2 standard deviations above the mean
shootings_filter <- shootings_filter %>% 
  mutate(outliers = state %in% state_2sd)

#########################################ASSOCIATION RULES ANALYSIS#################################
library(arules)

#convert entire df into factors
shootings_filter[,] <- lapply(shootings_filter, as.factor)

# Create the transactions
shooting_trans <- as(shootings_filter %>% select(
  -id, -name, -date, -age, -city), 'transactions')

#test <- apriori(shooting_trans, parameter = list(minlen =2,maxlen=20, target = 'rules'))
test <- apriori(shooting_trans, parameter = list(support = 0.15, confidence = 0.8, minlen =2, maxlen =90, target = 'rules'))
#restricting the right hand side to just the outliers
test_sub <- subset(test, subset = rhs %pin% 'outliers')
inspect(head(sort(test_sub, by = 'lift'), 20))
inspect(tail(test_sub, 20))

#will have to try different combinations of these of the above and also combine the other ACS data to this
#within states perhaps do a city-level analysis?
#within the outliers are there certain cities that have more police deaths

#Decision Trees
library(C50)
library(plyr)

#drop all cases of missing values
shootings_nomissing <- shootings_filter[complete.cases(shootings_filter),]

dt_shooting <- shootings_nomissing %>% select(-id, -name, -date, -age, -city, -state)
#have to account for the spaces in some of the column names to avoid a parsing error
colnames(dt_shooting) <- gsub(" ","",colnames(dt_shooting))
colnames(dt_shooting) <- gsub(";","",colnames(dt_shooting))
colnames(dt_shooting) <- gsub("-","",colnames(dt_shooting))

set.seed(12345)
dt_rand <- dt_shooting[order(runif(dim(dt_shooting)[1])), ]

#10-fold cross validation code
accuracy.c50 <- rep(NA, length(folds))
# form <- "outliers ~ manner_of_death + armed + gender + race + signs_of_mental_illness + threat_level + flee + body_camera + wday + month + year + age_bin +
# PercentVETERANSTATUSCivilianpopulation18yearsandoverCivilianveterans + PercentDISABILITYSTATUSOFTHECIVILIANNONINSTITUTIONALIZEDPOPULATIONTotalCivilianNoninstitutionalizedPopulationWithadisability +
# PercentPLACEOFBIRTHTotalpopulationNative + PercentPLACEOFBIRTHTotalpopulationForeignborn + PercentU.S.CITIZENSHIPSTATUSForeignbornpopulationNotaU.S.citizen + PercentEMPLOYMENTSTATUSPopulation16yearsandoverInlaborforce +
# PercentEMPLOYMENTSTATUSPopulation16yearsandoverInlaborforceCivilianlaborforceEmployed + PercentEMPLOYMENTSTATUSPopulation16yearsandoverInlaborforceCivilianlaborforceUnemployed +
# PercentPERCENTAGEOFFAMILIESANDPEOPLEWHOSEINCOMEINTHEPAST12MONTHSISBELOWTHEPOVERTYLEVELAllfamilies" 
folds <- split(dt_rand, cut(sample(1:nrow(dt_rand)),10))
for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  test <- test[, -1]
  train <- ldply(folds[-i], data.frame)
  train <- train[, -1]
  tmp.model <- C5.0(train[-22], train$outliers) #had to ignore column 18, 'veil type', because it has only 1 level
  #plot(tmp.model)
  tmp.predict <- predict(tmp.model, test, type='class')
  conf.mat <- table(test$outliers, tmp.predict)
  accuracy.c50[i] <- 100*sum(diag(conf.mat))/sum(conf.mat)
}
mean(accuracy.c50) #99.89011% 

#plot(tmp.model)


#############################BAYESIAN BELIEF NETWORKS########################################################
library(bnlearn)

#check to see which of the columns have missing values
colnames(shootings_filter)[colSums(is.na(shootings_filter)) > 0]

#drop all cases of missing values
shootings_nomissing <- shootings_filter[complete.cases(shootings_filter),]

#create the BNN
bnn <- hc(shootings_nomissing %>% select(-id, -name, -date, -age, -city), na.exclude(shootings_nomissing))

for(i in 1:dim(shootings_filter)[2]){
  shootings_nomissing[[i]] = factor(shootings_nomissing[[i]],levels = levels(shootings_nomissing[[i]]))
}

 




############################PREDICTIVE MODELING######################################################


shootings2015 <- shootings_joined[shootings_joined$year == 2015,]
shootings2015 <- shootings2015[, -c(20, 33, 34, 35, 42, 48, 54, 64, 66, 68, 70, 72, 74, 81, 84, 85, 92, 99)]
shootings2015 <- shootings2015[, -c(83, 87, 90, 94)]


#how to deal with the missing values

shootings2015$age_bin <- as.factor(shootings2015$age_bin)
shootings2015$age <- as.numeric(shootings2015$age)
shootings2015$armed <- as.factor(shootings2015$armed)
shootings2015$race <- as.factor(shootings2015$race)
sapply(colnames(shootings2015)[colSums(is.na(shootings2015)) > 0], class)

# shootings2015$armed <- addNA(shootings2015$armed)
# shootings2015$race <- addNA(shootings2015$race)
# shootings2015$flee <- addNA(shootings2015$age_bin)
# shootings2015$age <- addNA(shootings2015$age)

#removed all the missing values
shootings2015 <- shootings2015[!is.na(shootings2015$race),]
shootings2015 <- shootings2015[!is.na(shootings2015$age),]
shootings2015 <- shootings2015[!is.na(shootings2015$armed),]
shootings2015 <- shootings2015[!is.na(shootings2015$flee),]
shootings2015 <- shootings2015[!is.na(shootings2015$age_bin),]

#may need to try imputation

#CART decision tree model
library(rpart)
fit <- rpart(race ~ . -id -name -date -age -city -threat_level -flee -year -region, data=shootings2015,
             method="class")

library(randomForest)
shootings2015 <- subset(shootings2015, select = c(-id, -name, -date, -age, -city, -threat_level, -flee, -year, -region))
fit <- randomForest(as.factor(race) ~ .,
                    data=shootings2015, 
                    importance=TRUE, 
                    ntree=100)

#For association analysis would want to have the same left hand side to make a one-to-one comparison



