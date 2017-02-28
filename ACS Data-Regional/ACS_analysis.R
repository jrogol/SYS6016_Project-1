#Looking at files 2 (Social) and 3 (Economic)
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

#get summary statistics for each of the variables in social and economic files 
#and calculate summary statistics on each of the percentages to get the bottom 25%, the middle 50% and the top 25%

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

#Convert the state names to abbreviations
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

ACS_Social$State <- abb2state(ACS_Social$Geography, convert = T)
ACS_Econ$State <- abb2state(ACS_Econ$Geography, convert = T)

#use this function down here to isolate the outliers of the states with the highest shootings
cities <- shootings %>%
  group_by(state, city) %>%
  count() %>%
  arrange(desc(n))

summary(cities)
plot(cities)
outliers <- c('CA', 'TX')

# ACS comes with a metadata CSV, listing all the included variables by name. 
# These files were amended in a text editor, indicating the files to keep with
# an 'x' in a new column, 'Keep'

# A Simple function reads these files, and returns the indicies of the variables to keep.

keeps <- function(file){
  temp <- read_csv(file)
  which(!is.na(temp$Keep))
}

Social_keeps <- keeps('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP02_metadata.csv') #have to go back and edit this
Econ_keeps <- keeps('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP03_metadata.csv')


# Join Social and Economic features, by region. Simultaneously change
# 'Geography' to 'region' for merging with primary data set later.

ACS_combined <- inner_join(ACS_Social[,Social_keeps],
                           ACS_Econ[,Econ_keeps],
                           by ="Geography" ) %>%
  rename(region = Geography)

# Join the ACS data with the shooting data (on region)

shootings_joined <- inner_join(shootings, ACS_combined,
                               by = 'region')




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



