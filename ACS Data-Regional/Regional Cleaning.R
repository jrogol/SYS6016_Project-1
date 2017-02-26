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

#### Read in the American Communities Survey Data from 2015 ####

# ACS data obtained from:
# https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t
# Data was broken down regionally.

# Create vectors containing the states within each region, add a new variable to
# 'shootings' containing the appropriate region

NE <- c("CT","ME","MA","NH","RI","VT",
        "NJ","NY","PA")
MW <- c("IL","IN","MI","OH","WI",
        "IA","KS","MN","MS","NE", "ND","SD")
So <- c("DE","FL","GA","MD","NC","SC","VA","DC","WV",
        "AL","KY","MS","TN",
        "AR","LA","OK","TX")
We <-c("AZ","CO","ID","MT","NV","NM","UT", "WY",
       "AK","CA","HI","OR","WA")

shootings$region <-
           ifelse(shootings$state %in% NE,
             'Northeast Region',
             ifelse(shootings$state %in% MW,
             'Midwest Region',
             ifelse(shootings$state %in% So,
             'South Region',
             'West Region')))

# Read in the 'Housing' and 'Housing and Demographic' ACS data

ACS_Housing <- read_csv('ACS Data-Regional/ACS_15_1YR_DP04_with_ann.csv',
                        na = c('','NA','(X)','N'),skip = 1)
ACS_Demo <- read_csv('ACS Data-Regional/ACS_15_1YR_DP05_with_ann.csv',
                     na = c('','NA','(X)','N'),skip = 1)

# ACS comes with a metadata CSV, listing all the included variables by name. 
# These files were amended in a text editor, indicating the files to keep with
# an 'x' in a new column, 'Keep'

# A Simple function reads these files, and returns the indicies of the variables to keep.

keeps <- function(file,n=-1){
  temp <- read_csv(file, n_max=n)
  which(!is.na(temp$Keep))
}

Housing_keeps <- keeps('ACS Data-Regional/ACS_15_1YR_DP04_metadata.csv')
Demo_keeps <- keeps('ACS Data-Regional/ACS_15_1YR_DP05_metadata.csv')


# Join Housing and Demographic features, by region. Simultaneously change
# 'Geography' to 'region' for merging with primary data set later.

ACS_combined <- inner_join(ACS_Housing[,Housing_keeps],
                           ACS_Demo[,Demo_keeps],
                           by = 'Geography') %>%
                rename(region = Geography)

# Join the ACS data with the shooting data (on region)

shootings_joined <- inner_join(shootings, ACS_combined,
                               by = 'region')

#### Use Decision Tree to Predict XXXX ####

library(C50)
shooting_tree <- C5.0.formula(c(race, gender)~, shootings_joined,
                              subset = which(shootings_joined$year == 2015),
                              rules = T)

shooting_tree
summary(shooting_tree)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))