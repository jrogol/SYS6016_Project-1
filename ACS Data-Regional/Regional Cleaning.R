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

shootings$region <- as.factor(shootings$region)
  
# Read in the 'Housing' and 'Housing and Demographic' ACS data

ACS_Housing <- read_csv('ACS Data-Regional/ACS_15_1YR_DP04_with_ann.csv',
                        na = c('','NA','(X)','N'),skip = 1)
ACS_Demo <- read_csv('ACS Data-Regional/ACS_15_1YR_DP05_with_ann.csv',
                     na = c('','NA','(X)','N'),skip = 1)

# Create vectors of the features to keep for each set
Housing_keeps <- c(3,4,8,12,180,292,296,300,316,352,356,360,368,400,
                   404,500,532,536)

Demo_keeps <- c(3,4,8,12,68,128,132,136,156,188,236,240,244,248,252,256,260)

# Join Housing and Demographic features, by region. Simultaneously change
# 'Geography' to 'region' for merging with primary data set later.

ACS_combined <- inner_join(ACS_Housing[,Housing_keeps],
                           ACS_Demo[,Demo_keeps],
                           by = 'Geography') %>%
                rename(region = Geography)

