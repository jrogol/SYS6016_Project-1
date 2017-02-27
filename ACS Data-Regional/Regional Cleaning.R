library(readr)
library(dplyr)
library(lubridate)
library(arules)


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
# Data was broken down by State

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

ACS_Housing <- read_csv('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP04_with_ann.csv',
                        na = c('','NA','(X)','N'),skip = 1)
ACS_Demo <- read_csv('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP05_with_ann.csv',
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
                rename(state_name = Geography)

# To Join The ACS data with the shootings data, we must pull in state names and
# abbreviations

# Function to create a list of State abbreviations and names to standardize
# between ACS and shootings data
statenames <- function(){
  data(state)
  state_names <- cbind(state.abb, state.name)
  state_names <- as.data.frame(rbind(state_names, c("DC","District of Columbia")))
  names(state_names) <- c("abb","state_name")
  state_names
}

states <- statenames()

# Join shootings with the state names
shootings <- inner_join(shootings, states,
                               by = c('state' = 'abb'))

# Filter for 2015 and 2016 only
shootings_filter <- shootings %>% filter(year <= 2016)


# Look at the number of deaths, aggregated by state
state_count <- shootings_filter %>%
  group_by(state) %>% 
  count() %>% 
  arrange(desc(n))

# Look for states whose cumulative total is more than 2 standard deviations
# above the state mean
state_count %>% 
  filter(n >= mean(n)+2*sd(n))

# Create a new variable separating CA and TX from other states!
two_sd <- (state_count %>% 
  filter(n >= mean(n)+2*sd(n)))$state

shootings_filter <- shootings_filter %>% 
  mutate(outliers = state %in% two_sd)


#### Prep for arules ####

# Turn everything into factors
shootings_filter[,] <- lapply(shootings_filter[,], as.factor)

# Create the transactions
shooting_trans <- as(shootings_filter %>% select(
  -id, -name, -date, -state_name, -age, -city, -state, -region), 'transactions')


# Create Some rules!
test <- apriori(shooting_trans, parameter = list(supp = .01, conf = .25,
                                         minlen =2,maxlen=90, target = 'rules'))


test_sub <- subset(test, subset = rhs %pin% 'outliers' & lhs %pin% 'age')

inspect(head(sort(test_sub, by = 'confidence')))
inspect(test_sub)









#### Use Decision Tree to Predict XXXX ####

library(C50)
shooting_tree <- C5.0.formula(c(race, gender)~, shootings_joined,
                              subset = which(shootings_joined$year == 2015),
                              rules = T)

shooting_tree
summary(shooting_tree)
