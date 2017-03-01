library(readr)
library(dplyr)
library(lubridate)
library(arules)


#### Read in the Initial Data ####

# Read in the WaPo fatal shootings data
# From: https://github.com/washingtonpost/data-police-shootings
shootings <- read_csv('https://github.com/washingtonpost/data-police-shootings/raw/master/fatal-police-shootings-data.csv')
# last accessed 3/1/17 at 6:39 PM EST

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

# Bin armed into: armed, not armed and undetermined
armed_levels <- as.list(levels(shootings$armed))
armed_levels[[61]] <- NULL #removing unarmed
armed_levels[[61]] <- NULL #removing undetermined
levels(shootings$armed)[levels(shootings$armed)%in%c(armed_levels)] <- "armed" #collapsed the factors into either armed or unarmed

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

# Here's a function which bins stats in ACS files by quantile. It takes a data
# frame and the vector of features to keep as arguments.

quants <- function(df, keep){
  # require dplyr - it has the 'ntile' function
  require(dplyr)
  # Subset the given data frame using the 'keep' vector
  temp <- df[,keep]
  # ignoring the first column, which contains the geography, replace the values
  # within each column with the quantile calculated for each column
  for (i in 2:ncol(temp)){
    temp[[i]] <- ntile(temp[[i]],4)
  }
  # return the newly calculated data
  temp
}



# Create filtered data sets for both ACS files, and combine them.
# Join Housing and Demographic features, by state. Simultaneously change
# 'Geography' to 'state_name' for merging with primary data set later.

ACS_Housing_filtered <- quants(ACS_Housing,Housing_keeps) %>%
  rename(state_name = Geography)

ACS_Demo_filtered <- quants(ACS_Demo,Demo_keeps) %>%
  rename(state_name = Geography)


ACS_combined <- inner_join(quants(ACS_Housing,Housing_keeps),
                           quants(ACS_Demo,Demo_keeps),
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

# California and Texas are also two of the most populous sttes in the union. Thus, 
# the counts should be normalized. In this case, the fourth column of ACS_Demo
# (Estimate; SEX AND AGE - Total population) contains estimates for state
# population. The per-state count will be divided by this number and multiplied
# by 1 million in order to see the number of shootings per million occupants in
# the state.

# Create the state populations, joined with the state abbriviations in
# preparation for an additional join
state_pop <- inner_join(states, 
                        ACS_Demo[,c('Estimate; SEX AND AGE - Total population','Geography')],
                        by = c("state_name" = 'Geography')) %>%
  rename(pop =`Estimate; SEX AND AGE - Total population`, state = abb) %>%
  select(pop, state)

# Join the state populations and create a new feature "shootings per milion
# residents"
state_count <- inner_join(state_count, state_pop) %>% 
  mutate(adjCount = n*1000000/pop) %>% 
  arrange(desc(adjCount))

# Look for states whose cumulative total is more than 2 standard deviations
# above the new feature's mean
state_count %>% 
  filter(adjCount >= mean(adjCount)+2*sd(adjCount))

# This yeilds three states: New Mexico, Alaska and Oklahoma

# Create a new variable separating NM, AK and OK  from other states!
two_sd <- (state_count %>% 
             filter(adjCount >= mean(adjCount)+2*sd(adjCount)))$state

shootings_filter <- shootings_filter %>% 
  mutate(outliers = state %in% two_sd)


#### Prep for arules ####

# Turn everything into factors
shootings_filter[,] <- lapply(shootings_filter[,], as.factor)

# Create the transactions for the shootings data set only.
shooting_trans <- as(shootings_filter %>% select(
  -id, -name, -date, -state_name, -age, -city, -state, -region), 'transactions')


# Create Some rules!
test <- apriori(shooting_trans, parameter = list(supp = .01, conf = .25,
                                         minlen =2,maxlen=90, target = 'rules'))


test_sub <- subset(test, subset = rhs %pin% 'outliers' & lhs %pin% 'body_')

inspect(head(sort(test_sub, by = 'lift')))
inspect(test_sub)



ACS_combined2[,] <- lapply(ACS_combined2[,], as.factor)

ACS_trans <- as(inner_join(ACS_combined2, shootings_filter )%>% select(
  -id, -name, -date, -state_name, -age, -city, -state, -region), 'transactions')

test <- apriori(ACS_trans, parameter = list(#supp = .01, conf = .25,
                                                 minlen =2,maxlen=90, maxtime = 90,
                                                 target = 'rules'))
test_sub <- subset(test, subset = rhs %pin% 'outliers')

inspect(head(sort(test_sub, by = 'lift')))
inspect(test_sub)





#### Use Decision Tree to Predict XXXX ####

library(C50)
shooting_tree <- C5.0.formula(c(race, gender)~, shootings_joined,
                              subset = which(shootings_joined$year == 2015),
                              rules = T)

shooting_tree
summary(shooting_tree)
