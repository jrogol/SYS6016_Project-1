#Looking at files 2 (Social) and 3 (Economic) from ACS
library(readr)
library(dplyr)
library(lubridate)
library(datasets)
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

#collapse armed factor levels into armed, not armed and undermined
armed_levels <- as.list(levels(shootings$armed))
armed_levels[[61]] <- NULL #removing unarmed
armed_levels[[61]] <- NULL #removing undetermined
levels(shootings$armed)[levels(shootings$armed)%in%c(armed_levels)] <- "armed" 


#### Read in the American Communities Survey Data from 2015 ####
#Reading in the ACS Data by state and looking at the social and economic files
ACS_Social <- read_csv('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP02_with_ann.csv',
                        na = c('','NA','(X)','N'),skip = 1)
ACS_Econ <- read_csv('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP03_with_ann.csv',
                     na = c('','NA','(X)','N'),skip = 1)

#The function below converts state names to abbreviations (add convert = T) and vice versa (convert = F))
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

#creating a state list with the names and abbreviations
state = list()
state[['name']] = c(state.name,"District Of Columbia")
state[['abb']] = c(state.abb,"DC")

#creating a dataframe of state names
state_name <- as.data.frame(unclass(state[1]))
colnames(state_name) <- c('state')

#creating a dataframe of state abbreviations
state_abb <- as.data.frame(unclass(state[2]))
colnames(state_abb) <- c('state')

#converting state names in Social and Economic fiels to abbreviations
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

#selecting the variables of interest from Social and Economic Files
ACS_S <- ACS_Social[,Social_keeps] %>% rename(state = Geography)
ACS_S$state <- as.factor(ACS_S$state)
ACS_E <- ACS_Econ[,Econ_keeps] %>% rename(state = Geography)
ACS_E$state <- as.factor(ACS_E$state)

#combine Social with shootings by state
shootings_Social <- inner_join(shootings, ACS_S, by = "state")

#remove the population total estimate column (was kept for purposes later on)
shootings_Social <- shootings_Social[, -21]

#for numeric variables in shootings_Social, segment the values into n bins
for (i in 19:ncol(shootings_Social)) {
  shootings_Social[[i]] <- ntile(shootings_Social[[i]], 4)
}

#filter shootings_Social to years 2015 and 2016
Social_filter <- shootings_Social %>% filter(year <= 2016)

#combine Economic with shootings by state
shootings_Econ <- inner_join(shootings, ACS_E, by = "state")

#for numeric variables in shootings_Econ, segment the values into n bins
for (i in 19:ncol(shootings_Econ)) {
  shootings_Econ[[i]] <- ntile(shootings_Econ[[i]], 4)
}

#filter shootings_Econ to years 2015 and 2016
Econ_filter <- shootings_Econ %>% filter(year <= 2016)

# Join Social and Economic features, by state.
ACS_combined <- inner_join(ACS_S,
                           ACS_E,
                           by ="state" )

# Join the ACS combined data with the shooting data (on state)
shootings_joined <- inner_join(shootings, ACS_combined,
                               by = 'state')

#remove the total population estimate column
shootings_joined <- shootings_joined[,-21]

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

# Create the state populations (from the ACS_S df with total population estimate), joined with the state abbreviations in
# preparation for an additional join
state_pop <- inner_join(state_abb, 
                        ACS_S[,c('Estimate; PLACE OF BIRTH - Total population','state')],
                        by = "state") %>%
  rename(pop =`Estimate; PLACE OF BIRTH - Total population`) %>%
  select(pop, state)

# Join the state populations and create a new feature "shootings per milion
# residents"
state_shoot <- inner_join(state_shoot, state_pop, by = "state") %>% 
  mutate(adjCount = n*1000000/pop) %>% 
  arrange(desc(adjCount))

#creating a new variable that looks for states with population counts two standard deviations above the mean
#find that NM, AK, OK are two std devs above mean
state_2sd <- (state_shoot %>% filter(adjCount >= mean(adjCount)+2*sd(adjCount)))$state

#find the states with 2 standard deviations above the mean
shootings_filter <- shootings_filter %>% 
  mutate(outliers = state %in% state_2sd)

#find the states with 2 standard deviations above the mean for Social_filter and Econ_filter dfs
Social_filter <- Social_filter %>% mutate(outliers = state %in% state_2sd)
Econ_filter <- Econ_filter %>% mutate(outliers = state %in% state_2sd)

#########################################ASSOCIATION RULES ANALYSIS#################################

#### Rules for the ACS Social Set

#conversion to all factors
Social_filter[,] <- lapply(Social_filter, as.factor)

# Create the transactions
Social_trans <- as(Social_filter %>% select(
  -id, -name, -date, -age, -city, -state), 'transactions')

#limiting support to 0.2
test <- apriori(Social_trans, parameter = list(supp = .2, conf = .01,
                                              minlen =2,maxlen=90,
                                              target = 'rules',
                                              originalSupport = F, ext = T))
# Top rules for Outliers, by lift
test_sub <- subset(test, subset = rhs %in% 'outliers=TRUE')
inspect(head(sort(test_sub, by = 'lift')))

# Rules for non-Outliers, containing the LHS above
test_sub2 <- subset(test, subset = lhs %ain% c(
  'armed=armed', 'gender=M', 'Percent; VETERAN STATUS - Civilian population 18 years and over - Civilian veterans=4') &
    rhs %in% 'outliers=FALSE')

inspect(head(sort(test_sub2, by = 'lift')))

#limiting support to 0.1
test <- apriori(Social_trans, parameter = list(supp = .1, conf = .01,
                                              minlen =2,maxlen=90,
                                              target = 'rules',
                                              originalSupport = F, ext = T))
# Top rules for Outliers, by lift
test_sub <- subset(test, subset = rhs %in% 'outliers=TRUE')
inspect(head(sort(test_sub, by = 'lift')))

# Rules for non-Outliers, containing the LHS above
test_sub2 <- subset(test, subset = lhs %ain% c(
'gender=M', 
'Percent; VETERAN STATUS - Civilian population 18 years and over - Civilian veterans=4', 
'Percent; DISABILITY STATUS OF THE CIVILIAN NONINSTITUTIONALIZED POPULATION - Total Civilian Noninstitutionalized Population - With a disability=4') &
    rhs %in% 'outliers=FALSE')

inspect(head(sort(test_sub2, by = 'lift')))

######## Rules for the ACS Economic Set #############

#conversion to all factors
Econ_filter[,] <- lapply(Econ_filter, as.factor)

# Create the transactions
Econ_trans <- as(Econ_filter %>% select(
  -id, -name, -date, -age, -city, -state), 'transactions')

#limiting support to 0.1
test <- apriori(Econ_trans, parameter = list(supp = 0.1, conf = .01,
                                               minlen =2,maxlen=90,
                                               target = 'rules',
                                               originalSupport = F, ext = T))

# Top rules for Outliers, by lift
test_sub <- subset(test, subset = rhs %in% 'outliers=TRUE')
inspect(head(sort(test_sub, by = 'lift')))

# Rules for non-Outliers, containing the LHS above
test_sub2 <- subset(test, subset = lhs %ain% c(
  'gender=M', 
  'Percent; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - No health insurance coverage=4', 
  'Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families=3') &
    rhs %in% 'outliers=FALSE')

inspect(head(sort(test_sub2, by = 'lift')))

####Rules for ACS Combined Set####
#convert entire df into factors
shootings_filter[,] <- lapply(shootings_filter, as.factor)

# Create the transactions
shooting_trans <- as(shootings_filter %>% select(
  -id, -name, -date, -age, -city, -state), 'transactions')

# limiting support to 0.1
test <- apriori(shooting_trans, parameter = list(supp = .1, conf = .01,
                                                minlen =2,maxlen=90,
                                                target = 'rules',
                                                originalSupport = F, ext = T))
test_sub <- subset(test, subset = rhs %in% 'outliers=TRUE')

inspect(head(sort(test_sub, by = 'lift')))

test_sub2 <- subset(test, subset = lhs %ain% c('gender=M',
                                               'Percent; VETERAN STATUS - Civilian population 18 years and over - Civilian veterans=4',
                                               'Percent; DISABILITY STATUS OF THE CIVILIAN NONINSTITUTIONALIZED POPULATION - Total Civilian Noninstitutionalized Population - With a disability=4') &
                                                rhs %in% 'outliers=FALSE')

inspect(head(sort(test_sub2, by = 'lift')))
