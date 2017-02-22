library(readr)
library(dplyr)
library(lubridate)

# From: https://github.com/washingtonpost/data-police-shootings
shootings2 <- read_csv('https://github.com/washingtonpost/data-police-shootings/raw/master/fatal-police-shootings-data.csv')


# Columns to factor
factors <- c(4,5,7:14)

shootings[, factors] <- lapply(shootings[, factors], as.factor)

# Extract Weekday, Month and Year from date.
shootings <- shootings %>%
        mutate(wday = wday(date, label = T), 
               month = month(date, label =T),
               year = year(date))

shootings %>% unique(city, state)

# Bin armed: firearms, pointy, blunt, other
# Bin ages:
# Flee == 'other', categorize as fleeing!
# Geographic options - Geographic regions (NE, SW, MW, NW, etc.)
# Geographic options - Red/Blue state
# Group cities by population size, majority demo, average income

cities <- shootings %>%
  group_by(state, city) %>%
  count() %>%
  arrange(desc(n))

cities[cities$n >1,] # 226 with more than 1
cities[cities$n >5,] # 46 with more than 1

# If shot, carrying a firearm and attacking - what other charachteristics exist?
# Could they theoretically have been avoided? -- Hypothesis


# Is there a relationship between victims in the top 10 cities?

# Bayesian networks; Trees