library(readr)
library(dplyr)
library(lubridate)

# From: https://github.com/washingtonpost/data-police-shootings
shootings <- read_csv('https://github.com/washingtonpost/data-police-shootings/raw/master/fatal-police-shootings-data.csv')


# Columns to factor
factors <- c(4,5,7:14)

shootings[, factors] <- lapply(shootings[, factors], as.factor)

# Aggregate armed into broader categories?

# Extract Weekday, Month and Year from date.
shootings <- shootings %>%
        mutate(wday = wday(date, label = T), 
               month = month(date, label =T),
               year = year(date))

shootings %>% unique(city, state)

