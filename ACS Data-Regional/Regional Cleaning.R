library(dplyr)

# Add Regions

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
             'Northeast',
             ifelse(shootings$state %in% MW,
             'Midwest',
             ifelse(shootings$state %in% So,
             'South',
             'West')))
  
