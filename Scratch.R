ACS_housing_state <- read_csv('ACS Data-Regional/ACS_byState/ACS_15_1YR_DP02_with_ann.csv',
                              na = c('','NA','(X)','N'),skip = 1)
ACS_housing_state <- inner_join(states, ACS_housing_state, by = c('state_name' = 'Geography')) 
head(ACS_housing_state[,20:30])



summary(ACS_housing_state[,10:20])



ACS_bin <- function(df,q=c(0,.25,.5, .75,1)){
  quantile(i,q)
}

xx <- quantile(df[[i]])

xx['0%']

namez <- ACS_Demo[,Demo_keeps]


quants <- function(df, keep){
  require(dplyr)
  temp <- df[,keep]
  for (i in 2:ncol(temp)){
    temp[[i]] <- ntile(temp[[i]],4)
  }
  temp
}


mu <- mean(temp[[i]])
MI <- min(temp[[i]])
MA <- max(temp[[i]])
plus_2sd <- mu +2*sd(temp[[i]])
minus_2sd <- mu - 2*sd(temp[[i]])

x <- c(MI,minus_2sd,mu,plus_2sd,MA)

temp <- ACS_Demo[,Demo_keeps]
test <- quants(ACS_Demo,Demo_keeps)
