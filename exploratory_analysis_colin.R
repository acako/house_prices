library(dplyr)
library(readr)
library(finalfit)

data <- read.csv('houses_edited.csv')
summary(data)
#some NA's in sqft
#check for NA's in other columns
any(is.na(select(data,1:6,8:22)))
#no other NA's
#will need to impute sqft as 4500 is too many missing variables and sqft is important
missing_plot(data)

#check for randomness
complete <- names(select(data, 3:6,8,11,14,15,17:19))
missing <- names(select(data,7))

for (i in c(1:length(complete))) {
  print(data %>% missing_compare(missing, complete[i]))
}