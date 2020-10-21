library(dplyr)
library(readr)
library(finalfit)
library(ggplot2)
library(grid)
library(gridExtra)

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

#add a column for total bedrooms
data <- data %>% mutate(beds = bedrooms_ag+bedrooms_bg)

#missing data seems to be strongly correlated to type of house and the price
theme_set(theme_minimal())
# check for distribution of numerical variables and outliers
#final price
h1 <- data %>% ggplot(aes(x=final_price/1000)) + geom_histogram(fill='grey', colour='black', alpha = 0.6, binwidth = 100) + xlab("Final Price in thousands") + xlim(100,5000)
#list price
h2 <- data %>% ggplot(aes(x=list_price/1000)) + geom_histogram(fill='grey', colour='black', alpha = 0.6, binwidth = 100) + xlab("List Price in thousands") + xlim(100,5000)
#bedrooms
h3 <- data %>% ggplot(aes(x=beds)) + geom_histogram(fill='grey', colour='black', alpha = 0.6, binwidth = 1) + xlab("Bedrooms") + scale_x_continuous(breaks = c(1:10))
#bathrooms
h4<- data %>% ggplot(aes(x=bathrooms)) + geom_histogram(fill='grey', colour='black', alpha = 0.6, binwidth = 1) + xlab("Bathrooms") + scale_x_continuous(breaks = c(1:10))
#sqft
h5 <- data %>% ggplot(aes(x=sqft)) + geom_histogram(fill='grey', colour='black', alpha = 0.6, binwidth = 100) + xlab("Square Footage")
#parking
h6 <- data %>% ggplot(aes(x=parking)) + geom_histogram(fill='grey', colour='black', alpha = 0.6, binwidth=1) + xlab("Parking") + scale_x_continuous(breaks = c(1:12))
#latitude
h7 <- data %>% ggplot(aes(x=lat)) + geom_histogram(fill='grey', colour='black', alpha = 0.6, binwidth = 0.01) + xlab("Latitude")
#longitude
h8 <- data %>% ggplot(aes(x=long)) + geom_histogram(fill='grey', colour='black', alpha = 0.6, binwidth = 0.01) + xlab("Longitude")
#mean district income
h9 <- data %>% ggplot(aes(x=mean_district_income/1000)) + geom_histogram(fill='grey', colour='black', alpha = 0.6, binwidth = 10) + xlab("Mean District Income")

grid.arrange(h1,h2,h3,h4,h5,h6,h7,h8,h9, ncol=3, nrow=3)

#distribution of categorical variables
b1 <- data %>% ggplot(aes(x=type)) + geom_bar(fill='grey',colour='black', alpha = 0.6) + theme(axis.text.x = element_text(angle = 90))
# district (need to fix this)
b2 <- data %>% ggplot(aes(x=city_district)) + geom_bar(fill='grey',colour='black', alpha = 0.6) + theme(axis.text.x = element_text(angle = 90))

# plot showing outliers
s1 <- data %>% ggplot(aes(x=final_price/1000,y=list_price/1000)) + geom_point(alpha=0.5)
# 5 houses that are clear outliers on price

s2 <- data %>% ggplot(aes(x=final_price/1000,y=sqft)) + geom_point(alpha=0.5)
# no real outliers for sqft
