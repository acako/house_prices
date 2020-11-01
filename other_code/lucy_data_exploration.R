library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(tidyr)
library(caret)
library(DT)
library(scales)
library(qwraps2)

# define the markup language we are working in.
# options(qwraps2_markup = "latex") is also supported.
options(qwraps2_markup = "markdown")


#presetting
#colors = c(""#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"")

#import the source dataset
house_price <- read.csv("houses_edited.csv")
#delete the ID column
house_price_ID <- house_price$index
house_price$index <- NULL
dim(house_price)
###display first 10 variables and the response variable

summary(house_price)

#plot - price distrubution
ggplot(data=house_price, aes(x=list_price)) +
  geom_histogram(fill="blue", binwidth = 100000) +
  scale_x_continuous(breaks= seq(0, 14000000, by=2000000), labels = comma)
ggplot(data=house_price, aes(x=final_price)) +
  geom_histogram(fill="orange", binwidth = 100000) +
  scale_x_continuous(breaks= seq(0, 14000000, by=2000000), labels = comma)
ggplot(data=house_price, aes(x=list_price)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth = 100000)+
  geom_density(alpha=.1, fill="#FF6666") +
  scale_x_continuous(breaks= seq(0, 14000000, by=2000000), labels = comma)
summary(house_price$list_price)

#mean district income distrubution 
ggplot(data=house_price, aes(x=mean_district_income)) +
  geom_histogram(fill="orange", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 300000, by=50000), labels = comma)

#bar plot 
#relationship with final price
ggplot(data=house_price, aes(x=bathrooms, y = final_price)) +
  geom_bar(stat="identity")
ggplot(data=house_price, aes(x=sqft, y = final_price)) +
  geom_bar(stat="identity")
ggplot(data=house_price, aes(x=parking, y = final_price)) +
  geom_bar(stat="identity")
ggplot(data=house_price, aes(x=bedrooms_ag, y = final_price)) +
  geom_bar(stat="identity")
ggplot(data=house_price, aes(x=bedrooms_bg, y = final_price)) +
  geom_bar(stat="identity")



#select columns
numericVars <- which(sapply(house_price, is.numeric))
all_numVar <- house_price[, numericVars]
house_price_numeric <- all_numVar %>% select(3:13)
#select only complete cases
complete_cases <- house_price_numeric[complete.cases(house_price_numeric), ]
dim(complete_cases)
summary(complete_cases)
#correlation with all selected colunms
cormat <- round(cor(complete_cases),1)
head(cormat)
p.mat <- cor_pmat(complete_cases)
ggcorrplot(cormat, hc.order = TRUE,
           type = "lower", p.mat = p.mat,lab = TRUE)

#sort on decreasing correlations with final price
#select and show only high correlations
cor_sorted <- as.matrix(sort(cormat[,'final_price_transformed'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.3)))
cormat2 <- cormat[CorHigh, CorHigh]
corrplot.mixed(cormat2, tl.col="black", tl.pos = "lt")


#scatter plot
# Convert column from a numeric to a factor variable
complete_cases$bathrooms <- as.factor(complete_cases$bathrooms)
complete_cases$sqft <- as.factor(complete_cases$sqft)
complete_cases$parking <- as.factor(complete_cases$parking)
complete_cases$mean_district_income <- as.factor(complete_cases$mean_district_income)
complete_cases$bedrooms_ag <- as.factor(complete_cases$bedrooms_ag)
complete_cases$bedrooms_bg <- as.factor(complete_cases$bedrooms_bg)

ggplot(complete_cases, aes(x=bathrooms, y=final_price_log)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(complete_cases, aes(x=sqft, y=final_price_transformed)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(complete_cases, aes(x=parking, y=final_price_transformed)) + 
  geom_point()+
  geom_smooth(method=lm)




#data normalization
preproc <- preProcess(complete_cases, method=c("range"))
complete_cases_norm <- predict(preproc, complete_cases)
summary(complete_cases_norm)
ggplot(data=complete_cases_norm, aes(x=sqft)) +
  geom_histogram(fill="blue", binwidth = 0.1)
ggplot(data=complete_cases_norm, aes(x=mean_district_income)) +
  geom_histogram(fill="blue", binwidth = 0.05)

ggplot(data=complete_cases_norm, aes(x=mean_district_income)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")




  