#load csv file
house_prices <- read.csv(file="~/Desktop/CML/assignment_2/houses_edited.csv", head=TRUE)

#explore variables to see type
str(house_prices)
#create a new column including all bedrooms to see if there is missing values
house_prices$bedrooms_total <- house_prices$bedrooms_ag + house_prices$bedrooms_bg

#impute data for sqft
#loading mice package
#checking at missing data using mice
library(mice)
md.pattern(house_prices)

#observation of missing data using VIM
library(VIM)
missing_data_plot <- aggr(house_prices, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(house_prices), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

house_prices_updated <- house_prices[-c(1, 2, 5, 9, 10, 12, 13, 21, 22)]

#combining the similar house types 
house_prices_updated$type[house_prices_updated$type=='Att/Row/Twnhouse'] <- 'Townhouse'
house_prices_updated$type[house_prices_updated$type=='Co-Op Apt'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Co-Ownership Apt'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Comm Element Condo'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Condo Apt'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Condo Townhouse'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Link'] <- 'Detached'
house_prices_updated$type[house_prices_updated$type=='Store W/Apt/Offc'] <- 'Condo'

head(house_prices_updated)

#imputation of sqft 

#imputing using the random sample in mice
#cl_houseprices_randomsample<- mice(house_prices_updated,m=5,maxit=50,meth='sample', seed = 500)
summary(tempData)
#imputing using the random forest in mice
#cl_houseprices_randomforest <- mice(house_prices_updated,m=5,maxit=50,meth='rf', seed = 500)
#imputing using cart in mice
cl_houseprices_cart <- mice(house_prices_updated,m=5,maxit=50,meth='cart', seed = 500)

#looking at summary of each imputed data
summary(cl_houseprices_randomsample)
summary(cl_houseprices_randomforest)
summary(cl_houseprices_cart)


#looking at density plot for each imputed data method
densityplot(cl_houseprices_randomforest)
densityplot(cl_houseprices_cart)
densityplot(cl_houseprices_randomsample)

complete_data_1 <- complete(cl_houseprices_cart, 1)
complete_data_2 <- complete(cl_houseprices_cart, 2)
complete_data_3 <- complete(cl_houseprices_cart, 3)
complete_data_4 <- complete(cl_houseprices_cart, 4)
complete_data_5 <- complete(cl_houseprices_cart, 5)

head(complete_data_1)
head(complete_data_2)
head(complete_data_3)
head(complete_data_4)
head(complete_data_5)

#convert type column to factor
complete_data_1$type <- as.factor(complete_data_1$type)
complete_data_2$type <- as.factor(complete_data_2$type)
complete_data_3$type <- as.factor(complete_data_3$type)
complete_data_4$type <- as.factor(complete_data_4$type)
complete_data_5$type <- as.factor(complete_data_5$type)
sapply(complete_data_1 , class)
head(complete_data_1)
#removing final price log/transformed, city district and list price
complete_d1 <- subset(complete_data_1, select=-c(city_district, list_price))
head(complete_d1)
complete_d_1 <- subset(complete_d1, select=-c(final_price_log, final_price_transformed))

complete_d2 <- subset(complete_data_2, select=-c(city_district, list_price))
complete_d_2 <- subset(complete_d2, select=-c(final_price_log, final_price_transformed))

complete_d3 <- subset(complete_data_3, select=-c(city_district, list_price))
head(complete_d3)
complete_d_3 <- subset(complete_d3, select=-c(final_price_log, final_price_transformed))

complete_d4 <- subset(complete_data_4, select=-c(city_district, list_price))
head(complete_d4)
complete_d_4 <- subset(complete_d4, select=-c(final_price_log, final_price_transformed))

complete_d5 <- subset(complete_data_5, select=-c(city_district, list_price))
head(complete_d5)
complete_d_5 <- subset(complete_d1, select=-c(final_price_log, final_price_transformed))

#removing outliers on final price
library(dplyr)
complete_d_1 <- complete_d_1 %>% subset(final_price<7500000)
complete_d_2 <- complete_d_2 %>% subset(final_price<7500000)
complete_d_3 <- complete_d_3 %>% subset(final_price<7500000)
complete_d_4 <- complete_d_4 %>% subset(final_price<7500000)
complete_d_5 <- complete_d_5 %>% subset(final_price<7500000)
head(complete_d_1)
#put data sets in list -ignore for now
#data_sets_all = list(complete_d_1, complete_d_2, complete_d_3, complete_d_4, complete_d_5)

library(caret)
library(rpart)
install.packages("xgboost")
library(xgboost)
install.packages("tidyverse")
library(tidyverse)
model_final_price <- train(final_price ~.,data=complete_d_1,trControl = trainControl("cv",number=10,savePredictions = 'all'),
                                   method='xgbTree')










