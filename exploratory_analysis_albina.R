#load csv file
house_prices <- read.csv(file="~/Desktop/CML/assignment_2/houses_edited.csv", head=TRUE)

#explore variables to see type
str(house_prices)

#explore variable distribution and missing values
summary(house_prices)
#create a new column including all bedrooms to see if there is missing values
house_prices$bedrooms_total <- house_prices$bedrooms_ag + house_prices$bedrooms_bg
#check to see if missing values in bedroom column
head(house_prices)
summary(house_prices)

#check for distribution of price - target variable
hist(house_prices$list_price)
hist(house_prices$final_price)

#check for distribution of other numerical features
hist(house_prices$bathrooms)
hist(house_prices$bedrooms)
hist(house_prices$sqft)
hist(house_prices$parking)
hist(house_prices$mean_district_income)

install.packages("ggplot2")

library(ggplot2)
library(dplyr)
#plotting 2d graphs to see relationship between independent variables and target variable
ggplot(house_prices, aes(mean_district_income, final_price), na.rm = TRUE) + geom_point()
ggplot(house_prices, aes(list_price, final_price), na.rm = TRUE) + geom_point()
ggplot(house_prices, aes(bathrooms, final_price), na.rm = TRUE) + geom_point()
ggplot(house_prices, aes(bedrooms_total, final_price), na.rm = TRUE) + geom_point()
ggplot(house_prices, aes(bedrooms_ag,final_price), na.rm = TRUE) + geom_point()
ggplot(house_prices, aes(bedrooms_bg, final_price), na.rm = TRUE) + geom_point()
ggplot(house_prices, aes(sqft, final_price), na.rm = TRUE) + geom_point()
ggplot(house_prices, aes(parking, final_price), na.rm = TRUE) + geom_point()
#look out for missing values - 4521 missing sqft values
sort(colSums(is.na(house_prices)), decreasing = T)

#correlation 
library(corrplot)

#select numeric features
house_prices_num <- select_if(house_prices, is.numeric)
head(house_prices_num)
#chosing a dataset to see correlation between certain variables
house_prices_num_edit <- subset(house_prices_num, select = -c(index, lat, long, final_price_transformed, final_price_log))
#library for correlation matrix
install.packages("Hmisc")
library("Hmisc")
#correlation matrix
house_prices_num_edit.rcorr = rcorr(as.matrix(house_prices_num_edit))
house_prices_num_edit.rcorr
#correlation plot

corrplot(house_prices_num_edit.rcorr$r)

#correlation scatter plot
#insttall.packages("PerformanceAnalytics")
#library(PerformanceAnalytics)
#chart.Correlation(house_prices_num_edit, histogram=TRUE, pch=19)

#correlation with heat map
#col<- colorRampPalette(c("blue", "white", "red"))(20)
#heatmap(x = house_prices_num_edit, col = col, symm = TRUE)

#looking at outliers in numeric data
boxplot(house_prices_num_edit$final_price, ylab = "Final Price")
boxplot(house_prices_num_edit$list_price, ylab = "List Price")
boxplot(house_prices_num_edit$bathrooms, ylab = "Number of Bathrooms")
boxplot(house_prices_num_edit$bedrooms_ag, ylab = "Number of Bedrooms Upstairs")
boxplot(house_prices_num_edit$bedrooms_bg, ylab = "Number of Bedrooms Basement")
boxplot(house_prices_num_edit$bedrooms_total, ylab = "Total Bedrooms")
boxplot(house_prices_num_edit$sqft, ylab = "Square Feet")
boxplot(house_prices_num_edit$parking, ylab = "Number Of Parking Spots")
boxplot(house_prices_num_edit$mean_district_income, ylab = "Mean District Income")

#visualizing missing data
library(naniar)
vis_miss(house_prices)


