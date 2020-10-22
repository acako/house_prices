library(dplyr)
library(readr)
library(finalfit)
library(ggplot2)
library(grid)
library(gridExtra)
library(mice)
library(caret)
library(rpart)

data <- read.csv('houses_edited.csv')
summary(data)
#some NA's in sqft
#check for NA's in other columns
any(is.na(select(data,1:6,8:22)))
#no other NA's
#will need to impute sqft as 4500 is too many missing variables and sqft is important
theme_set(theme_minimal())
missing_plot(data)

#there are two categorical columns that have many levels, look into those:
data %>% group_by(type) %>% summarise(count = length(type))
#check how prices vary by type
price_type_plot <- data %>% ggplot(aes(x=type,y=final_price/1000)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
#some of these descriptions are very similar and have few values lets combine them into similar levels
data$type[data$type=='Att/Row/Twnhouse'] <- 'Townhouse'
data$type[data$type=='Co-Op Apt'] <- 'Condo'
data$type[data$type=='Co-Ownership Apt'] <- 'Condo'
data$type[data$type=='Comm Element Condo'] <- 'Condo'
data$type[data$type=='Condo Apt'] <- 'Condo'
data$type[data$type=='Condo Townhouse'] <- 'Condo'
data$type[data$type=='Link'] <- 'Detached'
data$type[data$type=='Store W/Apt/Offc'] <- 'Condo'
#check the summary again
data %>% group_by(type) %>% summarise(count = length(type))
#check how prices vary by type
price_type_plot <- data %>% ggplot(aes(x=type,y=final_price/1000)) + geom_boxplot(alpha=0.25) + theme(axis.text.x = element_text(angle = 90))

#add a column for total bedrooms
data <- data %>% mutate(beds = bedrooms_ag+bedrooms_bg)

#check for randomness
complete <- names(select(data, 3:6,8,11,14,15,17:19,23))
missing <- names(select(data,7))

for (i in c(1:length(complete))) {
  print(data %>% missing_compare(missing, complete[i]))
}
#missing data seems to be strongly correlated to type of house and the price


#visualize the data
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
b1
# district (need to fix this)
b2 <- data %>% ggplot(aes(x=city_district)) + geom_bar(fill='grey',colour='black', alpha = 0.6) + theme(axis.text.x = element_text(angle = 90))
b2
# plot showing outliers
s1 <- data %>% ggplot(aes(x=final_price/1000,y=list_price/1000)) + geom_point(alpha=0.5) + geom_abline(intercept=0,slope=1, colour='red')
s1
# 5 houses that are clear outliers on price
s2 <- data %>% ggplot(aes(x=final_price/1000,y=sqft)) + geom_point(alpha=0.5)
s2
# no real outliers for sqft

#removing outliers for price > $7.5M
data <- data %>% subset(final_price<7500000)
# plot showing removal of outliers
s3 <- data %>% ggplot(aes(x=final_price/1000,y=list_price/1000)) + geom_point(alpha=0.5) + geom_abline(intercept=0,slope=1, colour='red')
s3
#show relationship between final and list price
data_with_price_diff <- data %>% mutate(price_diff = final_price-list_price)
price_diff <- data_with_price_diff %>% ggplot(aes(x=price_diff/1000)) + geom_density(fill='grey', colour='black', alpha = 0.6) + xlim(-250,250)
price_diff

#impute missing values
set.seed(123)
# methods that dont work include pmm, midastouch, numeric, binary, ordered, unordered
summary(data$sqft)
# random forest method
imputed_data_rf <- mice(data, m=5, maxit=50, meth='rf')
densityplot(imputed_data_rf)
summary(imputed_data_rf$imp$sqft)

# random sample of observed values
imputed_data_sample <- mice(data, m=5, maxit=50, meth='sample')
densityplot(imputed_data_sample)
summary(imputed_data_sample$imp$sqft)

# classification and regression trees
imputed_data_cart <- mice(data, m=5, maxit=50, meth='cart')
densityplot(imputed_data_cart)
summary(imputed_data_cart$imp$sqft)

#linear discriminant analysis
imputed_data_lda <- mice(data, m=5, maxit=50, meth='lda')
densityplot(imputed_data_lda)
summary(imputed_data_lda$imp$sqft)

#cart looks to be the best option
#run regression on all 5 of the cart options

data_cart_1 <- complete(imputed_data_cart,1)
data_cart_2 <- complete(imputed_data_cart,2)
data_cart_3 <- complete(imputed_data_cart,3)
data_cart_4 <- complete(imputed_data_cart,4)
data_cart_5 <- complete(imputed_data_cart,5)

#framework for running regression:
data_sets = list(data_cart_1, data_cart_2, data_cart_3, data_cart_4, data_cart_5)

models_final_price = c()
models_list_price = c()
train_control = trainControl(method='cv', number=5)
i=1
'
for (set in data_sets){
  df_final <- set %>% select(3,5:7,10,13,14,16,21,22)
  df_list <- set %>% select(2,5:7,10,13,14,16,21,22)
  models_final_price[i] <- #model code goes here...use df_final as dataset
  models_list_price[i] <- #model code goes here...use df_list as dataset
  i = i +1
}
'
#glm
for (set in data_sets){
  df_final <- set %>% select(3,5:7,10,13,14,17,21,22)
  df_list <- set %>% select(2,5:7,10,13,14,17,21,22)
  models_final_price[i] <- train(final_price ~.,data=df_final,trControl=train_control, method='glm')
  models_list_price[i] <- train(list_price ~.,data=df_list,trControl=train_control, method='glm')
  i = i +1
}

models_final_price[1] <- train(data_cart_1$final_price ~.,data=data_cart_1,trControl=train_control, method='rpart')
