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
#imputed_data_rf <- mice(data, m=5, maxit=50, meth='rf')
#densityplot(imputed_data_rf)
#summary(imputed_data_rf$imp$sqft)

# random sample of observed values
#imputed_data_sample <- mice(data, m=5, maxit=50, meth='sample')
#densityplot(imputed_data_sample)
#summary(imputed_data_sample$imp$sqft)

# classification and regression trees
imputed_data_cart <- mice(data, m=5, maxit=50, meth='cart')
densityplot(imputed_data_cart)
summary(imputed_data_cart$imp$sqft)

#linear discriminant analysis
#imputed_data_lda <- mice(data, m=5, maxit=50, meth='lda')
#densityplot(imputed_data_lda)
#summary(imputed_data_lda$imp$sqft)

#cart looks to be the best option
#run regression on all 5 of the cart options
data$type <- as.factor(data$type)
data_cart_1 <- complete(imputed_data_cart,1)
data_cart_2 <- complete(imputed_data_cart,2)
data_cart_3 <- complete(imputed_data_cart,3)
data_cart_4 <- complete(imputed_data_cart,4)
data_cart_5 <- complete(imputed_data_cart,5)

#framework for running regression:
data_sets = list(data_cart_1, data_cart_2, data_cart_3, data_cart_4, data_cart_5)

models_final_price <- list()
models_list_price <- list()
models_final_price_cp <- list()
models_list_price_cp <- list()
models_final_price_md <- list()
models_list_price_md <- list()
ctrl <- trainControl(method='cv', number=5, savePredictions = 'all')
i <- 1

#decision tree no tuning
for (set in data_sets){
  df_final <- set %>% select(3,6:8,11,14,15,16,21:23)
  df_list <- set %>% select(4,6:8,11,14,15,16,21:23)
  models_final_price[[i]] <- train(final_price ~.,
                                  data=df_final,
                                  trControl = ctrl,
                                  method='rpart',
                                  tuneLength=10)
  models_list_price[[i]] <- train(list_price ~.,
                                  data=df_list,
                                  trControl = ctrl,
                                  method='rpart',
                                  tuneLength=10)
  i = i +1
}

i <- 1
grid_cp <- expand.grid(cp=seq(0.005,0.2,0.02))
#decision tree tuning cp
for (set in data_sets){
  df_final <- set %>% select(3,6:8,11,14,15,16,21:23)
  df_list <- set %>% select(4,6:8,11,14,15,16,21:23)
  models_final_price_cp[[i]] <- train(final_price ~.,
                                      data=df_final,
                                      trControl = ctrl,
                                      method='rpart',
                                      tuneGrid=grid_cp)
  models_list_price_cp[[i]] <- train(list_price ~.,
                                    data=df_list,
                                    trControl = ctrl,
                                    method='rpart',
                                    tuneGrid=grid_cp)
  i = i +1
}

i <- 1
grid_md <- expand.grid(maxdepth=seq(2,11,1))
#decision tree tuning maxdepth
for (set in data_sets){
  df_final <- set %>% select(3,6:8,11,14,15,16,21:23)
  df_list <- set %>% select(4,6:8,11,14,15,16,21:23)
  models_final_price_md[[i]] <- train(final_price ~.,
                                      data=df_final,
                                      trControl = ctrl,
                                      method='rpart2',
                                      tuneGrid=grid_md)
  models_list_price_md[[i]] <- train(list_price ~.,
                                    data=df_list,
                                    trControl = ctrl,
                                    method='rpart2',
                                    tuneGrid=grid_md)
  i = i +1
}


#model performance:
tunes_final = list(models_final_price, models_final_price_cp, models_final_price_md)
tunes_list = list(models_list_price, models_list_price_cp, models_list_price_md)

rmse <- list()
i = 1
for (model in tunes_final){
  for (y in c(1:5)){
    for (x in c(1:10)){
      rmse[i] <- model[[y]]$results$RMSE[x]
      i = i + 1
    }
  }
}

rsquared <- list()
i = 1
for (model in tunes_final){
  for (y in c(1:5)){
    for (x in c(1:10)){
      rsquared[i] <- model[[y]]$results$Rsquared[x]
      i = i + 1
    }
  }
}

mae <- list()
i = 1
for (model in tunes_final){
  for (y in c(1:5)){
    for (x in c(1:10)){
      mae[i] <- model[[y]]$results$MAE[x]
      i = i + 1
    }
  }
}

model_names_as_strings <- list('models_final_price', 'models_final_price_cp', 'models_final_price_md')
model_name <- list()
i=1
for (model in model_names_as_strings){
  for (x in c(1:50)){
    model_name[i] <- model
    i = i + 1
  }
}
i=1
iteration <- list()
for (x in c(1:15)){
  j = 1
  for (y in c(1:10)){
    iteration[i] <- j
    j = j + 1
    i = i + 1
  }
}

metrics_df <- data.frame(model_name=unlist(model_name), iteration=unlist(iteration), rmse=unlist(rmse), rsquared=unlist(rsquared), mae=unlist(mae))

mae_rmse <- metrics_df %>% ggplot(aes(x=mae, y=rmse)) + geom_point(aes(colour=model_name))
mae_rs <- metrics_df %>% ggplot(aes(x=mae, y=rsquared)) + geom_point(aes(colour=model_name))
rs_rmse <- metrics_df %>% ggplot(aes(x=rsquared, y=rmse)) + geom_point(aes(colour=model_name))
