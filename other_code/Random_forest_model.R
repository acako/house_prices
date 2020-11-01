library(dplyr)
library(readr)
library(finalfit)
library(ggplot2)
library(grid)
library(gridExtra)
library(mice)
library(caret)
library(rpart)
library(randomForest)


data <- read.csv('houses_edited.csv')
summary(data)

data <- data %>% subset(final_price<7500000)
data$type[data$type=='Att/Row/Twnhouse'] <- 'Townhouse'
data$type[data$type=='Co-Op Apt'] <- 'Condo'
data$type[data$type=='Co-Ownership Apt'] <- 'Condo'
data$type[data$type=='Comm Element Condo'] <- 'Condo'
data$type[data$type=='Condo Apt'] <- 'Condo'
data$type[data$type=='Condo Townhouse'] <- 'Condo'
data$type[data$type=='Link'] <- 'Detached'
data$type[data$type=='Store W/Apt/Offc'] <- 'Condo'
data <- data %>% mutate(beds = bedrooms_ag+bedrooms_bg)

imputed_data_cart <- mice(data, m=5, maxit=50, meth='cart')
summary(imputed_data_cart$imp$sqft)
data$type <- as.factor(data$type)
data_cart_1 <- complete(imputed_data_cart,1)
data_cart_2 <- complete(imputed_data_cart,2)
data_cart_3 <- complete(imputed_data_cart,3)
data_cart_4 <- complete(imputed_data_cart,4)
data_cart_5 <- complete(imputed_data_cart,5)

#framework for running regression:
#data_sets = list(data_cart_1, data_cart_2, data_cart_3, data_cart_4, data_cart_5)


#for (set in data_sets){
  #df_final <- set %>% select(3,6:8,11,14,15,17,21:23)
  #df_list <- set %>% select(2,6:8,11,14,15,17,21:23)
  #models_final_price[[i]] <- #model code goes here...use df_final as dataset
  #models_list_price[[i]] <- #model code goes here...use df_list as dataset
  #i = i +1
#}

#creat data sets used for modeling
df_final <- data_cart_1 %>% select(3,6:8,11,17,23)
df_list <- data_cart_1 %>% select(2,6:8,11,17,23)

#Split the training data and test data
set.seed(123)
ind <- sample(2, nrow(df_final), replace = TRUE, prob = c(0.7, 0.3))
train <- df_final[ind==1,]
test <- df_final[ind==2,]


set.seed(222)
#create the model using randomForest package
#I tried some different number of trees and no big difference
models_rf <- randomForest(final_price ~., data=train,
                          trControl = trainControl("cv",number=10,savePredictions = 'all'),
                          ntree = 150,
                          mtry = 6,
                          importance = TRUE,
                          proximity = TRUE)

print(models_rf)#has mean of squared residuals:42284526674
attributes(models_rf)
#predict the outcome on a test set
p1 <- predict(models_rf, test)
head(p1)
head(test)
#check the performance of the model
RMSE(p1, test$final_price)#RMSE
R2(p1, test$final_price)#R2
MAE(p1, test$final_price)#MAE
mean((test$final_pricel - p1)^2)
plot(models_rf)

which.min(models_rf$mse)#MIN MSE

sqrt(models_rf$mse[which.min(models_rf$mse)])
#to find the best mtry, I tried multiple different mtry the results are really similiar
t <- tuneRF(test[,-1], test[,1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

#variable importance
varImpPlot(models_rf,
           sort = T,
           n.var = 5,
           main = "Top 5 - Variable Importance")

importance(models_rf)
varUsed(models_rf)

saveRDS(models_rf, file = "randomForests.rds")
#create the model using caret package
#Specify the tuning grid for different parameters
rf_grid <- expand.grid(mtry = c(2,4),
                       splitrule = c("gini"),
                       min.node.size = c(1))
#model using kfold
rf_fit_final <- train(as.factor(final_price) ~ ., 
                data = train, 
                method = "ranger",
                #trControl = trainControl("cv",number=10,savePredictions = 'all'),
                #tuneGrid = rf_grid
                )

rf_fit_final

#predict the outcome on a test set
rf_predict_final <- predict(rf_fit_final, test)






