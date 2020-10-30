#load cleaner data
complete_d1 <- read.csv("app_house_pdata")
head(complete_d1)
#finish data cleaning
complete_d_1 <- subset(complete_d1, select=-c(1, 3, 8, 9, 10, 12))
library(dplyr)
complete_d_1 <- complete_d_1 %>% subset(final_price<9000000)
head(complete_d_1)
max(complete_d_1$final_price)
nrow(complete_d_1)

#load libraries
library(caret)
library(rpart)
library(xgboost)
library(tidyverse)



set.seed(100)
#model with standard parameters
#model_final_price <- train(final_price ~.,data=complete_d_1,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                           #method='xgbTree', )

#tune model
grid <- expand.grid(nrounds = c(100, 200), eta = c(0.1, 0.3), max_depth = c(3, 6), gamma = 0, colsample_bytree = 0.8, min_child_weight = c(1,5), subsample = 0.8)
model_final_price_1 <- train(final_price ~.,data=complete_d_1,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                           method='xgbTree', tuneGrid = grid)

#model statistics
summary(model_final_price_1)
print(model_final_price_1)
plot(model_final_price_1)
attributes(model_final_price_1)

#model accuracy
min(model_final_price_1$results$RMSE)
max(model_final_price_1$results$Rsquared)
max(model_final_price_1$resuls$MAE)


#Min RMSE: 220850.4
#Min MAE: 116308.5
#Max Rsquared: 0.861169

#save RDS
saveRDS(model_final_price_1, 'albina_model.rds')


