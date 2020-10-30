complete_d1 <- read.csv("app_house_pdata")
head(complete_d1)

complete_d_1 <- subset(complete_d1, select=-c(1, 3, 8, 9, 10, 12))
library(dplyr)
complete_d_1 <- complete_d_1 %>% subset(final_price<9000000)
head(complete_d_1)
max(complete_d_1$final_price)

nrow(complete_d_1)
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

summary(model_final_price_1)
print(model_final_price_1)

plot(model_final_price_1)


attributes(model_final_price_1)
library(ggplot2)

min(model_final_price_1$results$RMSE)
max(model_final_price_1$results$Rsquared)
max(model_final_price_1$resuls$MAE)


#test_data1 <- 
#predict(model_final_price, new_data = test_data1, type='prob')



