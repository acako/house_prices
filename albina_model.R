complete_d1 <- read.csv("app_house_pdata")
head(complete_d1)

complete_d_1 <- subset(complete_d1, select=-c(1, 3, 8, 9, 10, 12))
head(complete_d_1)
library(caret)
library(rpart)

library(xgboost)

library(tidyverse)



set.seed(100)
#model with standard parameters
model_final_price <- train(final_price ~.,data=complete_d_1,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                           method='xgbTree', )

#tune model
#grid <- expand.grid(nrounds = c(100, 200), eta = c(0.1, 0.3), max_depth = c(3, 6), gamma = seq(0, 10, 1), colsample_bytree = seq (0.5, 1, 0.1), min_child_weight = c(1,5), subsample = c(0.5, 0.8))
#model_final_price_1 <- train(final_price ~.,data=complete_d_1,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                           #method='xgbTree')
summary(model_final_price)
print(model_final_price)

plot(model_final_price)


attributes(model_final_price)
library(ggplot2)

min(model_final_price$results$RMSE)
max(model_final_price$results$Rsquared)
max(model_final_price$resuls$MAE)


test_data <- read.csv(file="~/Desktop/test_data1.csv", head=TRUE)
predict(model_final_price, test_data1=test-frame, type='prob')



