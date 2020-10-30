library(dplyr)
library(readr)
library(mice)
library(ggplot2)
library(VIM)
library(caret)
library(gbm)
library(mice)

df <- read.csv('houses_edited.csv')

#check for duplicates - none found
df$duplicate <- duplicated(df)
df <- df[, !names(df) %in% c('duplicate')]
df$bedrooms <- df$bedrooms_ag + df$bedrooms_bg
df <-  df[, !names(df) %in% c('index', 'title', 'full_link', 'mls', 'description', 'full_address', 'final_price_transformed', 'final_price_log', 'bedrooms_ag', 'bedrooms_bg')]
numerical_cols <- c('bathrooms', 'sqft', 'parking', 'lat', 'long', 'mean_district_income', 'bedrooms')
categorical_cols <- c('type' ,'city_district')


#cleaning apartment types
df$type[df$type=='Att/Row/Twnhouse'] <- 'Townhouse'
df$type[df$type=='Co-Op Apt'] <- 'Condo'
df$type[df$type=='Co-Ownership Apt'] <- 'Condo'
df$type[df$type=='Comm Element Condo'] <- 'Condo'
df$type[df$type=='Condo Apt'] <- 'Condo'
df$type[df$type=='Condo Townhouse'] <- 'Condo'
df$type[df$type=='Link'] <- 'Detached'
df$type[df$type=='Store W/Apt/Offc'] <- 'Condo'

table(df$type)
barplot(table(df$type))

#removing outliers for price df
price_lm <- lm(final_price ~ list_price, df)
ggplot(df, aes(x=list_price,y=final_price)) + geom_point() + geom_smooth(method = 'lm') + annotate('text', label = paste('R-sqr = ', round(summary(price_lm)$adj.r.squared, 2)), x = 0.8*max(df$list_price), y= max(df$final_price))
df <- subset(df, df$final_price < 9000000)
price_lm <- lm(final_price ~ list_price, df)
ggplot(df, aes(x=list_price,y=final_price)) + geom_point() + geom_smooth(method = 'lm') + annotate('text', label = paste('R-sqr = ', round(summary(price_lm)$adj.r.squared, 2)), x = 0.8*max(df$list_price), y= max(df$final_price))
price_lm$coefficients

#imputing missing df
set.seed(123)
summary(df$sqft)

# classification and regression trees
imputed_df_cart <- mice(df, m=5, maxit=50, meth='cart')
densityplot(imputed_df_cart)
summary(imputed_df_cart$imp$sqft)
saveRDS(imputed_df_cart, 'imputed_df_cart.rds')

imputed_df_cart <- readRDS('imputed_df_cart.rds')

train_df <- read.csv('imputed_data.csv')
train_df <- train_df[, names(train_df) %in% c('final_price','bathrooms','sqft','parking','type','mean_district_income','beds')]
train_df$type <- as.factor(train_df$type)

# Training the first model with k-fold cross validation. Shrinkage was held at a constant 0.1, n.minobsinnode 
#held at constant of 10. Interaction depth was varied between 1:3, n.trees varied between 50, 100, 150
#Final model: n.trees = 100, interaction.depth = 3, shrinkage = 0.1, n.minobssinnode = 10
# 269699.9  0.8101509  148161.2

cv <- trainControl(method = 'cv', number = 10)
gbm1 <- train(final_price ~.,data = train_df, trControl = cv, method = 'gbm')
gbm1_plot <- summary(gbm1$finalModel)
gbm.perf(gbm1$finalModel) #60 trees
summary(gbm1)
gbm1
plot(gbm1)
saveRDS(gbm1, 'gbm1.rds')

#interaction depth = number of splits
#shrinkage = learning rate (reduces size of steps in-between iterations). High learn rates can overfit models with poor performance, use a small shrinkage rate when growing many trees
#trees = number of trees (number of gradient boosting iterations)
#minobsinnode = default 10, can try higher for regression


#Random grid search was then performed:
# here, best model was n.trees = 288, interaction.depth = 10, shrinkage = 0.2017825 and n.minobsinnode = 18
# seemed to prefer a higher number of trees, interaction depth, shrinkage, and n.minobssinnode. However, model accuracy was not improved
# 276098.6  0.8062016  136506.9
rndmCtrl <- trainControl(method = 'cv', number = 10, search = 'random')
gbm_rndm <- train(final_price ~.,data = train_df, trControl = rndmCtrl, method = 'gbm', tuneLength = 10)
gbm_rndm2_plot <- summary(gbm_rndm$finalModel)
gbm.perf(gbm_rndm$finalModel)
gbm.perf(gbm_rndm$finalModel, method = 'OOB')
summary(gbm_rndm)
gbm_rndm
saveRDS(gbm_rndm, 'gbm_rndm.rds')

#Use an even larger grid. seemed like n.trees didn't need to go very high. however here, we evaluate shrinkage 
# and give the model an opportunity to grow smaller trees while evaluating smaller shrinkage
# best model used n.trees = 200, interaction.depth = 9, shrinkage = 0.1 and n.minobsinnode = 20.
# RMSE: 257316.5  R2: 0.8282601  MAE: 134117.1
gbmGrid <- expand.grid(interaction.depth = c(1, 5, 9), 
                       n.trees = c(50, 100, 200), 
                       shrinkage = c(0.1, 0.01, 0.001),
                       n.minobsinnode = c(1, 10, 20)
                       )
gbm3 <- train(final_price ~.,data = train_df, trControl = cv, tuneGrid = gbmGrid, method = 'gbm')
gbm3_plot <- summary(gbm3$finalModel)
gbm.perf(gbm3$finalModel)
gbm.perf(gbm3$finalModel, method = 'OOB')
summary(gbm3)
gbm3
saveRDS(gbm3, 'gbm3.rds')

# Plotting importance of model 3:
par(mfrow=c(1,1))
model_importance <- summary(gbm3$finalModel)
barplot(model_importance[9:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = model_importance[9:1, 'var'], main = 'Relative Importance of Variables')

#optimal number of iterations = 51
gbm.perf(gbm3$finalModel)

#model 3 had the best accuracy in turns of RMSE, Rsquared, and MAE

#increasing shrinkage played a large role in lowering RMSE
par(mfrow=c(2,2))
boxplot(RMSE~shrinkage, data = gbm3$results, col = 'blue')
boxplot(RMSE~interaction.depth, data = gbm3$results, col = 'blue')
boxplot(RMSE~n.minobsinnode, data = gbm3$results, col = 'blue')
boxplot(RMSE~n.trees, data = gbm3$results, col = 'blue')

#both shrinkage and interaction depth played a role in increasing R2
par(mfrow=c(2,2))
boxplot(Rsquared~shrinkage, data = gbm3$results, col = 'red')
boxplot(Rsquared~interaction.depth, data = gbm3$results, col = 'red')
boxplot(Rsquared~n.minobsinnode, data = gbm3$results, col = 'red')
boxplot(Rsquared~n.trees, data = gbm3$results, col = 'red')

#shrinkage played a role in lowering MAE
par(mfrow=c(2,2))
boxplot(MAE~shrinkage, data = gbm3$results, col = 'green')
boxplot(MAE~interaction.depth, data = gbm3$results, col = 'green')
boxplot(MAE~n.minobsinnode, data = gbm3$results, col = 'green')
boxplot(MAE~n.trees, data = gbm3$results, col = 'green')

#Note that shrinkage and more trees play a similar role in tuning the model. Smaller shrinkage = finer tuning, but more trees. Large shrinkage = larger jumps and higher potential for overfitting 
# but also correcting. Typically leads to higher accuracy




