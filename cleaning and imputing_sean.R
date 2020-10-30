library(dplyr)
library(readr)
library(mice)
library(ggplot2)
library(VIM)
library(caret)

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
# random forest method
imputed_df_rf <- mice(df, m=5, maxit=50, meth='rf')
densityplot(imputed_df_rf)
summary(imputed_df_rf$imp$sqft)
saveRDS(imputed_df_rf, 'imputed_df_rf.rds')

# random sample of observed values
imputed_df_sample <- mice(df, m=5, maxit=50, meth='sample')
densityplot(imputed_df_sample)
summary(imputed_df_sample$imp$sqft)
saveRDS(imputed_df_sample, 'imputed_df_sample.rds')

# classification and regression trees
imputed_df_cart <- mice(df, m=5, maxit=50, meth='cart')
densityplot(imputed_df_cart)
summary(imputed_df_cart$imp$sqft)
saveRDS(imputed_df_cart, 'imputed_df_cart.rds')

#mean imputation
imputed_df_mean <- mice(df, m=5, maxit=50, meth='mean')
densityplot(imputed_df_mean)
summary(imputed_df_mean$imp$sqft)
saveRDS(imputed_df_mean, 'imputed_df_mean.rds')

imputed_df_cart <- readRDS('imputed_df_cart.rds')

train_list <- list()
for (i in 1:5){
  train_list[[i]] <- complete(imputed_df_cart, i)
}

train_list_price <- list()
for (i in 1:5){
  train_list_price[[i]] <- train_list[[i]][, !names(train_list[[i]]) %in% c('final_price', 'district_code')]
}

train_final_price <- list()
for (i in 1:5){
  train_final_price[[i]] <- train_list[[i]][, !names(train_list[[i]]) %in% c('list_price', 'district_code')]
}


pred_list_price <- list()
for (i in 1:5){
  pred_list_price[[i]] <- train(list_price ~.,data = train_list_price[[i]],trControl = trainControl('cv', number=10, savePredictions = 'all'),
                                                 method='gbm')
}
saveRDS(pred_list_price,'pred_list_price.rds')


pred_final_price <- list()
for (i in 1:5){
  pred_final_price[[i]] <- train(final_price ~.,data = train_final_price[[i]],trControl = trainControl('cv', number=10, savePredictions = 'all'),
                                method='gbm')
}

saveRDS(pred_final_price,'pred_final_price.rds')



## training removing city
train_list_price_nocity <- list()
for (i in 1:5){
  train_list_price_nocity[[i]] <- train_list[[i]][, !names(train_list[[i]]) %in% c('final_price', 'city_district', 'district_code')]
}

train_final_price_nocity <- list()
for (i in 1:5){
  train_final_price_nocity[[i]] <- train_list[[i]][, !names(train_list[[i]]) %in% c('list_price', 'city_district', 'district_code')]
}

pred_list_price_nocity <- list()
for (i in 1:5){
  pred_list_price_nocity[[i]] <- train(list_price ~.,data = train_list_price_nocity[[i]],trControl = trainControl('cv', number=10, savePredictions = 'all'),
                                method='gbm')
}
saveRDS(pred_list_price_nocity,'pred_list_price_nocity.rds')

pred_final_price_nocity <- list()
for (i in 1:5){
  pred_final_price_nocity[[i]] <- train(final_price ~.,data = train_final_price_nocity[[i]],trControl = trainControl('cv', number=10, savePredictions = 'all'),
                                 method='gbm')
}
saveRDS(pred_final_price_nocity,'pred_final_price_nocity.rds')

#model evaluation
#list price model summaries
pred_final_price <- readRDS('pred_final_price.rds')
pred_final_price_nocity <- readRDS("pred_final_price_nocity.rds")
pred_list_price <- readRDS("pred_list_price.rds")
pred_list_price_nocity <- readRDS("pred_list_price_nocity.rds")
pred_no_imp_lp <- readRDS("pred_no_imp_lp.rds")
pred_no_imp_fp <- readRDS("pred_no_imp_fp.rds")



lp1 <- data.frame(summary(pred_list_price[[1]]$finalModel))
lp2 <- summary(pred_list_price[[2]]$finalModel)
lp3 <- summary(pred_list_price[[3]]$finalModel)
lp4 <- summary(pred_list_price[[4]]$finalModel)
lp5 <- summary(pred_list_price[[5]]$finalModel)

par(mfrow=c(2,3))
barplot(lp1[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = lp1[10:1, 'var'], main = 'Variable importance - List price df 1')
barplot(lp2[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = lp2[10:1, 'var'], main = 'Variable importance - List price df 2')
barplot(lp3[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = lp3[10:1, 'var'], main = 'Variable importance - List price df 3')
barplot(lp4[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = lp4[10:1, 'var'], main = 'Variable importance - List price df 4')
barplot(lp5[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = lp5[10:1, 'var'], main = 'Variable importance - List price df 5')

#final price models
fp1 <- summary(pred_final_price[[1]]$finalModel)
fp2 <- summary(pred_final_price[[2]]$finalModel)
fp3 <- summary(pred_final_price[[3]]$finalModel)
fp4 <- summary(pred_final_price[[4]]$finalModel)
fp5 <- summary(pred_final_price[[5]]$finalModel)

par(mfrow=c(2,3))
barplot(fp1[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = fp1[10:1, 'var'], main = 'Variable importance - Final price df 1')
barplot(fp2[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = fp2[10:1, 'var'], main = 'Variable importance - Final price df 2')
barplot(fp3[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = fp3[10:1, 'var'], main = 'Variable importance - Final price df 3')
barplot(fp4[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = fp4[10:1, 'var'], main = 'Variable importance - Final price df 4')
barplot(fp5[10:1, 'rel.inf'], col = 'blue', xlab = 'relative influence', horiz = TRUE, las = 2, names = fp5[10:1, 'var'], main = 'Variable importance - Final price df 5')



#list price no city model summaries
lpnc1 <- summary(pred_list_price_nocity[[1]]$finalModel)
lpnc2 <- summary(pred_list_price_nocity[[2]]$finalModel)
lpnc3 <- summary(pred_list_price_nocity[[3]]$finalModel)
lpnc4 <- summary(pred_list_price_nocity[[4]]$finalModel)
lpnc5 <- summary(pred_list_price_nocity[[5]]$finalModel)

par(mfrow=c(2,3))
barplot(lpnc1[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = lpnc1[10:1, 'var'], main = 'Variable importance - List price df 1')
barplot(lpnc2[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = lpnc2[10:1, 'var'], main = 'Variable importance - List price df 2')
barplot(lpnc3[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = lpnc3[10:1, 'var'], main = 'Variable importance - List price df 3')
barplot(lpnc4[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = lpnc4[10:1, 'var'], main = 'Variable importance - List price df 4')
barplot(lpnc5[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = lpnc5[10:1, 'var'], main = 'Variable importance - List price df 5')


#final price no city model summaries

fpnc1 <- summary(pred_final_price_nocity[[1]]$finalModel)
fpnc2 <- summary(pred_final_price_nocity[[2]]$finalModel)
fpnc3 <- summary(pred_final_price_nocity[[3]]$finalModel)
fpnc4 <- summary(pred_final_price_nocity[[4]]$finalModel)
fpnc5 <- summary(pred_final_price_nocity[[5]]$finalModel)

par(mfrow=c(2,3))
barplot(fpnc1[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = fpnc1[10:1, 'var'], main = 'Variable importance - Final price df 1')
barplot(fpnc2[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = fpnc2[10:1, 'var'], main = 'Variable importance - Final price df 2')
barplot(fpnc3[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = fpnc3[10:1, 'var'], main = 'Variable importance - List price df 3')
barplot(fpnc4[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = fpnc4[10:1, 'var'], main = 'Variable importance - List price df 4')
barplot(fpnc5[10:1, 'rel.inf'], col = 'green', xlab = 'relative influence', horiz = TRUE, las = 2, names = fpnc5[10:1, 'var'], main = 'Variable importance - List price df 5')

#with city district: sqft, mean income, bathrooms, long, typeDetached, lat, parking, bedrooms, city_districtAnnex, city_districtBedford Park-Nortown
#without city district: sqft, mean district income, bathrooms, long, typeDetached, lat, parking, bedrooms, typeTownhouse, typePlex
#other plots:
par(mfrow=c(1,1))
print(gbm.perf(pred_final_price[[1]]$finalModel,plot.it = TRUE))
print(gbm.perf(pred_final_price_nocity[[1]]$finalModel,plot.it = TRUE))
print(gbm.perf(pred_list_price[[1]]$finalModel,plot.it = TRUE))


#train without imputation
train_no_imp_lp <- df[, !names(df) %in% c('final_price', 'district_code', 'city_district')]
pred_no_imp_lp <- train(list_price ~.,data = train_no_imp_lp,trControl = trainControl('cv', number=10, savePredictions = 'all'), method='gbm', na.action = na.pass)
saveRDS(pred_no_imp_lp, 'pred_no_imp_lp.rds')

train_no_imp_fp <- df[, !names(df) %in% c('list_price', 'district_code', 'city_district')]
pred_no_imp_fp <- train(final_price ~.,data = train_no_imp_fp,trControl = trainControl('cv', number=10, savePredictions = 'all'), method='gbm', na.action = na.pass)
saveRDS(pred_no_imp_fp, 'pred_no_imp_fp.rds')

nilp <- summary(pred_no_imp_lp$finalModel)
nifp <-summary(pred_no_imp_fp$finalModel)

par(mfrow=c(1,2))
barplot(nilp[10:1, 'rel.inf'], col = 'red', xlab = 'relative influence', horiz = TRUE, las = 2, names = nilp[10:1, 'var'], main = 'Variable importance - List price no imp')
barplot(nifp[10:1, 'rel.inf'], col = 'red', xlab = 'relative influence', horiz = TRUE, las = 2, names = nifp[10:1, 'var'], main = 'Variable importance - Final price no imp')
#without imputation: bathrooms, mean district income, sqft, long, typeDetached, lat, parking, bedrooms, typeTownhouse, typePlex

#Overall model accuracy
pred_list_price[[1]] #RMSE: 233066.6, R2: 0.8531249, MAE: 131277.3
pred_list_price[[2]] #233732.7  0.8511510  130870.6
pred_list_price[[3]] #235154.9  0.8505187  131784.2
pred_list_price[[4]] #234800.6  0.8503774  132111.5
pred_list_price[[5]] #234867.3  0.8510420  132023.5
pred_final_price[[1]] #RMSE: 221545.5, R2: 0.8621175, MAE: 126529.9 
pred_list_price_nocity[[1]] #RMSE: 233454.2, R2: 0.8522975, MAE: 131180.7
pred_final_price_nocity[[1]]  #RMSE: 222611.5, R2: 0.8593473, MAE: 126701.0
pred_no_imp_lp #RMSE: 243366.2, R2: 0.8388627, MAE: 134214.2
pred_no_imp_fp  #RMSE: 234262.4, R2:  0.8436058, MAE: 130088.2

#overall finding: removing city district didn't actually change error that much but greatly cut down time on training 
#no imputation resulted in slight increase in error but also reduces time imputing data

#Will try training model on fewer categories, or perhaps use PCA first then train again, or train on bedrooms above ground?





