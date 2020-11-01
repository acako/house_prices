library(dplyr)
library(VIM)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(Hmisc)
library(ggcorrplot)
library(ggpmisc)

df <- read.csv('houses_edited.csv')

#Getting rid of variables without data utility or repeats:
df <-  df[, !names(df) %in% c('index', 'title', 'full_link', 'mls', 'bedrooms', 'description', 'full_address', 'final_price_transformed', 'final_price_log')]

#summary of numerical data - long/lat was included but will need a better way to deal with geographical location. What can be done is to apply cluster algorithm to see if they can be sort into neighborhoods with a low K
numeric_cols <- list('final_price', 'list_price', 'bathrooms', 'sqft', 'parking', 'lat', 'long', 'mean_district_income', 'bedrooms_ag', 'bedrooms_bg')
predictor_cols <- list('bathrooms', 'sqft', 'parking', 'lat', 'long', 'mean_district_income', 'bedrooms_ag', 'bedrooms_bg')

#histograms - note skew in prices
par(mfrow=c(2, 5))
for (i in numeric_cols) {
  hist(df[[i]], main = paste(i), xlab = '', col = 4)
}

#boxplot and outliers
par(mfrow=c(2, 5))
for (i in numeric_cols) {
  boxplot(df[[i]], main = paste(i), xlab = '', col = 4)
}

par(mfrow=c(2, 5))
for (i in numeric_cols) {
  d <- density(df[[i]], na.rm = TRUE)
  plot(d, type='n', main=numeric_cols)
  polygon(d, col='blue', border='gray')
}

#relationship between final price and list price: which one to use as target? (alternatively, can be used for both: suggested listing price, and predicted final price)
price_lm <- lm(final_price ~ list_price, df)
ggplot(df, aes(x=list_price,y=final_price)) + geom_point() + geom_smooth(method = 'lm') + annotate('text', label = paste('R-sqr = ', round(summary(price_lm)$adj.r.squared, 2)), x = 0.8*max(df$list_price), y= max(df$final_price))

#corrplot for numerical
cor <- rcorr(as.matrix(df[, names(df) %in% numeric_cols]))
par(mfrow=c(1,1))
ggcorrplot(cor$r, type = 'upper')

#corrplot for predictors only
cor_pred <- rcorr(as.matrix(df[, names(df) %in% predictor_cols]))
ggcorrplot(cor_pred$r, type = 'upper')

#text summary
summary(df[, names(df) %in% numeric_cols])

#summary of categorical data
categorical_cols <- c('type' ,'city_district')

#barplots
p1 <- ggplot(df, aes(type)) + geom_bar(fill = 'red') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2 <- ggplot(df, aes(city_district)) + geom_bar(fill = 'red') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
grid.arrange(p1, p2)

#text summary
table(df$type)
table(df$city_district)
  
#missing values
aggr_plot = aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), ylab=c('Histogram of missing data','Pattern'))
