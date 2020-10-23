#load csv file
house_prices <- read.csv(file="~/Desktop/CML/assignment_2/houses_edited.csv", head=TRUE)

#explore variables to see type
str(house_prices)
#create a new column including all bedrooms to see if there is missing values
house_prices$bedrooms_total <- house_prices$bedrooms_ag + house_prices$bedrooms_bg

#impute data for sqft
#loading mice package
#checking at missing data using mice
library(mice)
md.pattern(house_prices)

#observation of missing data using VIM
library(VIM)
missing_data_plot <- aggr(house_prices, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(house_prices), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

house_prices_updated <- house_prices[-c(1, 2, 5, 9, 10, 12, 13, 21, 22)]

#combining the similar house types 
house_prices_updated$type[house_prices_updated$type=='Att/Row/Twnhouse'] <- 'Townhouse'
house_prices_updated$type[house_prices_updated$type=='Co-Op Apt'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Co-Ownership Apt'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Comm Element Condo'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Condo Apt'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Condo Townhouse'] <- 'Condo'
house_prices_updated$type[house_prices_updated$type=='Link'] <- 'Detached'
house_prices_updated$type[house_prices_updated$type=='Store W/Apt/Offc'] <- 'Condo'

head(house_prices_updated)

#imputation of sqft 

#imputing using the random sample in mice
cl_houseprices_randomsample<- mice(house_prices_updated,m=5,maxit=50,meth='sample', seed = 500)
summary(tempData)
#imputing using the random forest in mice
cl_houseprices_randomforest <- mice(house_prices_updated,m=5,maxit=50,meth='rf', seed = 500)
#imputing using cart in mice
cl_houseprices_cart <- mice(house_prices_updated,m=5,maxit=50,meth='cart', seed = 500)

#looking at summary of each imputed data
summary(cl_houseprices_randomsample)
summary(cl_houseprices_randomforest)
summary(cl_houseprices_cart)


#looking at density plot for each imputed data method
densityplot(cl_houseprices_randomforest)
densityplot(cl_houseprices_cart)
densityplot(cl_houseprices_randomsample)

complete_data <- complete(cl_houseprices_cart, 3)
head(complete_data)



