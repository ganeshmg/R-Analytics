#Analytics Vidhya - Black Friday
#Author: Ganesh Babu Gajendiran
#Date: 22-JUL-2016


#Importing necessary libraries
library(stringr)
library(sqldf)
library(forecast)
library(timeDate)
library(data.table)
library(chron)

setwd("BlackFriday-Jul2016/")
#Reading the train and test data set from csv file to data frame
if (!exists("train")) {
        train = as.data.table(read.csv("train.csv"))
              
}

if (!exists("test")) {
        test =  as.data.table(read.csv("test-comb.csv"))
}

train$Age_R = 0
train[train$Age=='0-17',]$Age_R =  1
train[train$Age=='18-25',]$Age_R =  2

train[train$Age=='26-35',]$Age_R =  3
train[train$Age=='36-45',]$Age_R =  4
train[train$Age=='46-50',]$Age_R =  5
train[train$Age=='51-55',]$Age_R =  6
train[train$Age=='55+',]$Age_R =  7

train$Age_R = as.factor(train$Age_R)

sqldf("select distinct Age, Age_R from train order by Age_R")

train$Occupation = as.factor(train$Occupation)
train$Product_Category_1 = as.factor(train$Product_Category_1)
train$Product_Category_2 = as.factor(train$Product_Category_2)
train$Product_Category_3 = as.factor(train$Product_Category_3)













train$Src = 'Train'
test$Number_SKU_Sold = 0
test$Src = 'Test'
data = rbind(train,test)

data$Date = as.Date(data$Date, format = '%d-%b-%y')

#Adding day of the week
data$weekday = weekdays(data$Date)
#Adding month
data$month = month(data$Date)
#Adding year
data$year = year(data$Date)
#Adding weekend 
data$weekend = 'O'
data[data$weekday == 'Saturday' | data$weekday == 'Sunday',]$weekend = 'Y'
data[data$weekday != 'Saturday' & data$weekday != 'Sunday',]$weekend = 'N'

sqldf("select distinct weekend from data where weekday = 'Saturday' or weekday = 'Sunday'")
sqldf("select distinct weekend from data where weekday != 'Saturday' and weekday != 'Sunday'")

#Adding holiday
data$holiday = isHoliday(data$Date, holidays = holidayNYSE(), wday = 1:5)
#returns same data as weekend.

tr = sqldf("select Date,Number_SKU_Sold,weekday,month,year,holiday from data where Src = 'Train'")

te = sqldf("select Date,Number_SKU_Sold,weekday,month,year,holiday from data where Src = 'Test'")

#linear model
lm_1 = lm(Number_SKU_Sold~., data = tr)
lm_2 = lm(Number_SKU_Sold~Date, data = tr)
lm_3 = lm(Number_SKU_Sold~Date+month+holiday,data = tr)
lm_4 = lm(Number_SKU_Sold~Date+month,data = tr)

set.seed(123)
boruta.tr= Boruta(Number_SKU_Sold~., data = tr, doTrace = 2)
boruta.tr

getSelectedAttributes(boruta.tr, withTentative = F)

boruta.tr.1= Boruta(Number_SKU_Sold~Date+year, data = tr, doTrace = 2)

lm_5 = lm(Number_SKU_Sold~Date+year,data = tr)

tr_control = trainControl(method = "cv", number = 5)
tr_grid = expand.grid(n.trees = seq(1:20)*10,interaction.depth = c(5,15,30),shrinkage = 0.1, n.minobsinnode = 10)
gbm_model = train(Number_SKU_Sold~Date+year, data = tr,
                  method="gbm",tuneGrid = tr_grid,trControl = tr_control)

tr_control1 = trainControl(method = "cv", number = 5)
tr_grid1 = expand.grid(n.trees = seq(1:20)*20,interaction.depth = c(5,15,30),shrinkage = 0.1, n.minobsinnode = 10)
#Same as gbm_model, except for the n.trees increased from a factor of 10 to 20
gbm_model1 = train(Number_SKU_Sold~Date+year, data = tr,
                   method="gbm",tuneGrid = tr_grid1,trControl = tr_control1)

tr_control2 = trainControl(method = "repeatedcv", number = 5)
tr_grid2 = expand.grid(n.trees = seq(1:20)*20,interaction.depth = c(5,15,30),shrinkage = 0.1, n.minobsinnode = 10)
#Same as gbm_model, except for the method from cv to repeatedcv
gbm_model2 = train(Number_SKU_Sold~Date+year, data = tr,
                   method="gbm",tuneGrid = tr_grid2,trControl = tr_control2)


rf = randomForest(Number_SKU_Sold~., data = tr, importance=TRUE, ntree = 2000)


te_pred = predict(gbm_model2,te)

submit = as.character(te$Date, format = '%d-%b-%y')
submit = cbind(Date = submit,Number_SKU_Sold = te_pred)
rownames(submit) = NULL

write.csv(submit,"Submit_gbm_model2.csv",row.names = FALSE)






#Data is more than a year, then it may be necessary to allow for annual seasonality 
#as well as weekly seasonality and hence 7 (days a week), 365.25 (days a year)
train_ts <- msts(train, seasonal.periods=c(7,365.25))

fit <- tbats(train_ts)
fc <- forecast(fit)
plot(fc)

plot(train)




#Arima

train = train[,c(1,2)]
y <- ts(train, frequency=7)
z <- fourier(ts(train, frequency=365.25), K=5)
zf <- fourierf(ts(train, frequency=365.25), K=5, h=100)
fit <- auto.arima(y, xreg=cbind(z,holiday))
fc <- forecast(fit, xreg=cbind(zf,holidayf), h=100)