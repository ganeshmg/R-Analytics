library(data.table)
if (!exists("train")) {
        train = fread("Train_KQyJ5eh.csv",header = TRUE)        
}

if (!exists("test")) {
        test = fread("Test_HmLwURQ.csv",header = TRUE)
}

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

set.seed(123)

tr_control = trainControl(method = "cv", number = 5)
tr_grid = expand.grid(n.trees = seq(1:20)*10,interaction.depth = c(5,15,30),shrinkage = 0.1, n.minobsinnode = 10)
gbm_model = train(Number_SKU_Sold~Date+year, data = tr,
                  method="gbm",tuneGrid = tr_grid,trControl = tr_control)

te_pred = predict(gbm_model,te)

submit = as.character(te$Date, format = '%d-%b-%y')
submit = cbind(Date = submit,Number_SKU_Sold = te_pred)
rownames(submit) = NULL

write.csv(submit,"Submit_gbm_model.csv",row.names = FALSE)