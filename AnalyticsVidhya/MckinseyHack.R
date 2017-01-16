#Analytics Vidhya - Mckinsey Hiring Hack
#Author: Ganesh Babu Gajendiran
#Date: 23-APR-2016


#Importing necessary libraries
library(stringr)
library(sqldf)
library(randomForest)
library(plyr)
library(dplyr)
library(Boruta)
library(party)
library(xgboost)
library(caret)
library(gbm)

#Reading the train and test data set from csv file to data frame
if (!exists("train")) {
        train = read.csv("Train.csv",header = TRUE)        
}

if (!exists("test")) {
        test = read.csv("Test.csv",header = TRUE)
}


##################
#Merge train and test to perform data exploration, imputation together

tmp_test = test
tmp_test$Email_Status = -1
tmp_test$Source = 'Test'
tmp_train = train
tmp_train$Source = 'Train'
all_data = rbind(tmp_train,tmp_test)
rm(tmp_train,tmp_test)


##################

#Data Exploration
#Looking for na values in the data set

na_values = apply(all_data, MARGIN = 2, FUN = is.na)
apply(na_values, MARGIN = 2, FUN = sum) 
#Total_Past_Communications(11515),Total_Links(3624) and Total_Images(2770) are having NA values

#Looking for nan values in the data set

nan_values = apply(all_data, MARGIN = 2, FUN = is.nan)
apply(nan_values, MARGIN = 2, FUN = sum) #Data looks good with no nan values

#Looking for null values in the data set

apply(all_data, MARGIN = 2, FUN = is.null) #Data looks good with no null values

#Looking for empty string values in the training and test data set

empty_string_values = apply(all_data, MARGIN = 2, function(x) !(nchar(x) > 0) )
apply(empty_string_values, MARGIN = 2, FUN = sum) 
#Customer_Location is having 19438 empty string values

#looking for the count of unique values in the data set
apply(all_data, MARGIN = 2, FUN = function(x) length(unique(x)))

summary(all_data)

unique(all_data$Customer_Location)
hist(all_data$Customer_Location)
plot(all_data$Customer_Location)
plot(all_data)

sqldf("select distinct Email_Campaign_Type,Email_Source_Type from all_data where Customer_Location = '' ")

sqldf("select distinct Email_Campaign_Type,Email_Source_Type,Customer_Location from all_data 
      where Customer_Location <> '' and Email_Campaign_Type = 1 and Email_Source_Type = 1")

sqldf("select distinct Customer_Location from all_data")

all_valid_data = all_data[complete.cases(all_data),]

sqldf("select Source, count(1) from all_data group by Source")
sqldf("select Source, count(1) from all_valid_data group by Source")

t_train = train[complete.cases(train),]
t_test = test[complete.cases(test),]
nrow(t_train)
nrow(t_test)

boxplot(all_data[all_data$Subject_Hotness_Score <= 3.9,]$Subject_Hotness_Score)

boxplot(t_train[t_train$Subject_Hotness_Score <= 3.9,]$Subject_Hotness_Score)

train_lt39 = t_train[t_train$Subject_Hotness_Score <= 3.9,]
train_gt39 = t_train[t_train$Subject_Hotness_Score > 3.9,]



#Model Generation

all_mod = with(t_train,lm(Email_Status~Email_Type+Subject_Hotness_Score+Email_Source_Type
                          +Customer_Location+Email_Campaign_Type+Total_Past_Communications
                          +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images))
summary(all_mod)

#sames as all_mod Without customer_location
model1 = with(t_train,lm(Email_Status~Email_Type+Subject_Hotness_Score+Email_Source_Type
                          +Email_Campaign_Type+Total_Past_Communications
                          +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images))

summary(model1)

#sames as model1 Without Time_Email_sent_Category
model2 = with(t_train,lm(Email_Status~Email_Type+Subject_Hotness_Score+Email_Source_Type
                         +Email_Campaign_Type+Total_Past_Communications
                         +Word_Count+Total_Links+Total_Images))

summary(model2)

#Randomforest
rf1 = with(t_train,randomForest(Email_Status~Email_Type+Subject_Hotness_Score+Email_Source_Type
                         +Email_Campaign_Type+Total_Past_Communications
                         +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images,
                         importance=TRUE, ntree = 500))
summary(rf1)
varImpPlot(rf1)
rf1$importance

#same as rf1, without Subject_Hotness_Score
rf2 = with(t_train,randomForest(Email_Status~Email_Type+Email_Source_Type
                                +Email_Campaign_Type+Total_Past_Communications
                                +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images,
                                importance=TRUE, ntree = 500))

varImpPlot(rf2)


#same as rf2, without Time_Email_sent_Category
rf3 = with(t_train,randomForest(Email_Status~Email_Type+Email_Source_Type
                                +Email_Campaign_Type+Total_Past_Communications
                                +Word_Count+Total_Links+Total_Images,
                                importance=TRUE, ntree = 500))

varImpPlot(rf3)

#sames as rf1 but with 1000 trees instead of 500
rf4 = with(t_train,randomForest(Email_Status~Email_Type+Subject_Hotness_Score+Email_Source_Type
                                +Email_Campaign_Type+Total_Past_Communications
                                +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images,
                                importance=TRUE, ntree = 1000))


#RandomForest for two diff data set based on hotness score
#Randomforest
rf_l39 = with(train_lt39,randomForest(Email_Status~Email_Type+Subject_Hotness_Score+Email_Source_Type
                                +Email_Campaign_Type+Total_Past_Communications
                                +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images,
                                importance=TRUE, ntree = 500))

rf_gt39 = with(train_gt39,randomForest(Email_Status~Email_Type+Subject_Hotness_Score+Email_Source_Type
                                      +Email_Campaign_Type+Total_Past_Communications
                                      +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images,
                                      importance=TRUE, ntree = 500))

#Without Subject_Hotness_Score
rf_l39_1 = with(train_lt39,randomForest(Email_Status~Email_Type+Email_Source_Type
                                      +Email_Campaign_Type+Total_Past_Communications
                                      +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images,
                                      importance=TRUE, ntree = 500))

rf_gt39_1 = with(train_gt39,randomForest(Email_Status~Email_Type+Email_Source_Type
                                       +Email_Campaign_Type+Total_Past_Communications
                                       +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images,
                                       importance=TRUE, ntree = 500))
#Logistic regression since the reponse variable has only few (3) unique values

glm_model = glm(Email_Status~Email_Type+Subject_Hotness_Score+Email_Source_Type
                            +Customer_Location+Email_Campaign_Type+Total_Past_Communications
                            +Time_Email_sent_Category+Word_Count+Total_Links+Total_Images,data = t_train,
                family = binomial(link = "logit"))

summary(glm_model)

anova(glm_model, test="Chisq")

library(pscl)
pR2(glm_model)

test_lt39 = test[t_test$Subject_Hotness_Score <= 3.9,]
test_gt39 = test[t_test$Subject_Hotness_Score > 3.9,]


#Prediction
test_predict_l39_1 = predict(rf_l39_1,test_lt39)
test_predict_gt39_1 = predict(rf_gt39_1,test_gt39)

lt39 = test_lt39[,c(1,2)]
gt39 = test_gt39[,c(1,2)]
lt39 = cbind(lt39,Email_Status =test_predict_l39_1)
gt39 = cbind(gt39,Email_Status =test_predict_gt39_1)
submit = rbind(lt39,gt39)
submit = submit[,c(1,3)]
write.csv(submit,"Submit_rf_39_1.csv",row.names = FALSE)

test_predict = predict(rf1,test)
submit = test[,c(1,2)]
submit = cbind(submit,Email_Status = test_predict)
submit = submit[,c(1,3)]
rownames(submit) = NULL

write.csv(submit,"Submit_rf_39.csv",row.names = FALSE)