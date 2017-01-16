#Analytics Vidhya - Big mart Sales Prediction Challenge
#Author: Ganesh Babu Gajendiran
#Date: 24-MAR-2016


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
library(data.table)

#Reading the train and test data set from csv file to data frame
if (!exists("train")) {
        train = read.csv("Train_UWu5bXk.csv",header = TRUE)        
}

if (!exists("test")) {
        test = read.csv("Test_u94Q5KV.csv",header = TRUE)
}

train = fread("Train_UWu5bXk.csv",header = TRUE)        
test = fread("Test_u94Q5KV.csv",header = TRUE)

##################
##################

#Data Exploration
#Looking for na values in the training and test data set

valid_train = apply(train, MARGIN = 2, FUN = is.na)
apply(valid_train, MARGIN = 2, FUN = sum) #Only Item_Weight is having 1463 NA values

valid_test = apply(test, MARGIN = 2, FUN = is.na)
apply(valid_test, MARGIN = 2, FUN = sum) #Only Item_Weight is having 976 NA values

#Looking for nan values in the training and test data set

valid_train = apply(train, MARGIN = 2, FUN = is.nan)
apply(valid_train, MARGIN = 2, FUN = sum) #Data looks good with no nan values

valid_test = apply(test, MARGIN = 2, FUN = is.nan)
apply(valid_test, MARGIN = 2, FUN = sum) #Data looks good with no nan values

#Looking for null values in the training and test data set

apply(train, MARGIN = 2, FUN = is.null) #Data looks good with no null values

apply(test, MARGIN = 2, FUN = is.null) #Data looks good with no null values


#Looking for empty string values in the training and test data set

valid_train = apply(train, MARGIN = 2, function(x) !(nchar(x) > 0) )
apply(valid_train, MARGIN = 2, FUN = sum) #Outlet_Size is having 2410 empty string values

valid_test = apply(test, MARGIN = 2, function(x) !(nchar(x) > 0) )
apply(valid_test, MARGIN = 2, FUN = sum) #Outlet_Size is having 1606 empty string values


#looking for the count of unique values in the data set
apply(train, MARGIN = 2, FUN = function(x) length(unique(x)))

apply(test, MARGIN = 2, FUN = function(x) length(unique(x)))


#Looking for the unique value of few of the columns
#to see their distribution
#Item_Type - 16
#Outlet_Identifier - 10
#Outlet_Size - 4
#Outlet_Location_Type - 3
#Outlet_Type - 4

table(train$Item_Type)
table(train$Outlet_Identifier)
table(train$Outlet_Size) #2410 null values are present, which needs to be corrected.
table(train$Outlet_Location_Type)
table(train$Outlet_Type)

table(train$Outlet_Type,train$Outlet_Size)

table(train$Outlet_Type,train$Outlet_Size)

table(train$Item_Fat_Content)

#To get a general summary of the data
summary(train)
summary(test)


##################
##################

#Data Cleaning - Imputing missing values
#Based on the data exploration done Item_Weight and Outlet_Size has missing values


#Item_Weight
Valid_Item_Weight = sqldf('select distinct Item_Identifier,Item_Weight from train where Item_Weight is not null')
Missing_Item_Weight = sqldf('select distinct Item_Identifier from train where Item_Weight is null')

#Taking counts to make sure that there arent any duplicates
tmp = sqldf('select count(distinct Item_Identifier),count(1) from Valid_Item_Weight') #count is 1555 for both.


tmp = sqldf('select count(1) from Valid_Item_Weight V,Missing_Item_Weight M 
      where M.Item_Identifier = V.Item_Identifier')
#count is 1138

#To find the missing values that still does not have any valid value
tmp = sqldf(' select Item_Identifier from Missing_Item_Weight where 
      Item_Identifier not in (select Item_Identifier from Valid_Item_Weight)')
#  Item_Identifier for which valid values could not be found.
#FDN52 , FDK57, FDE52, FDQ60

#The above mentioned missing weights have valid values in test set?
temp_test = sqldf('select distinct Item_Identifier,Item_Weight from test 
      where Item_Identifier in ("FDN52","FDK57","FDE52","FDQ60")
      and Item_Weight is not null')

#Adding the missing values in train that were found in test
dim(Valid_Item_Weight) #1555,2
dim(temp_test) #4,2

Valid_Item_Weight = rbind(Valid_Item_Weight,temp_test)
dim(Valid_Item_Weight) #1555,2

#To re-verify the missing values that still does not have any valid value
tmp = sqldf(' select Item_Identifier from Missing_Item_Weight where 
      Item_Identifier not in (select Item_Identifier from Valid_Item_Weight)')
#no rows - AS EXPECTED



valid_train = apply(new_train, MARGIN = 2, FUN = is.na)
apply(valid_train, MARGIN = 2, FUN = sum) #Only Item_Weight is having 1463 NA values

valid_test = apply(new_test, MARGIN = 2, FUN = is.na)
apply(valid_test, MARGIN = 2, FUN = sum) #Only Item_Weight is having 976 NA values


new_train = train


#Updating the Item_Weight with valid values based on Item_Identifier for train data set
nrow(new_train[is.na(new_train$Item_Weight),])

new_train[is.na(new_train$Item_Weight),]$Item_Weight = Valid_Item_Weight[match(new_train[is.na(new_train$Item_Weight),]$Item_Identifier,Valid_Item_Weight$Item_Identifier),2]

Valid_Item_Weight1 = sqldf('select distinct Item_Identifier,Item_Weight from new_train where Item_Weight is not null')

Valid_Item_Weight = Valid_Item_Weight[order(Valid_Item_Weight$Item_Identifier),]
rownames(Valid_Item_Weight) = NULL
Valid_Item_Weight1 = Valid_Item_Weight1[order(Valid_Item_Weight1$Item_Identifier),]
rownames(Valid_Item_Weight1) = NULL

identical(Valid_Item_Weight,Valid_Item_Weight1)

#Updating the Item_Weight with valid values based on Item_Identifier for test data set

new_test = test

nrow(new_test[is.na(new_test$Item_Weight),])

new_test[is.na(new_test$Item_Weight),]$Item_Weight = Valid_Item_Weight[match(new_test[is.na(new_test$Item_Weight),]$Item_Identifier,Valid_Item_Weight$Item_Identifier),2]

Valid_Item_Weight_t1 = sqldf('select distinct Item_Identifier,Item_Weight from new_test where Item_Weight is not null')
Valid_Item_Weight_t = sqldf('select distinct Item_Identifier,Item_Weight from test where Item_Weight is not null')

Valid_Item_Weight_t = Valid_Item_Weight_t[order(Valid_Item_Weight_t$Item_Identifier),]
rownames(Valid_Item_Weight_t) = NULL
Valid_Item_Weight_t1 = Valid_Item_Weight_t1[order(Valid_Item_Weight_t1$Item_Identifier),]
rownames(Valid_Item_Weight_t1) = NULL

identical(Valid_Item_Weight_t,Valid_Item_Weight_t1)

#Outlet_Size

#train,test missing value 2410,1606

sqldf('select count(1) from train 
      where Outlet_Size != ""')

sqldf('select distinct Outlet_Identifier,Outlet_Size from train 
                          where Outlet_Size != "" order by Outlet_Identifier')

sqldf('select distinct Outlet_Identifier,Outlet_Size from test 
                          where Outlet_Size != "" order by Outlet_Identifier')

sqldf('select distinct Outlet_Identifier from train 
                            where Outlet_Size = "" order by Outlet_Identifier')

sqldf('select distinct Outlet_Identifier from test 
                            where Outlet_Size = "" order by Outlet_Identifier')

sqldf('select distinct Outlet_Type,Outlet_Size from train 
                          where Outlet_Size != "" order by Outlet_Type')

sqldf('select distinct Outlet_Type,Outlet_Size from test 
                          where Outlet_Size != "" order by Outlet_Type')

sqldf('select distinct Outlet_Type,Outlet_Size from train 
                            where Outlet_Size = "" order by Outlet_Type')

sqldf('select distinct Outlet_Type,Outlet_Size from test 
                            where Outlet_Size = "" order by Outlet_Type')

#For all "Grocery Store" the outlet size is "Small" in both train and test
# However for Supermarket Type1, it can be either Small, Medium or High
# Need to find out what value needs to be assigned.

Valid_Outlet_Size = sqldf('select distinct Outlet_Identifier,Outlet_Size from train 
                          where Outlet_Size != "" order by Outlet_Identifier')
Missing_Outlet_Size = sqldf('select distinct Outlet_Identifier from train 
                            where Outlet_Size = "" order by Outlet_Identifier')


#Updating the outlet size to "Small" for Outlet_type of "Grocery Store" 
new_train[new_train$Outlet_Size =="" & new_train$Outlet_Type =="Grocery Store",]$Outlet_Size = "Small"
new_test[new_test$Outlet_Size =="" & new_test$Outlet_Type =="Grocery Store",]$Outlet_Size = "Small"


nrow(new_train[new_train$Outlet_Size =="",]) #1855
nrow(new_test[new_test$Outlet_Size =="",]) #1236

nrow(new_train[new_train$Outlet_Size ==""& new_train$Outlet_Type =="Grocery Store",]) #0
nrow(new_test[new_test$Outlet_Size ==""& new_test$Outlet_Type =="Grocery Store",]) #0


nrow(train[train$Outlet_Size ==""& train$Outlet_Type =="Grocery Store",]) #0
nrow(test[test$Outlet_Size ==""& test$Outlet_Type =="Grocery Store",]) #0

valid_new_train = apply(new_train, MARGIN = 2, function(x) !(nchar(x) > 0) )
apply(valid_train, MARGIN = 2, FUN = sum) #Outlet_Size is having 2410 empty string values

valid_new_test = apply(new_test, MARGIN = 2, function(x) !(nchar(x) > 0) )
apply(valid_test, MARGIN = 2, FUN = sum) 


sqldf('select distinct Outlet_Identifier,Outlet_Size,Outlet_Location_Type,Outlet_Type from train 
                            where Outlet_Size = "" order by Outlet_Identifier')

sqldf('select distinct Outlet_Identifier,Outlet_Size,Outlet_Location_Type,Outlet_Type from train 
                            where Outlet_Size != "" order by Outlet_Identifier')

sqldf('select distinct Outlet_Size,Outlet_Location_Type,Outlet_Type from train 
      where Outlet_Size != "" order by Outlet_Identifier')

sqldf('select distinct Outlet_Size,Outlet_Location_Type,Outlet_Type from train 
      where Outlet_Size = "" order by Outlet_Identifier')

sqldf('select distinct Outlet_Size,Outlet_Location_Type,Outlet_Type from test 
      where Outlet_Size = "" order by Outlet_Identifier')

#Null values for outlet_size for "Supermarket Type1" are all in "Tier 2" location type
#Are there any pattern for such data?

sqldf('select distinct Outlet_Size,Outlet_Location_Type,Outlet_Type from train 
      where Outlet_Size = "" and Outlet_Type = "Supermarket Type1" order by Outlet_Identifier')

sqldf('select distinct Outlet_Size,Outlet_Location_Type,Outlet_Type from test 
      where Outlet_Size = "" and Outlet_Type = "Supermarket Type1" order by Outlet_Identifier')


sqldf('select distinct Outlet_Size,Outlet_Location_Type,Outlet_Type from train 
      where Outlet_Size != "" and Outlet_Type = "Supermarket Type1" order by Outlet_Identifier')

sqldf('select distinct Outlet_Size,Outlet_Location_Type,Outlet_Type from test 
      where Outlet_Size != "" and Outlet_Type = "Supermarket Type1" order by Outlet_Identifier')

#For "Tier 2" and "Supermarket Type1" the outlet size is always "Small"
#but for how many?

sqldf('select Outlet_Size,Outlet_Location_Type,Outlet_Type,count(1) from train 
      where Outlet_Location_Type = "Tier 2" and Outlet_Type = "Supermarket Type1" 
      group by Outlet_Size,Outlet_Location_Type,Outlet_Type
      order by Outlet_Identifier')

sqldf('select Outlet_Size,Outlet_Location_Type,Outlet_Type,count(1) from test 
      where Outlet_Location_Type = "Tier 2" and Outlet_Type = "Supermarket Type1" 
      group by Outlet_Size,Outlet_Location_Type,Outlet_Type
      order by Outlet_Identifier')


sqldf('select Outlet_Size,Outlet_Location_Type,Outlet_Type,count(1) from new_train 
      where Outlet_Location_Type = "Tier 2" and Outlet_Type = "Supermarket Type1" 
      group by Outlet_Size,Outlet_Location_Type,Outlet_Type
      order by Outlet_Identifier')

sqldf('select Outlet_Size,Outlet_Location_Type,Outlet_Type,count(1) from new_test 
      where Outlet_Location_Type = "Tier 2" and Outlet_Type = "Supermarket Type1" 
      group by Outlet_Size,Outlet_Location_Type,Outlet_Type
      order by Outlet_Identifier')


#Updating the outlet size to "Small" for Outlet_type of "Supermarket Type1" 
#and Outlet_Location_Type = "Tier 2"
new_train[new_train$Outlet_Size =="" & new_train$Outlet_Type =="Supermarket Type1" & new_train$Outlet_Location_Type == "Tier 2",]$Outlet_Size = "Small"
new_test[new_test$Outlet_Size =="" & new_test$Outlet_Type =="Supermarket Type1" & new_test$Outlet_Location_Type == "Tier 2",]$Outlet_Size = "Small"


nrow(new_train[new_train$Outlet_Size =="",]) #0
nrow(new_test[new_test$Outlet_Size =="",]) #0




#Validating the data again to make sure no NULL values or empty strings are present 
#for any of the variables in both the new_train and new_test data frame.

valid_train = apply(new_train, MARGIN = 2, FUN = is.na)
apply(valid_train, MARGIN = 2, FUN = sum) #Only Item_Weight is having 1463 NA values

valid_test = apply(new_test, MARGIN = 2, FUN = is.na)
apply(valid_test, MARGIN = 2, FUN = sum) #Only Item_Weight is having 976 NA values

valid_train = apply(new_train, MARGIN = 2, function(x) !(nchar(x) > 0) )
apply(valid_train, MARGIN = 2, FUN = sum) #no empty string values present

valid_test = apply(new_test, MARGIN = 2, function(x) !(nchar(x) > 0) )
apply(valid_test, MARGIN = 2, FUN = sum) #no empty string values present




#Feature Engineering

#Creating new column for outlets years in service instead of year of establishment
#since the service years will create a impact instead of year of establishment
#2013 has been used instead of 2016(current year), since the problem statement mentioned
#that the data was of the year 2013
new_train$Outlet_Yearsin_Service = as.integer(2013) - new_train$Outlet_Establishment_Year
new_test$Outlet_Yearsin_Service = as.integer(2013) - new_test$Outlet_Establishment_Year


#Update Item_Fat_Content to have same category of values for misspelled ones

table(new_train$Item_Fat_Content)

new_train[new_train$Item_Fat_Content=='LF' | new_train$Item_Fat_Content=='low fat',]$Item_Fat_Content = "Low Fat"
new_train[new_train$Item_Fat_Content=='reg',]$Item_Fat_Content = "Regular"

new_train$Item_Fat_Content = as.character(new_train$Item_Fat_Content)
new_train$Item_Fat_Content = as.factor(new_train$Item_Fat_Content)
table(new_train$Item_Fat_Content)

table(new_test$Item_Fat_Content)

new_test[new_test$Item_Fat_Content=='LF' | new_test$Item_Fat_Content=='low fat',]$Item_Fat_Content = "Low Fat"
new_test[new_test$Item_Fat_Content=='reg',]$Item_Fat_Content = "Regular"

new_test$Item_Fat_Content = as.character(new_test$Item_Fat_Content)
new_test$Item_Fat_Content = as.factor(new_test$Item_Fat_Content)
table(new_test$Item_Fat_Content)

#Add one more column for broad category of food item based on first 2 characters of product id

new_train$Item_Category = str_sub(new_train$Item_Identifier,1,2)
new_test$Item_Category = str_sub(new_test$Item_Identifier,1,2)

table(new_train$Item_Category)
table(new_test$Item_Category)

#Update Item_Visibility since there are values as Zero (0.0000) which doesnt make sense
#since the product cannot be invisible

nrow(new_train[new_train$Item_Visibility==0,]) #526
nrow(new_test[new_test$Item_Visibility==0,]) #353
#total 879 (526+353)

match = sqldf("select distinct Item_Identifier,Outlet_Identifier from new_train
      where Item_Visibility = 0 
      intersect 
      select distinct Item_Identifier,Outlet_Identifier from new_train
      where Item_Visibility != 0")
#none match based on Item_Identifier,Outlet_Identifier alone in training data set


Invalid_Item_Out_id = sqldf("select distinct Item_Identifier,Outlet_Identifier from new_train
              where Item_Visibility = 0 
                union
        select distinct Item_Identifier,Outlet_Identifier from new_test 
              where Item_Visibility = 0")
               
Valid_Item_Out_id = sqldf ("select distinct Item_Identifier,Outlet_Identifier from new_train
              where Item_Visibility != 0
              union
              select distinct Item_Identifier,Outlet_Identifier from new_test
              where Item_Visibility != 0")

match = sqldf("select distinct Item_Identifier,Outlet_Identifier from Valid_Item_Out_id
      intersect 
      select distinct Item_Identifier,Outlet_Identifier from Invalid_Item_Out_id
      ")

#none matched between train and test based on item and outlet identifiers.


Invalid_Items = sqldf("select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type from new_train
              where Item_Visibility = 0 
                union
        select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type from new_test 
              where Item_Visibility = 0")
nrow(Invalid_Items) #865

Invalid_Items = sqldf("select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type 
from Invalid_Items")

nrow(Invalid_Items) #865

Valid_Items = sqldf ("select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type from new_train
              where Item_Visibility != 0
              union
              select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type from new_test
              where Item_Visibility != 0")

nrow(Valid_Items)#10512

Valid_Items = sqldf("select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type 
from Valid_Items")

nrow(Valid_Items)#10512

match = sqldf("select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type 
        from Invalid_Items
        intersect 
        select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type 
        from Valid_Items")
#255 match found


#missing values

miss = sqldf("select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type 
        from Invalid_Items 
        except 
        select distinct Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type 
        from Valid_Items")
#610 values are missing


tmp_train_valid_Items = 
sqldf ("select  Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type,Item_Visibility 
        from new_train
        where Item_Visibility != 0
        union
        select Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type,Item_Visibility 
        from new_test
        where Item_Visibility != 0")

dup_test = sqldf("select Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type,count(distinct Item_Visibility)
                 from tmp_train_valid_Items
                 group by Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type
                 having count(distinct Item_Visibility) > 1")




#######################
#Analysis for few data set

tmp = sqldf("select Item_Identifier,Outlet_Identifier,Item_Visibility,Outlet_Size,Outlet_Type,
        Outlet_Location_Type,'train' as Dataset from new_train
        where Item_Identifier = 'DRA12'
        union 
        select Item_Identifier,Outlet_Identifier,Item_Visibility,Outlet_Size,Outlet_Type,
        Outlet_Location_Type,'test' as Dataset from new_test
        where Item_Identifier = 'DRA12'")


sqldf("select * from tmp where Outlet_Size = 'Small' and Outlet_Location_Type = 'Tier 2'")
#There is one row with non-zero item visibility for the same outlet location type, 
#outlet type, outlet size for DRA12 - 0.04117751

tmp1 = sqldf("select Item_Identifier,Outlet_Identifier,Item_Visibility,Outlet_Size,Outlet_Type,
        Outlet_Location_Type,'train' as Dataset from new_train
        where Item_Identifier = 'DRC13'
        union 
        select Item_Identifier,Outlet_Identifier,Item_Visibility,Outlet_Size,Outlet_Type,
        Outlet_Location_Type,'test' as Dataset from new_test
        where Item_Identifier = 'DRC13'")

sqldf("select * from tmp1 where Outlet_Size = 'Small' and Outlet_Location_Type = 'Tier 2'")
#There are two rows with non-zero item visibility for the same outlet location type, 
#outlet type, outlet size for DRC13
#Since there are more than one values, taking mean would prove beneficial.
#mean(0.03262507,0.03243544) 0.03262507


#Creating a data frame with the mean value for Item_Visibility for each group of
#Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type
#to be used as a lookup table for updating the 0's in the train,test data frame

visib_avg_value = sqldf("select Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type, avg(Item_Visibility) as Item_Visib_AVG from tmp_train_valid_Items
                        where Item_Visibility != 0
                        group by Item_Identifier,Outlet_Size,Outlet_Type,Outlet_Location_Type")
#10512 entries

#Updating the training data set for the visibility values.
tmp_train = new_train

nrow(tmp_train[tmp_train$Item_Visibility==0,]) #526

tmp_train = merge(visib_avg_value,tmp_train, by = c('Item_Identifier','Outlet_Size','Outlet_Type','Outlet_Location_Type'), all.y = TRUE)

sum(is.na(tmp_train$Item_Visib_AVG))

tmp_train[is.na(tmp_train$Item_Visib_AVG),]$Item_Visib_AVG = 0

tmp_train = tmp_train %>% mutate(Final_Item_Visibility = ifelse(tmp_train$Item_Visibility == 0 & tmp_train$Item_Visib_AVG !=0, tmp_train$Item_Visib_AVG, tmp_train$Item_Visibility))
tmp_train$Item_Visibility = tmp_train$Final_Item_Visibility

nrow(tmp_train[tmp_train$Final_Item_Visibility==0,])#393

nrow(tmp_train[tmp_train$Item_Visibility==0,])
tmp_train = tmp_train[,c(1,6,7,8,9,10,11,12,2,4,3,13,14,15)]

sum(is.na(t$Item_Visib_AVG))

#Updating the test data set for the visibility values.

tmp_test = new_test

nrow(tmp_test[tmp_test$Item_Visibility==0,]) #353

tmp_test = merge(visib_avg_value,tmp_test, by = c('Item_Identifier','Outlet_Size','Outlet_Type','Outlet_Location_Type'), all.y = TRUE)

sum(is.na(tmp_train$Item_Visib_AVG))

tmp_test[is.na(tmp_test$Item_Visib_AVG),]$Item_Visib_AVG = 0

tmp_test = tmp_test %>% mutate(Final_Item_Visibility = ifelse(tmp_test$Item_Visibility == 0 & tmp_test$Item_Visib_AVG !=0, tmp_test$Item_Visib_AVG, tmp_test$Item_Visibility))
tmp_test$Item_Visibility = tmp_test$Final_Item_Visibility

nrow(tmp_test[tmp_test$Final_Item_Visibility==0,])#274

nrow(tmp_test[tmp_test$Item_Visibility==0,])
tmp_test = tmp_test[,c(1,6,7,8,9,10,11,12,2,4,3,13,14)]


#Still 393 rows having the visibility as 0 in training data set
#Finding how many unique such products are present.

length(unique(tmp_train[tmp_train$Item_Visibility==0,]$Item_Identifier))#345 such products

sqldf("select distinct Item_Identifier from tmp_train where Item_Visibility = 0
      except 
      select distinct Item_Identifier from tmp_train where Item_Visibility != 0")

head(sqldf("select Item_Identifier,Outlet_Type,Outlet_Location_Type,Outlet_Size from tmp_train where Item_Visibility = 0
group by Item_Identifier,Outlet_Type,Outlet_Location_Type,Outlet_Size
      except 
      select  Item_Identifier,Outlet_Type,Outlet_Location_Type,Outlet_Size from tmp_train where Item_Visibility != 0
      group by Item_Identifier,Outlet_Type,Outlet_Location_Type,Outlet_Size"))

#Creating a data frame with the mean visibility for that product

Valid_Items = sqldf ("select Item_Identifier,Item_Visibility from tmp_train
              where Item_Visibility != 0
                     union
                     select  Item_Identifier,Item_Visibility from tmp_test
                     where Item_Visibility != 0")

visib_avg_item = sqldf("select Item_Identifier,avg(Item_Visibility) as Item_Visib_AVG from Valid_Items
                        group by Item_Identifier") 


nrow(tmp_train[tmp_train$Item_Visibility==0,])#393

nrow(tmp_test[tmp_test$Item_Visibility==0,])#274

v_temp = sqldf("select Item_Identifier,Item_Visibility from tmp_train
               union select Item_Identifier,Item_Visibility from tmp_test")

v_temp_visib = with(v_temp,aggregate(Item_Visibility, data=v_temp, FUN = mean, by = list(Item_Identifier)))
names(v_temp_visib) = c("Item_Identifier","Avg_Visibility")

#Steps to update the visibility for those which are 0 in both train and test data set

tmp_train_t = merge(tmp_train,v_temp_visib, by = c('Item_Identifier'), all.x = TRUE)

tmp_train_t = tmp_train_t %>% mutate(Final_Item_Visibility = ifelse(tmp_train_t$Item_Visibility == 0 & tmp_train_t$Avg_Visibility !=0, tmp_train_t$Avg_Visibility, tmp_train_t$Item_Visibility))
tmp_train_t$Item_Visibility = tmp_train_t$Final_Item_Visibility

nrow(tmp_train_t[tmp_train_t$Final_Item_Visibility==0,])#0
nrow(tmp_train_t[tmp_train_t$Item_Visibility==0,])#0

tmp_train_t = tmp_train_t[,c(1:14)]
tmp_train = tmp_train_t
rm(tmp_train_t)

tmp_test_t = merge(tmp_test,v_temp_visib, by = c('Item_Identifier'), all.x = TRUE)

tmp_test_t = tmp_test_t %>% mutate(Final_Item_Visibility = ifelse(tmp_test_t$Item_Visibility == 0 & tmp_test_t$Avg_Visibility !=0, tmp_test_t$Avg_Visibility, tmp_test_t$Item_Visibility))
tmp_test_t$Item_Visibility = tmp_test_t$Final_Item_Visibility

nrow(tmp_test_t[tmp_test_t$Final_Item_Visibility==0,])#0
nrow(tmp_test_t[tmp_test_t$Item_Visibility==0,])#0

tmp_test_t = tmp_test_t[,c(1:13)]
tmp_test = tmp_test_t
rm(tmp_test_t)

nrow(tmp_train[tmp_train$Item_Visibility==0,])#0

nrow(tmp_test[tmp_test$Item_Visibility==0,])#0

#updating the fat content to Non-Edible for Non consumables - Item_Category(NC)
tmp_train$Item_Fat_Content = as.character(tmp_train$Item_Fat_Content)
tmp_train[tmp_train$Item_Category == 'NC',]$Item_Fat_Content = 'Non-Edible'
tmp_train$Item_Fat_Content = as.factor(tmp_train$Item_Fat_Content)

tmp_test$Item_Fat_Content = as.character(tmp_test$Item_Fat_Content)
tmp_test[tmp_test$Item_Category == 'NC',]$Item_Fat_Content = 'Non-Edible'
tmp_test$Item_Fat_Content = as.factor(tmp_test$Item_Fat_Content)

#Creating an empty data frame for storing the results of different models.
model_summary = data.frame(reg_func = character(), ResStdErr = numeric(), df = numeric(),RSquared = numeric(),AdjRSqu = numeric(),F_stat = numeric(), Comments = character())

#Function to gather the model summary stats into a data frame
model_f = function(df,srce,typ)
{
        if(typ=='lm') 
        {  tmp_df = data.frame(reg_func = as.character(df$call)[2],
                      ResStdErr = round(df$sigma,4), 
                      df = df$df[2],
                      RSquared = round(df$r.squared*100,2),
                      AdjRSqu = round(df$adj.r.squared*100,2),
                      F_stat = df$fstatistic[1],
                      Source = srce
                      )
        } else if (typ == 'rf')
        {
                tmp_df = data.frame(reg_func = as.character(df$call)[2],
                                    ResStdErr = round(mean(df$mse),4), 
                                    df = 0,
                                    RSquared = round(mean(df$rsq)*100,2),
                                    AdjRSqu = 0,
                                    F_stat = 0,
                                    Source = srce
                )
        }
  return(tmp_df)
}

summary(tmp_train)

boxplot(tmp_train[,c(2:3)])

#Model Generation - Multiple Linear Regression
#tmp_train and tmp_test will be used for the regression

all_mod = with(tmp_train,lm(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category))
d=summary(all_mod)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,all_mod','lm'))

model1 = with(tmp_train,lm(Item_Outlet_Sales~Item_Identifier))
d=summary(model1)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model1','lm'))

model2 = with(tmp_train,lm(Item_Outlet_Sales~Item_Identifier+Item_Fat_Content+Item_Visibility+Outlet_Yearsin_Service+Item_Category))
d=summary(model2)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model2','lm'))

model3 = with(tmp_train,lm(Item_Outlet_Sales~Item_Identifier+Item_Visibility+Outlet_Yearsin_Service+Item_Category))
d=summary(model3)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model3','lm'))

model4 = with(tmp_train,lm(Item_Outlet_Sales~Item_Identifier+Item_Visibility+Outlet_Yearsin_Service+Item_Category))
d=summary(model4)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model4','lm'))

model5 = with(tmp_train,lm(Item_Outlet_Sales~Item_Identifier+Outlet_Yearsin_Service+Item_Category))
d=summary(model5)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model5','lm'))

model6 = with(tmp_train,lm(Item_Outlet_Sales~Item_Identifier+Item_Category))
d=summary(model6)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model6','lm'))

model7 = with(tmp_train,lm(Item_Outlet_Sales~Item_Weight))
d=summary(model7)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model7','lm'))

model8 = with(tmp_train,lm(Item_Outlet_Sales~Item_Fat_Content))
d=summary(model8)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model8','lm'))

model9 = with(tmp_train,lm(Item_Outlet_Sales~Item_Visibility))
d=summary(model)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model9','lm'))

model10 = with(tmp_train,lm(Item_Outlet_Sales~Item_Type))
d=summary(model10)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model10','lm'))

model11 = with(tmp_train,lm(Item_Outlet_Sales~Item_MRP))
d=summary(model11)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model11','lm'))

model12 = with(tmp_train,lm(Item_Outlet_Sales~Outlet_Identifier))
d=summary(model12)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model12','lm'))

model13 = with(tmp_train,lm(Item_Outlet_Sales~Outlet_Size))
d=summary(model13)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model13','lm'))

model14 = with(tmp_train,lm(Item_Outlet_Sales~Outlet_Location_Type))
d=summary(model14)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model14','lm'))

model15 = with(tmp_train,lm(Item_Outlet_Sales~Outlet_Type))
d=summary(model15)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model15','lm'))

model16 = with(tmp_train,lm(Item_Outlet_Sales~Outlet_Yearsin_Service))
d=summary(model16)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model16','lm'))

model17 = with(tmp_train,lm(Item_Outlet_Sales~Item_Category))
d=summary(model17)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model17','lm'))

model18 = with(tmp_train,lm(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category))
d=summary(model18)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model18','lm'))

model19 = with(tmp_train,glm(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category))
d=summary(model19)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,Gen linear model,model19','lm'))

model20 = with(tmp_train,lm(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category))
d=summary(model20)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model20','lm'))

model21 = with(tmp_train,lm(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service))
d=summary(model21)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model21','lm'))

model22 = with(tmp_train,lm(Item_Outlet_Sales~Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service))
d=summary(model22)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model22','lm'))

model23 = with(tmp_train,lm(Item_Outlet_Sales~Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service))
d=summary(model23)
model_summary = rbind(model_summary,model_f(d,'df=tmp_train,linear model,model23','lm'))


#Cross validation - ten fold
library(DAAG)
cv = cv.lm(tmp_train, model, m=10) # ten-fold cross validation

        
#Boruta for identifying the significant attributes
set.seed(123)
boruta.train = Boruta(Item_Outlet_Sales~., data = tmp_train, doTrace = 2)
boruta.train
#Boruta performed 99 iterations in 56.898 mins.
#12 attributes confirmed important: Item_Category, Item_Identifier, Item_MRP,
#Item_Type, Item_Visibility and 7 more.
#No attributes deemed unimportant.
#1 tentative attributes left: Item_Fat_Content.
getSelectedAttributes(boruta.train, withTentative = F)

boruta.train.1 = Boruta(Item_Outlet_Sales~.-Outlet_Establishment_Year, data = tmp_train, doTrace = 2)
boruta.train.1
getSelectedAttributes(boruta.train.1, withTentative = F)

final.boruta <- TentativeRoughFix(boruta.train)
final.boruta


#Generating a randomForest model
tmp_train$Item_Category = as.factor(tmp_train$Item_Category)
#Item identifier not used for RF since it has more than 53 levels (1559 levels).
set.seed(540)
rf = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, importance=TRUE, ntree = 2000))
#varImpPlot(M_rf)
summary(rf)
varImpPlot(rf)
rf$importance
model_summary = rbind(model_summary,model_f(rf,'df=tmp_train,random forest,rf','rf'))

rf1_n2000 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, importance=TRUE, ntree = 2000))
summary(rf1_n2000)
varImpPlot(rf1_n2000)
rf1_n2000$importance
model_summary = rbind(model_summary,model_f(rf1_n2000,'df=tmp_train,random forest,rf','rf'))

rf2_n2000 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2000))
rf2_n2000$importance

rf3_n2000 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2000))
rf3_n2000$importance
varImpPlot(rf3_n2000)

rf4_n2000 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2000))
rf4_n2000$importance

rf3_n500 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 500))
rf3_n500$importance

rf3_n5000 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 5000))
rf3_n5000$importance

rf3_n1000 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 1000))
rf3_n1000$importance

rf3_n2000mtry5 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2000, mtry = 5))
rf3_n2000mtry5$importance
varImpPlot(rf3_n2000mtry5)

rf3_n2000mtry3 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2000, mtry = 3))
rf3_n2000mtry3$importance
varImpPlot(rf3_n2000mtry3)

rf3_n2500mtry3 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2500, mtry = 3))
rf3_n2500mtry5 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2500, mtry = 5))

rf3_n2500mtry3$importance
rf3_n2500mtry5$importance
par(mfrow=c(2,2))
varImpPlot(rf3_n2500mtry5)
varImpPlot(rf3_n2500mtry3)

rf3_n2300mtry3 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2300, mtry = 3))
rf3_n2300mtry5 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2300, mtry = 5))


rf3_n1800mtry3 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 1800, mtry = 3))
rf3_n1900mtry3 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 1900, mtry = 3))
rf3_n2100mtry3 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2100, mtry = 3))
rf3_n2200mtry3 = with(tmp_train,randomForest(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, importance=TRUE, ntree = 2200, mtry = 3))

#Conditional inference trees
set.seed(540)
ci_n2_mtry3 = cforest(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, data = tmp_train, controls=cforest_unbiased(ntree=2, mtry=3))
#runs for a very long time. Once ran for more than 6 hours and still did not complete.
ci_n20_mtry3 = cforest(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, data = tmp_train, controls=cforest_unbiased(ntree=20, mtry=3))


#one hot encoding for xgboost models
t_tr = cbind(tmp_train,Source = 'Train')
t_te = cbind(test_processed,Source = 'Test')
all_data = rbind(t_tr,t_te)

all_data$Item_Fat_Content_n = as.numeric(all_data$Item_Fat_Content)
all_data$Item_Type_n = as.numeric(all_data$Item_Type)
all_data$Outlet_Identifier_n = as.numeric(all_data$Outlet_Identifier)
all_data$Item_Identifier_n = as.numeric(all_data$Item_Identifier)
all_data$Outlet_Size_n = as.numeric(all_data$Outlet_Size)
all_data$Outlet_Location_Type_n = as.numeric(all_data$Outlet_Location_Type)
all_data$Outlet_Type_n = as.numeric(all_data$Outlet_Type)
all_data$Item_Category_n = as.numeric(all_data$Item_Category)
all_data=all_data[,c(19,2,16,4,17,6,18,20,21,22,23,13,12,15)]

train_processed_xg = all_data[all_data$Source=='Train',-c(14)]
test_processed_xg = all_data[all_data$Source=='Test',-c(14)]
rownames(train_processed_xg) = NULL
rownames(test_processed_xg) = NULL


#GBM
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
set.seed(99)
gbm = train(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, data = tmp_train, method = 'gbm',trcontrol = fitControl,verbose = FALSE)
gbm = train(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, data = tmp_train, method = 'gbm')



tr_control = trainControl(method = "cv", number = 5)
tr_grid = expand.grid(n.trees = seq(1:20)*10,interaction.depth = c(5,15,30),shrinkage = 0.1, n.minobsinnode = 10)

gbm_model = train(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, data = tmp_train,
               method="gbm",tuneGrid = tr_grid,trControl = tr_control)
#the above model generation took rougly 1.5 hour for execution. not sure though.

plot(gbm_model)

set.seed(99)
tr_control1 = trainControl(method = "repeatedcv", number = 5)
tr_grid1 = expand.grid(n.trees = seq(1:20)*10,interaction.depth = c(5,15,30),shrinkage = 0.1, n.minobsinnode = 10)

#Same as gbm_model, except for the method changed from cv to repeatedcv
gbm_model1 = train(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, data = tmp_train,
                  method="gbm",tuneGrid = tr_grid1,trControl = tr_control1)
#the above model generation took rougly 2 hours for execution

plot(gbm_model1)


set.seed(99)
tr_control2 = trainControl(method = "cv", number = 5)
tr_grid2 = expand.grid(n.trees = seq(1:20)*20,interaction.depth = c(5,15,30),shrinkage = 0.1, n.minobsinnode = 10)
#Same as gbm_model, except for the n.trees increased from a factor of 10 to 20
gbm_model2 = train(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, data = tmp_train,
                  method="gbm",tuneGrid = tr_grid2,trControl = tr_control2)
plot(gbm_model2)

#Running now
set.seed(99)
tr_control3 = trainControl(method = "cv", number = 5)
tr_grid3 = expand.grid(n.trees = seq(1:20)*10,interaction.depth = c(5,15,30),shrinkage = 0.1, n.minobsinnode = 10)

#Same as gbm_model except that the Item_Category has been removed as one of the 
#predictors, since it is highly correlated with Item_Identifier
gbm_model3 = train(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service, data = tmp_train,
                   method="gbm",tuneGrid = tr_grid2,trControl = tr_control2)

#Yet to be run
tr_control4 = trainControl(method = "cv", number = 5)
tr_grid4 = expand.grid(n.trees = seq(1:20)*40,interaction.depth = c(5,15,30),shrinkage = 0.1, n.minobsinnode = 10)
#Same as gbm_model, except for the n.trees increased from a factor of 20 to 50
#Increasing the n.trees from 10 to 20 had increased the lb score 1 point.
#so does more n.trees means a better model?
gbm_model4 = train(Item_Outlet_Sales~Item_Identifier+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Outlet_Yearsin_Service+Item_Category, data = tmp_train,
                   method="gbm",tuneGrid = tr_grid2,trControl = tr_control2)
plot(gbm_model4)


set.seed(100)
xgb <- xgboost(data = train_processed_xg,label = NULL,eta = 0.1,max_depth = 15,nround=25,subsample = 0.5,colsample_bytree = 0.5,seed = 1,eval_metric = "merror",objective = "multi:softprob",num_class = 12,nthread = 3)
zero.var = nearZeroVar(train_processed_xg, saveMetrics=TRUE)


test_processed = tmp_test
test_processed$Item_Outlet_Sales = 0
test_processed$Item_Category = as.factor(test_processed$Item_Category)
test_processed = test_processed[,c(1,2,3,4,5,6,7,8,9,10,11,14,13,12)]

#Predicting the sales based on the best model
#test_predict = predict(all_mod,tmp_test)
#test_predict = predict(model23,tmp_test)
#test_predict = predict(ci_n2_mtry3,test_processed, OOB=TRUE)
test_predict = predict(gbm_model2,test_processed)


submit = test_processed[,c(1,7)]
submit = cbind(submit,Item_Outlet_Sales = test_predict)
rownames(submit) = NULL

write.csv(submit,"Submit_gbm_model2.csv",row.names = FALSE)