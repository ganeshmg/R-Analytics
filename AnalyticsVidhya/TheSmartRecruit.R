#Analytics Vidhya - The smart recruits
#Author: Ganesh Babu Gajendiran
#Date: 23/24-JUL-2016


#Importing necessary libraries
library(stringr)
library(sqldf)
library(forecast)
library(timeDate)
library(data.table)
library(chron)

setwd("TheSmartRecruit/")
#Reading the train and test data set from csv file to data frame
if (!exists("train")) {
        train = as.data.table(read.csv("Train_pjb2QcD.csv"))
        }

if (!exists("test")) {
        test =  as.data.table(read.csv("Test_wyCirpO.csv"))
}

test$Business_Sourced = 2
all = rbind(train, test)
str(train)

all$Application_Receipt_Date = as.Date(all$Application_Receipt_Date,'%m/%d/%Y')
all$Applicant_BirthDate = as.Date(all$Applicant_BirthDate,'%m/%d/%Y')
all$Manager_DOJ = as.Date(all$Manager_DOJ,'%m/%d/%Y')
all$Manager_DoB = as.Date(all$Manager_DoB,'%m/%d/%Y')

sqldf("select distinct Application_Receipt_Date from all")
sqldf("select distinct Applicant_BirthDate from all")
sqldf("select distinct Manager_DOJ from all")
sqldf("select distinct Manager_DoB from all")

curr_year = 2008
head(all$Applicant_BirthDate)
head(format(all$Applicant_BirthDate,'%Y'))
all$Applicant_Age = 2008 - as.integer(format(all$Applicant_BirthDate,'%Y'))
all$Manager_Age = 2008 - as.integer(format(all$Manager_DoB,'%Y'))
all$Manager_Exp = 2008 - as.integer(format(all$Manager_DOJ,'%Y'))

sqldf("select distinct Applicant_Age from all")
sqldf("select distinct Manager_Age from all")
sqldf("select distinct Manager_Exp from all")

##################

#Data Exploration
#Looking for na values in the training and test data set

valid_train = apply(all, MARGIN = 2, FUN = is.na)
apply(valid_train, MARGIN = 2, FUN = sum) 

all_data = all



valid_train = apply(all_data, MARGIN = 2, FUN = is.na)
apply(valid_train, MARGIN = 2, FUN = sum) 

#NA Values 
#Manager_DOJ,Manager_DoB,Manager_Grade- 1507
#Applicant_BirthDate - 96
all_data[is.na(Manager_DOJ),]$Manager_DOJ = as.Date('1900-01-01','%Y-%m-%d')

nrow(all_data[is.na(Manager_DOJ),])
nrow(all_data[Manager_DOJ=="2005-11-10",])
nrow(all_data[is.na(Manager_DOJ),])
all_data$Manager_Joining_Designation = as.character(all_data$Manager_Joining_Designation)
all_data$Manager_Current_Designation = as.character(all_data$Manager_Current_Designation)
unique(all_data[!is.na(Manager_DOJ),.(Manager_DOJ,Manager_Joining_Designation,Manager_Current_Designation)])

all_data$tmp = paste(all_data$Manager_Joining_Designation,all_data$Manager_Current_Designation)

plot(all_data$tmp,all_data$Manager_DOJ)
#Manager_DOJ,Manager_DoB,Manager_Grade can be inferenced based on
#the values of Manager_Joining_Designation,Manager_Current_Designation
sqldf("select count(1) from all_data 
      where Manager_DOJ = to_date('1900/01/01','yyyy")

sqldf("select Manager_DOJ from all_data where Manager_DOJ like '%1900%'")

sqldf("select distinct Manager_DOJ,Manager_Joining_Designation,Manager_Current_Designation 
from all_data where Manager_DOJ = '1900-01-01'")



write.csv(all_data,"all_data.csv")




