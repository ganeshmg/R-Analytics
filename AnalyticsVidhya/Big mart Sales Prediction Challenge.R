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

#Reading the train and test data set from csv file to data frame
if (!exists("train")) {
        train = read.csv("train.csv",header = TRUE)        
}

if (!exists("test")) {
        test = read.csv("test.csv",header = TRUE)
}

str(train)

#Creating new data set for continuous and categorical variables separately.
train_cont = train[,c("ID","Age","Hours.Per.Week")]
train_cat = train[,-c(1,2,10)]

summary(train_cont)

install.packages("pastecs")
library(pastecs)

#set sigificant digits and get detailed summary
options(scipen = 100)
options(digits = 2)
stat.desc(train_cont)

#nbr.val - shows number of values in a variable
#nbr.null - shows number of null(missing) values
#nbr.na - shows number of NA(missing) values
#CI.mean.0.95 - considers 95% confidence interval


apply(train_cat,MARGIN = 2, function(x){length(unique(x))})

#print the counts of each category
table(train_cat$Race)
#print the percentage of observation in each category
as.matrix(prop.table(table(train_cat$Race)))

#print the counts of top 20 countries
head(sort(table(train_cat$Native.Country), decreasing = TRUE),20)

#print the percentage of observations of top 20 countries
head(round(sort(prop.table(table(train_cat$Native.Country)), decreasing = TRUE),6),20)


#cross-tabulation or confusion matrix of the two variables.
install.packages("gmodels")
library(gmodels)

#cross table for Sex and Income group
CrossTable(train$Sex,train$Income.Group)

#Using stacked bar chart for this analysis
library(ggplot2)

#plotting the stacked bar chart
ggplot(train,aes(Sex, fill = Income.Group)) + geom_bar()+labs(title = "Stacked Bar Chart",x = "Sex", y = "Count")+theme_bw()


#box plot
ggplot(train,aes(Sex, Hours.Per.Week)) + geom_boxplot()+labs(title = "Box Plot")+theme_bw()

#Checking missing values
table(is.na(train))

colSums(is.na(train))

colSums(is.na(test))

#mlr packages is one of multi-tasking and powerful package for imputing missing values.

install.packages("mlr")
library(mlr)

#impute missing values with mode
imputed_data = impute(train,classes = list(factor = imputeMode()))

t_train = imputed_data$data

identical(train,t_train)

#Outlier check

ggplot(train,aes(ID,Age))+geom_jitter()

#Variable transformation
sapply(train,class)

#Predictive modelling

#Data preprocessing

table(train$Income.Group)

train$Income.Group = ifelse(train$Income.Group == "<=50K",0,1)

#removing identifier variable from train data
t_train = train[,-c(1)]

#Model building
#install.packages(rpart)
library(rpart)

set.seed(333)
train.tree = rpart(Income.Group ~., data = t_train, method = "anova",
                   control = rpart.control(minsplit = 20,
                                           minbucket = 100,
                                           maxdepth = 10,
                                           xval = 5))

#minsplit - refers to the minimum number of observations which must exist in a node to split
#minbucket - refers to the minimum number of observations which must exist in terminal nodes (leaf)
#maxdepth - refers to the depth of the tree
#xval - refers to cross validation

summary(train.tree)

#for better understanding let us plot this tree and visualize
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(train.tree)

#Interpretations from the above rpart.plot
#Relationship is the most important variable.
#The first node: If the relationship status is 'Not in family','Own child', 'Unmarried', 'Other relatives', the tree predicts their salary <= 50K, else if the relationship status is different, the tree moves to node 2.
#We get 6 terminal nodes (leaf).
#Similary, you can understand the splitting at other nodes.


#Linear models
t_train$Income.Group = as.factor(t_train$Income.Group)
l_model = glm(Income.Group~., data = t_train, family = "quasibinomial")
summary(l_model)

l_model1 = glm(Income.Group~., data = t_train, family = "binomial")
summary(l_model)

t_train_1 = t_train
t_train_1$Education = as.character(t_train_1$Education)
t_train_1$Education = ifelse(t_train_1$Education == "1st-4th","<=12th",t_train_1$Education)
t_train_1$Education = ifelse(t_train_1$Education == "5th-6th","<=12th",t_train_1$Education)
t_train_1$Education = ifelse(t_train_1$Education == "7th-8th","<=12th",t_train_1$Education)
t_train_1$Education = ifelse(t_train_1$Education == "9th","<=12th",t_train_1$Education)
t_train_1$Education = ifelse(t_train_1$Education == "10th","<=12th",t_train_1$Education)
t_train_1$Education = ifelse(t_train_1$Education == "11th","<=12th",t_train_1$Education)
t_train_1$Education = ifelse(t_train_1$Education == "12th","<=12th",t_train_1$Education)
t_train_1$Education = ifelse(t_train_1$Education == "Preschool","<=12th",t_train_1$Education)


test$Education = as.character(test$Education)
test$Education = ifelse(test$Education == "1st-4th","<=12th",test$Education)
test$Education = ifelse(test$Education == "5th-6th","<=12th",test$Education)
test$Education = ifelse(test$Education == "7th-8th","<=12th",test$Education)
test$Education = ifelse(test$Education == "9th","<=12th",test$Education)
test$Education = ifelse(test$Education == "10th","<=12th",test$Education)
test$Education = ifelse(test$Education == "11th","<=12th",test$Education)
test$Education = ifelse(test$Education == "12th","<=12th",test$Education)
test$Education = ifelse(test$Education == "Preschool","<=12th",test$Education)
test$Education = as.factor(test$Education)


t_train_1$Marital.Status = as.character(t_train_1$Marital.Status)
t_train_1$Marital.Status = ifelse(t_train_1$Marital.Status == "Married-spouse-absent","Separated",t_train_1$Marital.Status)
t_train_1$Marital.Status = ifelse(t_train_1$Marital.Status == "Widowed","Separated",t_train_1$Marital.Status)
t_train_1$Marital.Status = as.factor(t_train_1$Marital.Status)

test$Marital.Status = as.character(test$Marital.Status)
test$Marital.Status = ifelse(test$Marital.Status == "Married-spouse-absent","Separated",test$Marital.Status)
test$Marital.Status = ifelse(test$Marital.Status == "Widowed","Separated",test$Marital.Status)
test$Marital.Status = as.factor(test$Marital.Status)

l_model_2 = glm(Income.Group~., data = t_train_1, family = "quasibinomial")
summary(l_model_2)

t_train_1$Relationship = ifelse(t_train_1$Relationship == "Other-relative","Unmarried",t_train_1$Relationship)
test$Relationship = ifelse(test$Relationship == "Other-relative","Unmarried",test$Relationship)

t_train_1$Race = ifelse(t_train_1$Race == "Black","Other",t_train_1$Race)
test$Race = ifelse(test$Race == "Black","Other",test$Race)

OccupationArmed-Forces
OccupationCraft-repair
OccupationTransport-moving

t_train_1$Occupation = ifelse(t_train_1$Occupation == "Craft-repair","Armed-Forces",t_train_1$Occupation)
t_train_1$Occupation = ifelse(t_train_1$Occupation == "Transport-moving","Armed-Forces",t_train_1$Occupation)

test$Occupation = ifelse(test$Occupation == "Craft-repair","Armed-Forces",test$Occupation)
test$Occupation = ifelse(test$Occupation == "Transport-moving","Armed-Forces",test$Occupation)

l_model_3 = glm(Income.Group~., data = t_train_1, family = "quasibinomial")
summary(l_model_3)

l_model_4 = glm(Income.Group~., data = t_train_1, family = "binomial")
summary(l_model_4)

t_train_us = t_train_1[t_train_1$Native.Country == 'United-States',]
t_train_nonus = t_train_1[t_train_1$Native.Country != 'United-States',]

t_train_us = t_train_us[,-c(10)]
l_model_5_1 = glm(Income.Group~., data = t_train_us, family = "binomial")
l_model_5_2 = glm(Income.Group~., data = t_train_nonus, family = "binomial")

#Making predictions
predict_train = predict(train.tree,train, type = 'class')
predict_train = predict(train.tree,test, type = 'class')
predict_test = predict(train.tree,test, type = 'vector')
predict_test = predict(l_model,test, type = 'response')

predict_train = predict(l_model_3,t_train_1, type = 'link')
predict_test = predict(l_model_3,test, type = 'response')

predict_train = predict(l_model_4,t_train_1, type = 'response')
predict_test = predict(l_model_4,test, type = 'response')


test_us = test[test$Native.Country == 'United-States',]
test_nonus = test[test$Native.Country != 'United-States',]

predict_train_us = predict(l_model_5_1,t_train_us, type = 'response')
predict_train_nonus = predict(l_model_5_2,t_train_nonus, type = 'response')
predict_test_us = predict(l_model_5_1,test_us, type = 'response')
predict_test_nonus = predict(l_model_5_2,test_nonus, type = 'response')

class(train.tree)
#Analyze results

#Various metrics can be used to evaluate a model depending on the problem at hand. 
#Let's use prediction accuracy here. Since this is a classification problem, we'll use 
#confusion matrix. It can be found in caret package. Confusion matrix is a NXN matrix 
#where N is the number of class predicted. It maps the number of labels which get 
#classified correctly and incorrectly. This matrix is easy to interpret and is being 
#used popularly.

#loading the required package for the confusion matrix
library(caret)
predict_train = ifelse(predict_train <= 0.50,0,1)
confusionMatrix(predict_train,t_train_1$Income.Group)


#train accuracy of 82%

#submiting the prediction on the test data

submit = data.frame(ID = test$ID, Income.Group = predict_test)

submit$Income.Group = ifelse(submit$Income.Group <=0.50,"<=50K",">50K")

write.csv(submit,"Solution_l_model_4.csv",row.names = FALSE)
