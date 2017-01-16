#Analytics Vidhya - Seers Accuracy
#Author: Ganesh Babu Gajendiran
#Date: 29-APR-2016

#Importing necessary libraries
library(sqldf)

#Reading the train and test data set from csv file to data frame
if (!exists("train")) {
        train = read.csv("Train_seers_accuracy.csv",header = TRUE)        
}


df = train[,c(9,2,18)]
names(df) = c("ID","Date","Amount")
# format date
df$Date <- as.character(df$Date)
df$Date <- as.Date(df$Date, format="%d-%b-%y")

#checking whether a client has more than one transaction

sqldf("select Client_ID, count(1) from train group by Client_ID having count(1) > 1")

#get the number of rows in the dataset
nrow<-nrow(df)

#order the dataframe by customer ID and transcation date
df <- df[order(df$ID,df$Date),]

#Combine transactions happening on same day to a single transaction
df1 = train[,c(9,2,18)]
names(df1) = c("ID","Date","Amount")
# format date
df1$Date <- as.character(df1$Date)
df1$Date <- as.Date(df1$Date, format="%d-%b-%y")
#record the number of days between a customer's each transcation, say 10 days between 1st purchase and the second, 15 days between the second and the third Interval <-rep(0,times=nrow)
sqldf("select ID,Date,count(1) from df1 group by ID,Date having count(1) > 1")
df2 = sqldf("select ID,Date,sum(Amount) from df1 group by ID,Date")
sqldf("select ID,Date,count(1) from df2 group by ID,Date having count(1) > 1")

df2 <- df2[order(df2$ID,df2$Date),]
nrow<-nrow(df2)
# record the # of a customer's each transaction say 1st transcation, 2nd transcation
Times <- rep(1,times=nrow)
Interval <- rep(1,times=nrow)
# record the total number of a customer's transcations
TotalTimes <- rep(1,times=nrow)

#caculate the data for the above three vectors.
n<-2

for (i in 2 : nrow){
        
        if (df2[i,"ID"] == df2[i-1,"ID"]){
                Interval[i] <- as.numeric(difftime(df2[i,"Date"],df2[i-1,"Date"],units="days"))	
                Times[i] <- n
                n <- n+1
        }else{
                TotalTimes[(i-n+1) : (i-1)] <- n-1
                n<-2
                
        }
        
}

#add the three vectors to the data frame 
df <- cbind(df, Interval,Times,TotalTimes)
df2 <- cbind(df2, Interval,Times,TotalTimes)

head(df)




# get the matrix of customer ID ~ the customer¡¯s total number of transactions
TimesByID <-as.data.frame(table(df$ID))
TimesByID_2 <-as.data.frame(table(df2$ID))

#get the matrix of total number of transactions ~ number of customers who have the total number
GroupByTimes <- as.data.frame(table(TimesByID$Freq))
GroupByTimes_2 <- as.data.frame(table(TimesByID_2$Freq))

names(GroupByTimes) <- c("Times","Customers")
names(GroupByTimes_2) <- c("Times","Customers")


# caculate the repeat purchase percentages

percentages<-round(GroupByTimes$Customers / 417107 ,3)
percentages2<-round(1-(GroupByTimes_2$Customers/417107),3)

GroupByTimes_2 = cbind(GroupByTimes_2,percentages2)

df2$Cross_Sell = 0

df2[df2$TotalTimes == 1,]$Cross_Sell = 0.962
df2[df2$TotalTimes == 2,]$Cross_Sell = 0.031
df2[df2$TotalTimes == 3,]$Cross_Sell = 0.005
df2[df2$TotalTimes == 4,]$Cross_Sell = 0.001

submit = df2[,c(1,7)]
submit = sqldf("select distinct Client_ID,Cross_Sell from submit")
names(submit) = c("Client_ID","Cross_Sell")

write.csv(submit,"Submission.csv",row.names = FALSE)

