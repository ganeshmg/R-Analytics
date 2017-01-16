#Analytics Vidhya - Seers Accuracy
#Author: Ganesh Babu Gajendiran
#Date: 29-APR-2016


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
        train = read.csv("Train_seers_accuracy.csv",header = TRUE)        
}


##################

#Data Exploration
#Looking for na values in the data set

na_values = apply(train, MARGIN = 2, FUN = is.na)
apply(na_values, MARGIN = 2, FUN = sum) 
#None are having NA values

#Looking for nan values in the data set

nan_values = apply(train, MARGIN = 2, FUN = is.nan)
apply(nan_values, MARGIN = 2, FUN = sum) #Data looks good with no nan values

#Looking for null values in the data set

apply(train, MARGIN = 2, FUN = is.null) #Data looks good with no null values

#Looking for empty string values (Spaces are being trimmed) in the training and test data set

empty_string_values = apply(train, MARGIN = 2, function(x) !(nchar(str_trim(x,side = "both")) > 0) )
apply(empty_string_values, MARGIN = 2, FUN = sum) 
#Referred_Friend is having 7 empty string values
#Payment_Mode is having 14513 empty string values
#Gender is having 2 empty string values

#looking for the count of unique values in the data set
apply(train, MARGIN = 2, FUN = function(x) length(unique(x)))


#Data Imputation

#Predicting the gender value for the 2 missing rows

gender_test = train[train$Gender != 'M' & train$Gender != 'F',]
#gender_train = train[train$Gender == 'M' | train$Gender == 'F',]

gender_train_m = train[train$Gender == 'M',]
gender_train_f = train[train$Gender == 'F',]

#Model generation facing memory issues
glm_model_m = glm(Gender~.,data = gender_train_m, family = 'binomial')
glm_model_f = glm(Gender~.,data = gender_train_f, family = 'binomial')

lm_model_m = lm(Gender~.,data = gender_train_m)
lm_model_f = lm(Gender~.,data = gender_train_f)

round(prop.table(table(train$Gender)),6)*100
#        C       F       M 
#0.0005  0.0002 26.6276 73.3718 

#since % for C and blank is very very minimal, updating it to M
train[train$Gender == 'C',]$Gender = 'M'
train[train$Gender == ' ',]$Gender = 'M'

#re factoring the value due to update in values
train$Gender = as.character(train$Gender)
train$Gender = as.factor(train$Gender)

round(prop.table(table(train$Gender)),6)*100
#F       M 
#26.6276 73.3724 


#Updating values for Referred_Friend
round(prop.table(table(train$Referred_Friend)),6)*100

#checking for relationship between Referred_Friend and Sales_Executive_ID

sqldf("select Sales_Executive_ID from train where Referred_Friend = ''")

table(train[train$Sales_Executive_ID == sales,]$Referred_Friend)

round(prop.table(table(train[train$Sales_Executive_ID == c('SD23000187'),]$Referred_Friend)),6)*100

sqldf("select Sales_Executive_ID, from train where Referred_Friend = ' '")

with(train,cor(as.numeric(Referred_Friend),as.numeric(Sales_Executive_ID)))
-0.02287907

with(train,cor(as.numeric(Referred_Friend),Client_ID))
-0.1228812

install.packages("polycor")
library(polycor)

hetcor(train) #consumes entire RAM, so check for individual columns




toInstallCandidates <- c("ggplot2", "BTYD", "reshape2", "plyr", "lubridate")
# check if pkgs are already present
toInstall <- toInstallCandidates[!toInstallCandidates%in%library()$results[,1]] 
if(length(toInstall)!=0)
{install.packages(toInstall, repos = "http://cran.r-project.org")}
# load pkgs
lapply(toInstallCandidates, library, character.only = TRUE)


data = train[,c(9,2,18)]
# format date
data$Transaction_Date <- as.character(data$Transaction_Date)
data$Transaction_Date <- as.Date(data$Transaction_Date, format="%d-%b-%y")

data = dc.MergeTransactionsOnSameDate(data)


# make log plot and plot sales

ggplot(data, aes(x=date,y=sales,group=cust))+
        geom_line(alpha=0.1)+
        scale_x_date()+
        scale_y_log10()+
        ggtitle("Sales for individual customers")+
        ylab("Sales ($, US)")+xlab("")+
        theme_minimal()

############################################
#
# DIVIDE DATA 
#
############################################


# into a calibration phase
# and a holdout phase

# determine middle point for splitting
(end.of.cal.period <-
         min(data$date)+as.numeric((max(data$date)-min(data$date))/2))


# split data into train(calibration) and test (holdout) and make matrices
train_data <- dc.ElogToCbsCbt(data, per="week", 
                        T.cal=end.of.cal.period,
                        merge.same.date=TRUE, # not needed, we already did it
                        statistic = "freq")   # which CBT to return

# take a look
str(data)


# cbs is short for "customer-by-sufficient-statistic” matrix
#               with the sufficient stats being: 
#                       frequency
#                       recency (time of last transaction) and
#                       total time observed

# extract calibration matrix
cal2.cbs <- as.matrix(data[[1]][[1]])
str(cal2.cbs)

############################################
#
# ESTIMATE PARAMETERS FOR MODEL
#
############################################

# initial estimate
params2 <- pnbd.EstimateParameters(cal2.cbs)

# look at log likelihood

(LL <- pnbd.cbs.LL(params2, cal2.cbs))


# make a series of estimates, see if they converge
p.matrix <- c(params2, LL)
for (i in 1:20) {
        params2 <- pnbd.EstimateParameters(cal2.cbs, params2)
        LL <- pnbd.cbs.LL(params2, cal2.cbs)
        p.matrix.row <- c(params2, LL)
        p.matrix <- rbind(p.matrix, p.matrix.row)
}

# examine
p.matrix

# use final set of values
(params2 <- p.matrix[dim(p.matrix)[1],1:4])

# Main model params are :

# r          gamma parameter for NBD transaction 
# alpha      gamma parameter for NBD transaction 
# s          gamma parameter for Pareto (exponential gamma) dropout process
# beta       gammma parameter for Pareto (exponential gamma) dropout process


############################################
#
# PLOT LOG-LIKELIHOOD ISO-CONTOURS FOR MAIN PARAMS
#
############################################

# set up parameter names for a more descriptive result
param.names <- c("r", "alpha", "s", "beta")

LL <- pnbd.cbs.LL(params2, cal2.cbs)

dc.PlotLogLikelihoodContours(pnbd.cbs.LL, params2, cal.cbs = cal2.cbs , n.divs = 5,
                             num.contour.lines = 7, zoom.percent = 0.3,
                             allow.neg.params = FALSE, param.names = param.names)


############################################
#
# PLOT GROUP DISTRIBUTION OF PROPENSITY TO PURCHASE, DROPOUT
#
############################################

# par to make two plots side by side
par(mfrow=c(1,2))

# Plot the estimated distribution of lambda 
# (customers' propensities to purchase)
pnbd.PlotTransactionRateHeterogeneity(params2, lim = NULL)
# lim is upper xlim
# Plot estimated distribution of gamma 
# (customers' propensities to drop out).

pnbd.PlotDropoutRateHeterogeneity(params2)

# set par to normal
par(mfrow=c(1,1))

############################################
#
# EXAMINE INDIVIDUAL PREDICTIONS
#
############################################

# estimate number transactions a new customer 
# will make in 52 weeks
pnbd.Expectation(params2, t = 52)

# expected characteristics for a specific individual, 
# conditional on their purchasing behavior during calibration

# calibration data for customer 1516
# frequency("x"), recency("t.x") and total time observed("T.cal")

cal2.cbs["1516",]
x <- cal2.cbs["1516", "x"]         # x is frequency
t.x <- cal2.cbs["1516", "t.x"]     # t.x is recency, ie time of last transactions
T.cal <- cal2.cbs["1516", "T.cal"] # T.cal is total time observed

# estimate transactions in a T.star-long duration for that cust
pnbd.ConditionalExpectedTransactions(params2, T.star = 52, # weeks
                                     x, t.x, T.cal)


############################################
#
# PROBABILITY A CUSTOMER IS ALIVE AT END OF CALIBRATION / TRAINING
#
############################################

x           # freq of purchase
t.x         # week of last purchase
T.cal <- 39 # week of end of cal, i.e. present
pnbd.PAlive(params2, x, t.x, T.cal)

# To visualize the distribution of P(Alive) across customers:
params3 <- pnbd.EstimateParameters(cal2.cbs)
p.alives <- pnbd.PAlive(params3, cal2.cbs[,"x"], cal2.cbs[,"t.x"], cal2.cbs[,"T.cal"])

ggplot(as.data.frame(p.alives),aes(x=p.alives))+
        geom_histogram(colour="grey",fill="orange")+
        ylab("Number of Customers")+
        xlab("Probability Customer is 'Live'")+
        theme_minimal()

# plot actual & expected customers binned by num of repeat transactions
pnbd.PlotFrequencyInCalibration(params2, cal2.cbs, 
                                censor=10, title="Model vs. Reality during Calibration")


############################################
#
# HOW WELL DOES MODEL DO IN HOLDOUT PERIOD?
#
############################################

# get holdout transactions from dataframe data, add in as x.star

x.star   <- data[[2]][[2]][,1]
cal2.cbs <- cbind(cal2.cbs, x.star)
str(cal2.cbs)

holdoutdates <- attributes(data[[2]][[1]])[[2]][[2]]
holdoutlength <- round(as.numeric(max(as.Date(holdoutdates))-
                                          min(as.Date(holdoutdates)))/7)

# plot predicted vs seen conditional freqs and get matrix 'comp' w values

T.star <- holdoutlength
censor <- 10 # Bin all order numbers here and above
comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params2, T.star,
                                                    cal2.cbs, x.star, censor)
rownames(comp) <- c("act", "exp", "bin")
comp

# plot predicted vs actual by week

# get data without first transaction, this removes those who buy 1x
removedFirst.elog <- dc.SplitUpElogForRepeatTrans(elog)$repeat.trans.elog
removedFirst.cbt <- dc.CreateFreqCBT(removedFirst.elog)

# get all data, so we have customers who buy 1x
allCust.cbt <- dc.CreateFreqCBT(elog)

# add 1x customers into matrix
tot.cbt <- dc.MergeCustomers(data.correct=allCust.cbt, 
                             data.to.correct=removedFirst.cbt)


lengthInDays <- as.numeric(max(as.Date(colnames(tot.cbt)))-
                                   min(as.Date(colnames(tot.cbt))))
origin <- min(as.Date(colnames(tot.cbt)))

tot.cbt.df <- melt(tot.cbt,varnames=c("cust","date"),value.name="Freq")
tot.cbt.df$date<-as.Date(tot.cbt.df$date)
tot.cbt.df$week<-as.numeric(1+floor((tot.cbt.df$date-origin+1)/7))

transactByDay  <- ddply(tot.cbt.df,.(date),summarize,sum(Freq))
transactByWeek <- ddply(tot.cbt.df,.(week),summarize,sum(Freq))
names(transactByWeek) <- c("week","Transactions")
names(transactByDay)  <- c("date","Transactions")


T.cal <- cal2.cbs[,"T.cal"]
T.tot <- 78 # end of holdout
comparisonByWeek <- pnbd.PlotTrackingInc(params2, T.cal,
                                         T.tot, actual.inc.tracking.data=transactByWeek$Transactions)

############################################
#
# FORMAL MEASURE OF ACCURACY
#
############################################

# root mean squared error
rmse <- function(est, act) { return(sqrt(mean((est-act)^2))) }

# mean squared logarithmic error
msle <- function(est, act) { return(mean((log1p(est)-log1p(act))^2)) }

str(cal2.cbs)

cal2.cbs[,"x"]

predict<-pnbd.ConditionalExpectedTransactions(params2, T.star = 38, # weeks
                                              x     = cal2.cbs[,"x"], 
                                              t.x   = cal2.cbs[,"t.x"], 
                                              T.cal = cal2.cbs[,"T.cal"])

cal2.cbs[,"x.star"]  # actual transactions for each person


rmse(act=cal2.cbs[,"x.star"],est=predict)
msle(act=cal2.cbs[,"x.star"],est=predict)

# not useful w/o comparison: is this better than guessing?