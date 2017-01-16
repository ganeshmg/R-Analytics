#rankhospital.R
#Week4 assignment
#Coursera - John Hopkins R Programming
#Ganesh
#March 2016

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_f <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states = unique(outcome_f$State)
  if(sum(states == state) == 0) stop('invalid state')
  
  if (outcome == 'heart attack') {
    colnum = 11
    outcome_f[, 11] = gsub("Not Available","",outcome_f[, 11])
    outcome_f = outcome_f[complete.cases(outcome_f[,c(11)]),]
    outcome_f[, 11] <- as.numeric(outcome_f[, 11])
  } else if (outcome == 'heart failure') {
    colnum = 17
    outcome_f[, 17] = gsub("Not Available","",outcome_f[, 17])
    outcome_f = outcome_f[complete.cases(outcome_f[,c(17)]),]
    outcome_f[, 17] <- as.numeric(outcome_f[, 17])
  } else if (outcome == 'pneumonia') {
    colnum = 23
    outcome_f[, 23] = gsub("Not Available","",outcome_f[, 23])
    outcome_f = outcome_f[complete.cases(outcome_f[,c(23)]),]
    outcome_f[, 23] <- as.numeric(outcome_f[, 23])
  } else stop('invalid outcome')
  
  filt_outcome = NULL
  filt_outcome = outcome_f[outcome_f$State == state,]
  filt_outcome = filt_outcome[order(filt_outcome[,colnum],filt_outcome[,2]),c(2,colnum)]
  filt_outcome = filt_outcome[complete.cases(filt_outcome[,c(2)]),]
  filt_outcome = filt_outcome[order(filt_outcome[,2],filt_outcome[,1]),]
  rownames(filt_outcome) = NULL
  filt_outcome$Rank = rownames(filt_outcome)
  colnames(filt_outcome)[2] = 'Rate'
  
  ## Return hospital name in that state with the given rank
  hospital_name = NULL
  
  if(num == 'best') {hospital_name = filt_outcome[1,1] }
  else if(num == 'worst') {hospital_name = filt_outcome[nrow(filt_outcome),1]}
  else if(is.numeric(num) == TRUE) {
          if(as.numeric(num)>nrow(filt_outcome)) return('NA')
            else hospital_name = filt_outcome[as.numeric(num),1]
  } else {stop('Invalid ranking value')}
  ## 30-day death rate
  #return(filt_outcome)
  hospital_name
}