#best.R
#Week4 assignment
#Coursera - John Hopkins R Programming
#Ganesh
#March 2016

best <- function(state, outcome) {
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
        }
  else if (outcome == 'heart failure') {
    colnum = 17
    outcome_f[, 17] = gsub("Not Available","",outcome_f[, 17])
    outcome_f = outcome_f[complete.cases(outcome_f[,c(17)]),]
    outcome_f[, 17] <- as.numeric(outcome_f[, 17])
    }
  else if (outcome == 'pneumonia') {
    colnum = 23
    outcome_f[, 23] = gsub("Not Available","",outcome_f[, 23])
    outcome_f = outcome_f[complete.cases(outcome_f[,c(23)]),]
    outcome_f[, 23] <- as.numeric(outcome_f[, 23])
    }
  else stop('invalid outcome')
  
  filt_outcome = outcome_f[outcome_f$State == state,]
  rownames(filt_outcome) = NULL
  least30daymortality = min(filt_outcome[,colnum])
  filt_outcome = filt_outcome[order(filt_outcome[,colnum],filt_outcome[,2]),]
  hospital_name = filt_outcome[1,2]
  #$Hospital.Name
  
  #paste('state=',state,'outcome=',outcome, 'colnum=',colnum)
  ## Return hospital name in that state with lowest 30-day death
  hospital_name
  ## rate
}