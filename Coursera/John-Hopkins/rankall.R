#rankall.R
#Week4 assignment
#Coursera - John Hopkins R Programming
#Ganesh
#March 2016

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_f <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states = unique(outcome_f$State)
  #if(sum(states == state) == 0) stop('invalid state')
  
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
  #filt_outcome = outcome_f[outcome_f$State == state,]
  filt_outcome = outcome_f[,c(2,colnum,7)]
  #filt_outcome = filt_outcome[complete.cases(filt_outcome[,c(2)]),]
  rownames(filt_outcome) = NULL
  filt_outcome = filt_outcome[complete.cases(filt_outcome[,c(2)]),]
  filt_outcome = filt_outcome[order(filt_outcome[,3],filt_outcome[,2],filt_outcome[,1]),]
  filt_outcome$Rank = ave(filt_outcome[,2],filt_outcome[,3],FUN = function(x) rank(x,ties.method="first"))
  colnames(filt_outcome) = c('hospital','rate','state','rank')
  
  ## For each state, find the hospital of the given rank
  all_hospitals = NULL

  if(num == 'best') {all_hospitals = filt_outcome[filt_outcome$rank == 1,c(1,3)] }
  else if(num == 'worst') {
    worst = as.data.frame(with(filt_outcome,tapply(rank,state,max)))
    worst$state = rownames(worst)
    colnames(worst) = c('worst_rank','state')
    worst$worst_rank = as.numeric(worst$worst_rank)
    all_hospitals = merge(filt_outcome,worst,by.x = c('state','rank'),by.y = c('state','worst_rank'))
    all_hospitals = all_hospitals[,c(3,1)]
    }
  else if(is.numeric(num) == TRUE) {
    all_hospitals = filt_outcome[filt_outcome$rank == num,c(1,3)]
      } else {stop('Invalid ranking value')}
  ## Return a data frame with the hospital names and the
  states = as.data.frame(states)
  colnames(states) = 'state'
  all_hospitals = merge(all_hospitals,states,by.x = c("state"), by.y = c("state"), all.y = TRUE)
  all_hospitals = all_hospitals[,c(2,1)]
  rownames(all_hospitals) = all_hospitals$state
  return(all_hospitals)
  #return(all_hospitals)
  ## (abbreviated) state name
}
