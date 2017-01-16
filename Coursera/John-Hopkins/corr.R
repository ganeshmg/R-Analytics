#Corr.R

corr <- function(directory, threshold = 0) {
  all_data =  NULL
  corr_vector = NULL
  id = 1:332
  j=1
  for (i in id) {
    monitor = i
    if (nchar(i) == 1) monitor = paste('00',i,sep = '')
    if (nchar(i) == 2) monitor = paste('0',i,sep = '')
    fname = paste(directory,"/",monitor,".csv",sep = '')
    tmp_data = read.csv(fname,header = TRUE,sep = ',')
    tmp_data = tmp_data[complete.cases(tmp_data[,c(2,3)]),]
    colnames(tmp_data) = c("Date","Sulphate","Nitrate","ID")
    n = nrow(tmp_data)
    if (n>=threshold) {
      corr_vector[j] = cor(tmp_data$Sulphate,tmp_data$Nitrate) 
      j=j+1
    }
    #else corr_vector[i] = 0
    #all_data = rbind(all_data,tmp_data)
  }
  corr_vector
}