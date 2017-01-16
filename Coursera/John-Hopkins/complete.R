#complete.R
complete <- function(directory,id = 1:332){
  all_data =  data.frame(id = numeric(),nobs = numeric())
  for (i in id) {
    monitor = i
    if (nchar(i) == 1) monitor = paste('00',i,sep = '')
    if (nchar(i) == 2) monitor = paste('0',i,sep = '')
    fname = paste(directory,"/",monitor,".csv",sep = '')
    tmp_data = read.csv(fname,header = TRUE,sep = ',')
    tmp_data = tmp_data[complete.cases(tmp_data[,c(2,3)]),]
    all_data = rbind(all_data,c(i,nrow(tmp_data)))
  }
  colnames(all_data) = c("id","nobs")
  all_data
}