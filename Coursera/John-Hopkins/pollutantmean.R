#pollutantmean.R
pollutantmean <- function(directory,pollutant,id=1:332) {
  all_data = NULL
      for (i in id) {
        #monitor = i
        #if (nchar(i) == 1) monitor = paste('00',i,sep = '')
        #if (nchar(i) == 2) monitor = paste('0',i,sep = '')
        monitor = sprintf("%03d",i)
        fname = paste(directory,"/",monitor,".csv",sep = '')
        tmp_data = read.csv(fname,header = TRUE,sep = ',')
        all_data = rbind(all_data,tmp_data)
      }
  if (pollutant == 'sulfate') fnl_data = all_data[,2]
  if (pollutant == 'nitrate') fnl_data = all_data[,3]
  
  fnl_mean = round(mean(fnl_data,na.rm = TRUE),3)
  fnl_mean
}