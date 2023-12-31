library(readxl)
library(tidyverse)
path = "C:/Users/huang/Downloads/第13任總統(副總統)得票概況/2012總統大選"

file_list = list.files(path)
files = file_list[which(grepl("A05-3",file_list))]
for (f in 1:length(files)){
  tmp_file = sprintf("%s/%s",path,files[f])
  tmp_line = as.data.frame(read_excel(tmp_file,sheet = 1))
  tmp_line = colnames(tmp_line)[1]
  county = substr(tmp_line,17,19)
  print(county)
  print(f)
  tmp_data = read_excel(tmp_file,sheet = 1,skip =5 )
  colnames(tmp_data)[c(1:6)] = c("city","village","DDP vote","KMT vote","PFP vote","valid vote")
  tmp_data1 = tmp_data[which(is.na(tmp_data$village)),]
  tmp_data1$County = county
  tmp_data1 = tmp_data1[,c("County","city","DDP vote","KMT vote","PFP vote","valid vote")]
  if(f == 1){
    data = tmp_data1
  }else{
    data = rbind(data,tmp_data1)
  }
}
outfile = "C:/Users/huang/Desktop/台灣總統大選數據/2012_TW_election.csv"
write_excel_csv(data,outfile) #this is key