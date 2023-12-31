library(readxl)
library(tidyverse)
path = "C:/Users/huang/Downloads/2020立委/立委-各投票所得票明細及概況(Excel檔)"

file_list = list.files(path)
files = file_list[which(grepl("A05-6",file_list))]
for (f in 1:length(files)){
  tmp_file = sprintf("%s/%s",path,files[f])
  tmp_line = as.data.frame(read_excel(tmp_file,sheet = 1))
  tmp_line = colnames(tmp_line)[1]
  county = substr(tmp_line,23,25)
  print(county)
  print(f)
  tmp_data = read_excel(tmp_file,sheet = 1,skip =2 )
  tmp_data = tmp_data[c(-1,-2),]
  KMT_tmp_n = which(grepl("國民黨",colnames(tmp_data)))
  DDP_tmp_n = which(grepl("民主進步",colnames(tmp_data)))
  tmp_data = tmp_data[,c(1,2,KMT_tmp_n,DDP_tmp_n,23)]
  colnames(tmp_data) = c("city","village","KMT vote","DDP vote","valid vote")
  tmp_data1 = tmp_data[which(is.na(tmp_data$village) & tmp_data$city != "總　計"),]
  tmp_data1$County = county
  tmp_data1 = tmp_data1[,c("County","city","DDP vote","KMT vote","valid vote")]
  if(f == 1){
    data = tmp_data1
  }else{
    data = rbind(data,tmp_data1)
  }
}
outfile = "C:/Users/huang/Desktop/台灣總統大選數據/2020_TW_政黨票_election.csv"
write_excel_csv(data,outfile) #this is key