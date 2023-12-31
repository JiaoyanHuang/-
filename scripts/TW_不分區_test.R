library(readxl)
library(sf)
library(ggplot2)
library(tidyverse)
library(stringr)
library(ggpubr)
library(stringr)

#台灣shp file
path = "C:/Users/huang/Downloads/2.鄉鎮界WGS84"
TW_shp_file = sprintf("%s/鄉鎮界_WGS84.shp",path)
yr = c("2008","2012","2016","2020")
taiwan_shp <- sf::st_read(TW_shp_file)
taiwan_shp$countyname[which(taiwan_shp$countyname == "臺南市(海)")] = "臺南市"
taiwan_shp$countyname[which(taiwan_shp$countyname == "臺中市(海)")] = "臺中市"
taiwan_shp$countyname[which(taiwan_shp$countyname == "基隆市(海)")] = "基隆市"
taiwan_shp$countyname[which(taiwan_shp$countyname == "高雄市(海)")] = "高雄市"


#總統大選數據
path = "C:/Users/huang/Desktop/台灣總統大選數據/"
for(y in 1:length(yr)){
  year = yr[y]
  if(as.numeric(year) >= 2016){
    taiwan_shp$countyname[which(taiwan_shp$countyname == "桃園縣")] = "桃園市"
    for(r in 1:nrow(taiwan_shp)){
      if(taiwan_shp$countyname[r] == "桃園市"){
        taiwan_shp$townname[r] = paste(substr(taiwan_shp$townname[r],1,2),"區",sep="")
      }
    }
  }
  
  for(i in 1:nrow(taiwan_shp)){
    tmp_town = taiwan_shp$townname[i]
    if(grepl("(海)",tmp_town)){
      if(taiwan_shp$townname[i] != "海端鄉"){
        taiwan_shp$townname[i] = substr(tmp_town,1,nchar(tmp_town)-3)
      }
    }
  }
  
  taiwan_shp$county_town = paste(taiwan_shp$countyname,taiwan_shp$townname,sep = "_")
  different_area = taiwan_shp$county_town[which(grepl("區",taiwan_shp$townname))]
  different_area1 = taiwan_shp$townname[which(grepl("區",taiwan_shp$townname))]
  
  tmp_file = sprintf("%s_TW_政黨票_election.csv",year)
  file = sprintf("%s/%s",path,tmp_file)
  data = read.csv(file,stringsAsFactors = F)
  data$County[which(data$County == "臺北縣")] = "新北市" 
  data$County[which(data$County == "臺中縣")] = "臺中市"
  data$County[which(data$County == "臺南縣")] = "臺南市"
  data$County[which(data$County == "高雄縣")] = "高雄市" 
  data$city  = str_trim(data$city)
  
  data$county_town = paste(data$County,data$city,sep = "_")
  for (i in 1:length(different_area)){
    if(different_area1[i] != "南區" & different_area1[i] != "東區" &
       different_area1[i] != "西區" & different_area1[i] != "北區" &
       different_area1[i] != "中區"){
      tmp = substr(different_area[i],1,nchar(different_area[i])-1)
      data$city[which(grepl(tmp,data$county_town))] = different_area1[i]
    }
  }
  data$city[which(data$city == "三民區")] = "那瑪夏區"
  data$DDP.vote = as.numeric(gsub(",","",data$DDP.vote))
  data$KMT.vote = as.numeric(gsub(",","",data$KMT.vote))
  data$valid.vote = as.numeric(gsub(",","",data$valid.vote))
  #KMT
  tmp_n = which(grepl("KMT",colnames(data)))
  tmp_n2 = which(grepl("valid",colnames(data)))
  tmp_data = data[,c(1,2,tmp_n,tmp_n2)]
  colnames(tmp_data) = c("countyname","townname","KMT_vote","valid_vote")

  tmp_data_bytown = tmp_data %>%
    group_by(countyname,townname) %>%
    summarise(KMT_vote = sum(KMT_vote),
              valid_vote = sum(valid_vote))
  
  tmp_data_bytown$KMT_percent = tmp_data_bytown$KMT_vote/tmp_data_bytown$valid_vote
  tmp_KMT = as.data.frame(tmp_data_bytown[,c("countyname","townname","KMT_percent")])
  tmp_KMT = merge(taiwan_shp,tmp_KMT,by = c("countyname","townname"))
  plot_title = sprintf("%s 不分區國民黨得票率",year)
  p1 = ggplot(tmp_KMT,
              aes(fill = KMT_percent)) +
    geom_sf() + 
    scale_fill_gradient(low = "white",high = "blue",limits=c(0,1)) +
    coord_sf(xlim= c(119,123)) + 
    ggtitle(plot_title)
  outfile = sprintf("%s/plots/%s_KMT_不分區.png",path,year)
  ggsave(outfile,p1,width = 6,height = 4,units = "in",dpi = 600)
  
  #綠營
  tmp_n = which(grepl("DDP",colnames(data)))
  tmp_data = data[,c(1,2,tmp_n,tmp_n2)]
  colnames(tmp_data) = c("countyname","townname","DPP_vote","valid_vote")
  
  tmp_data$county_town = paste(tmp_data$countyname,tmp_data$townname,sep = "_")
  tmp_data = tmp_data[,c("countyname","townname","DPP_vote","valid_vote")]
  tmp_data_bytown = tmp_data %>%
    group_by(countyname,townname) %>%
    summarise(DPP_vote = sum(DPP_vote),
              valid_vote = sum(valid_vote))
  
  tmp_data_bytown$DPP_percent = tmp_data_bytown$DPP_vote/tmp_data_bytown$valid_vote
  tmp_DPP = tmp_data_bytown[,c("countyname","townname","DPP_percent")]
  tmp_DPP = merge(taiwan_shp,tmp_DPP,by = c("countyname","townname"))
  plot_title = sprintf("%s 不分區民進黨得票率",year)
  p2 = ggplot(tmp_DPP,
              aes(fill = DPP_percent)) +
    geom_sf() + 
    scale_fill_gradient(low = "white",high = "green",limits=c(0,1)) +
    coord_sf(xlim= c(119,123)) + 
    ggtitle(plot_title)
  outfile = sprintf("%s/plots/%s_DDP_不分區.png",path,year)
  ggsave(outfile,p2,width = 6,height = 4,units = "in",dpi = 600)
  
  
  test = tmp_DPP
  test$diff = tmp_DPP$DPP_percent - tmp_KMT$KMT_percent
  plot_title = sprintf("%s 不分區藍綠版圖",year)
  p3 = ggplot(test,
              aes(fill = diff)) +
    geom_sf() + 
    scale_fill_gradient2(low = "blue",high = "green",mid = "white",midpoint = 0,limits=c(-1,1)) +
    coord_sf(xlim= c(119,123)) + 
    ggtitle(plot_title) +
    guides(fill=guide_legend(title="藍綠版圖"))
  outfile = sprintf("%s/plots/%s_DDP_KMT_不分區.png",path,year)
  ggsave(outfile,p3,width = 6,height = 4,units = "in",dpi = 600)
  
  p4 = ggarrange(p1,p2,p3,ncol = 3, nrow = 1)
  outfile = sprintf("%s/plots/%s_不分區.png",path,year)
  ggsave(outfile,p4,width = 18,height = 4,units = "in",dpi = 600)
}
