library(stringr)
july9data<-read.csv("7 days data.csv",header=T,stringsAsFactors = F)
data2<- aggregate(Package_AWB~Delhivery_Bag_Id, data=july9data, FUN=length)
bagd <- july9data %>% select(Package_AWB,Delhivery_Bag_Id) %>% group_by(Delhivery_Bag_Id) %>% summarise(num_packages = length(Package_AWB)) %>% group_by(num_packages) %>% summarise(num_bags = length(Delhivery_Bag_Id))  %>% as.data.frame()

library(dplyr)
library(plotly)
plot_ly(data = bagd,x = num_packages,y = num_bags,marker=list(color = toRGB("red")), type = "bar",width = 1000)

july9data <- july9data %>% mutate(IN_BAG_TIMESTAMP = as.POSIXct(strptime(IN_BAG_TIMESTAMP,"%d-%m-%Y %H:%M:%S")), BAG_CLOSED_TIMESTAMP = as.POSIXct(strptime(july9data$BAG_CLOSED_TIMESTAMP,"%d-%m-%Y %H:%M:%S")) )
july9data$tobc<-as.numeric((july9data$BAG_CLOSED_TIMESTAMP-july9data$IN_BAG_TIMESTAMP)/60) 
july9jdata_agg<-(select(july9data,Delhivery_Bag_Id,tobc)) %>% group_by(Delhivery_Bag_Id) %>% summarise(tobc=max(tobc)) %>% as.data.frame() 

##tobc=time of bag closing

july9data_filter <- july9data %>% inner_join(july9jdata_agg,by= c("Delhivery_Bag_Id"="Delhivery_Bag_Id","tobc"))
july9jdata_agg$bin <- cut( july9jdata_agg$tobc , breaks = c(0, seq(15,450, by=15)), labels = 0:29) ##created bins or range
xxx<- july9jdata_agg %>% mutate(bin=as.integer(bin)) %>% group_by(bin) %>% summarise(num_bags=length(Delhivery_Bag_Id))
p1<- plot_ly(data = xxx,x = bin,y = num_bags,marker=list(color = toRGB("red")), type = "bar",width = 1000)
p1

july9jdata_agg <- july9jdata_agg %>% inner_join(data2,by= c("Delhivery_Bag_Id"))
xx<- july9jdata_agg %>% mutate(bin=as.integer(bin))%>% select(Package_AWB,bin)%>%group_by(bin)%>% summarise(Mean_Package_AWB=mean(Package_AWB))
p2<- plot_ly(data = xx,x = bin,y = Mean_Package_AWB,marker=list(color = toRGB("blue")), type = "bar",width = 500)
p2

p1 %>% add_trace(data = xx,x = bin,y = Mean_Package_AWB,marker=list(color = toRGB("orange")), type = "bar",width = 500)

july9data$hour<- substr(july9data$INSCANNED_TIMESTAMP,12,13)
july9data_sorted<- july9data %>% select(Package_AWB,hour) %>% group_by(hour) %>% summarise(Package_sorted=length(Package_AWB))
july9data$hourbagged<- substr(july9data$BAG_CLOSED_TIMESTAMP,12,13)
july9data_bagclosed<- july9data %>% select(Package_AWB,hourbagged) %>% group_by(hourbagged) %>% summarise(Package_bagged=length(Package_AWB))

p3<- plot_ly(july9data_sorted,x=hour,y=Package_sorted,fill = "tozeroy")
p3 %>% add_trace(data = july9data_bagclosed,x = hourbagged,y = Package_bagged,fill = "tozeroy")

mean(july9data$tobc,na.rm=TRUE)
mean(july9data_filter$tobc,na.rm=TRUE)
mean(data2$Package_AWB,na.rm=TRUE)

july9data <- july9data %>% mutate(PRIMARY_SORTED_TIMESTAMP = as.POSIXct(strptime(PRIMARY_SORTED_TIMESTAMP,"%d-%m-%Y %H:%M:%S")), BAG_CLOSED_TIMESTAMP = as.POSIXct(strptime(july9data$BAG_CLOSED_TIMESTAMP,"%d-%m-%Y %H:%M:%S")) )
july9data$newtime<- as.numeric((july9data$BAG_CLOSED_TIMESTAMP-july9data$PRIMARY_SORTED_TIMESTAMP)/60) 
mean(july9data$newtime,na.rm=TRUE)
