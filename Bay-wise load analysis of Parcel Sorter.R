sorterdata<-read.csv("15day_Bhiwandi.csv",header=T,stringsAsFactors = F) ##reading 15 days Bhiwandi sorter data
library(dplyr) 
a<-sorterdata %>% select(Package_AWB,PRIMARY_SORT_BAY_ID) %>% group_by(PRIMARY_SORT_BAY_ID) %>% summarise(num_packages = length(Package_AWB)) %>% as.data.frame() ## for aggregating number of shipments against each bay
b<-sorterdata %>% select(Package_AWB,SECONDARY_SORT_PTL_ID) %>% group_by(SECONDARY_SORT_PTL_ID) %>% summarise(num_packages = length(Package_AWB)) %>% as.data.frame() ## for aggregating number of shipments against each PTL

c<- subset(a,!is.na(a$PRIMARY_SORT_BAY_ID) & a$PRIMARY_SORT_BAY_ID != "")
library(stringr)
c$bayid<- substr(c$PRIMARY_SORT_BAY_ID,5,6)
d<- order(c$bayid)
c$bayid <- as.numeric(c$bayid)
d<- c[order(c$bayid),] 

library(plotly)
p <- plot_ly(data = d,x = PRIMARY_SORT_BAY_ID,y = num_packages ,type = "bar",width = 10)
p

e<-b[which(b$SECONDARY_SORT_PTL_ID>1500 & b$SECONDARY_SORT_PTL_ID<1600),]
p1<- plot_ly(data = e,x = SECONDARY_SORT_PTL_ID,y = num_packages ,type = "bar",width = 10)
p1

f<-b[which(b$SECONDARY_SORT_PTL_ID>1300 & b$SECONDARY_SORT_PTL_ID<1400),]
p2<- plot_ly(data = f,x = SECONDARY_SORT_PTL_ID,y = num_packages ,type = "bar",width = 10)
p2