xx <- xx %>% as.data.frame()
xxx <- xxx %>% as.data.frame()
xx_join <- xx %>% inner_join(xxx)
p1<- plot_ly(data = xxx,x = bin,y = num_bags, type = "bar",width = 500)

p2<- plot_ly(data = xx,x = bin,y = Mean_Package_AWB, type = "bar",width = 500)
plot_ly(data = xx,x = bin,y = Mean_Package_AWB, type = "bar",width = 500) %>% 
  add_trace(data = xxx, x = bin, y = num_bags, type = "bar",width = 500)
p1
p2
p1 %>% add_trace(p2)
p2 %>% add_trace(p1)
