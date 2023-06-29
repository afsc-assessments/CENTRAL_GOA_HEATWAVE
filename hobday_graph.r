d<-ggplot(hobday[year(day)>=2014],aes(y=SST_SEAS,x=day))+geom_line(aes(y=SST_SEAS2,x=day),color="red",size=2)+geom_line()+geom_line(aes(y=thresh_seas,x=day),color="black",size=1.25)

d




d<-ggplot(hobday[year(day)>=2014],aes(y=SST_thresh,x=day))+geom_line()


 d<-ggplot(hobday[year(day)>=2010],aes(y=SST_thresh,x=day))+geom_line()
> d<-d+geom_hline(aes(yintercept=0),color="red",size=1.5)
> d<-d+ylab(expression(paste(degree,"C from upper 90th percentile for day of year")))+xlab("Date")+theme_bw(16)
> d
> d<-ggplot(hobday[year(day)>=2010],aes(y=SST_SEAS,x=day))+geom_line()
> d<-d+geom_hline(aes(yintercept=0),color="red",size=1.5)
> d<-d+ylab(expression(paste(degree,"C from the mean by day of year")))+xlab("Date")+theme_bw(16)
> d
> 
