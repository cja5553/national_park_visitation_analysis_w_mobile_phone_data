library(data.table)
library(ggplot2)
library(scales) 
model1<-read.csv("cbg_data_model.csv")
model1$date_range_start<-as.Date(model1$date_range_start,"%Y-%m-%d")
model1$date_range_end<-as.Date(model1$date_range_end,"%Y-%m-%d")
d<-data.frame(date=as.Date(c("2020-01-21", "2020-03-11")), event=c("First US COVID case", "WHO declares COVID19 a global pandemic"))

model1<-subset(model1, select=c(visitor_count, date_range_start))
model1<-aggregate(visitor_count~., data=model1, sum, na.rm=TRUE)

ggplot(model1, aes(x = date_range_start, y = visitor_count)) + ggtitle("Visitor Count to all National Parks Jan 2018 - Apr 2021")+
  geom_area(fill='blue', alpha = 0.5, position = position_dodge(0.8),size=1)+ylab("visitor count")+xlab("date")+theme(legend.position="bottom", legend.text=element_text(size=6), legend.title = element_blank(),axis.text.x = element_text(angle = 90),
                                                                                                         legend.key.size = unit(0.5, 'cm'),axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20))+scale_x_date(date_breaks = "months", date_labels = "%Y-%m")+ guides(colour = guide_legend(nrow = 12))+geom_vline(data=d, mapping=aes(xintercept=date), color="blue") +geom_text(data=d, mapping=aes(x=date, y=0, label=event), size=3, angle=90, vjust=-0.4, hjust=0)
