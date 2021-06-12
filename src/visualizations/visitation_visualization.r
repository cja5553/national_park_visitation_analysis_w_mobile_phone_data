
library(data.table)
library(ggplot2)
library(scales) 
model1<-read.csv("model1.csv")
model1$date_range_start<-as.Date(model1$date_range_start,"%Y-%m-%d")
model1$date_range_end<-as.Date(model1$date_range_end,"%Y-%m-%d")
d<-data.frame(date=as.Date(c("2020-01-21", "2020-03-11")), event=c("First US COVID case", "WHO declares COVID19 a global pandemic"))

ggplot(model1, aes(x = date_range_start, y = raw_visitor_counts)) + ggtitle("Visitor Count to National Parks Jan 2018 - Apr 2021 (Using Model 1)")+
  geom_area(aes(color = UNIT_NAME, fill = UNIT_NAME), 
            alpha = 0.5, position = position_dodge(0.8))+ylab("visitor count")+xlab("date")+theme(legend.position="bottom", legend.text=element_text(size=6), legend.title = element_blank(),axis.text.x = element_text(angle = 90),
       legend.key.size = unit(0.5, 'cm'),axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20))+scale_x_date(date_breaks = "months", date_labels = "%Y-%m")+ guides(colour = guide_legend(nrow = 12))+geom_vline(data=d, mapping=aes(xintercept=date), color="blue") +geom_text(data=d, mapping=aes(x=date, y=0, label=event), size=3, angle=90, vjust=-0.4, hjust=0)

combined<-subset(model1, select=c(safegraph_place_id,raw_visitor_counts,UNIT_NAME,PARKNAME,location_name, covid_era))
combined[is.na(combined)] <- 0
aggdata <-aggregate(raw_visitor_counts~., data=combined, mean, na.rm=TRUE)

aggdata_precovid<-aggdata[(aggdata$covid_era==F),]
aggdata_precovid$covid_era<-"Before COVID19"
aggdata_covid<-aggdata[(aggdata$covid_era==T),]
aggdata_covid$covid_era<-"During / After COVID"
aggdata<-rbind(aggdata_precovid,aggdata_covid)




model1_precovid<-model1[(model1$date_range_start<"2020-03-01" & model1$date_range_start>="2019-03-01"),]
model1_precovid$covid_era<-"Before COVID19 (2019 March to 2020 Feb)"
model1_covid<-model1[(model1$date_range_start>="2020-03-01" & model1$date_range_start<"2021-03-01"),]
model1_covid$covid_era<-"During / After COVID (2020 March to 2021 Feb)"
model1<-rbind(model1_covid,model1_precovid)


combined<-subset(model1, select=c(safegraph_place_id,raw_visitor_counts,PARKNAME, covid_era))
combined[is.na(combined)] <- 0

aggdata <-aggregate(raw_visitor_counts~., data=combined, mean, na.rm=TRUE)

aggdata_precovid<-aggdata[(aggdata$covid_era=="Before COVID19 (2019 March to 2020 Feb)"),]
aggdata_precovid$covid_era<-"Before COVID19 (2019 March to 2020 Feb)"
aggdata_covid<-aggdata[(aggdata$covid_era=="During / After COVID (2020 March to 2021 Feb)"),]
aggdata_covid$covid_era<-"During / After COVID (2020 March to 2021 Feb)"
aggdata<-rbind(aggdata_precovid,aggdata_covid)




ggplot(aggdata, aes(x=covid_era,y=raw_visitor_counts,fill=PARKNAME)) + ylab("average monthly visitors")+xlab("")+
  geom_bar(stat="identity",position=position_dodge())+theme(legend.position="bottom", legend.text=element_text(size=6), legend.title = element_blank(),
                                                            legend.key.size = unit(0.5, 'cm'))+ guides(colour = guide_legend(nrow = 6))

ggplot(aggdata, aes(fill=forcats::fct_rev(reorder(covid_era,covid_era)),x=raw_visitor_counts,y=forcats::fct_rev(reorder(PARKNAME,PARKNAME)))) + xlab("average monthly visitors")+ylab("Park Name")+
  geom_bar(stat="identity",position=position_dodge())+theme(legend.position="bottom",legend.title = element_blank())+scale_fill_discrete(guide = guide_legend(reverse=TRUE))


ggplot(model1, aes(fill=forcats::fct_rev(reorder(covid_era,covid_era)),x=raw_visitor_counts,y=forcats::fct_rev(reorder(PARKNAME,PARKNAME)))) + ylab("monthly visitors")+xlab("Park Name")+
  geom_boxplot(outlier.colour="red", outlier.shape=4,
               outlier.size=0.8)+theme(legend.position="bottom",axis.text.x = element_text(angle = 90), legend.title = element_blank())
