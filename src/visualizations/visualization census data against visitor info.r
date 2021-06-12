library(data.table)
library(ggplot2)
library(scales) 
library(bit64)
options(scipen = 999)
model1<-read.csv("processed_model_1.csv")
model2<-read.csv("processed_model_2.csv")
model1$date_range_start<-as.Date(model1$date_range_start,"%Y-%m-%d")
model1$date_range_end<-as.Date(model1$date_range_end,"%Y-%m-%d")

model1_precovid<-model1[(model1$date_range_start<"2020-03-01"),]
model1_precovid$covid_era<-"Before COVID19"
model1_covid<-model1[(model1$date_range_start>="2020-03-01"),]
model1_covid$covid_era<-"After COVID19"
model1<-rbind(model1_covid,model1_precovid)

model2_precovid<-model2[(model2$date_range_start<"2020-03-01"),]
model2_precovid$covid_era<-"Before COVID19"
model2_covid<-model2[(model2$date_range_start>="2020-03-01"),]
model2_covid$covid_era<-"After COVID19"
model2<-rbind(model2_covid,model2_precovid)

#mynames<-c("pre-COVID", "COVID")

#aggdata <-aggregate(visitor_count~., data=subset(model1, select=c(vistor_census_block, visitor_count, minorityportion, covid_era)), mean, na.rm=TRUE)
ggplot(data = model1, aes(x = minorityportion, y = visitor_count, color = covid_era)) +geom_point() +labs(title = "minority portion and monthly national parks visitations (model 1)",x = "minority portion from visitor census block group",y = "monthly visitations to distinct national parks from each census block group",color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())


ggplot(data = model1, aes(x = dist, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "distance and monthly national parks visitations (model 1)",
             x = "distance (meters) to distinct national parks from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())


ggplot(data = model1, aes(x = median.age, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "age and monthly national parks visitations (model 1)",
             x = "median age from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

ggplot(data = model1, aes(x = MEDIAN.INCOME, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "income and monthly national parks visitations (model 1)",
             x = "median income from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

# Model 2  

#mynames<-c("pre-COVID", "COVID")

#aggdata <-aggregate(visitor_count~., data=subset(model1, select=c(vistor_census_block, visitor_count, minorityportion, covid_era)), mean, na.rm=TRUE)
ggplot(data = model2, aes(x = minorityportion, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "minority portion and monthly national parks visitations (model 2)",
             x = "minority portion from visitor census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())


ggplot(data = model2, aes(x = dist, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "distance and monthly national parks visitations (model 2)",
             x = "distance (meters) to distinct national parks from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

ggplot(data = model2, aes(x = median.age, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "age and monthly national parks visitations (model 2)",
             x = "median age from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

ggplot(data = model2, aes(x = MEDIAN.INCOME, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "income and monthly national parks visitations (model 2)",
             x = "median income from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())