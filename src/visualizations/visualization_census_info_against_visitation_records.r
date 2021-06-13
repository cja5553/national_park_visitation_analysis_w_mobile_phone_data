
# Reading libraries and files 
library(data.table)
library(ggplot2)
library(scales) 
library(bit64)
options(scipen = 999)
census<-read.csv("census_info.csv")
model1<-read.csv("model1.csv")
model2<-read.csv("model2.csv")

# for model 1
## plotting proportion of minorities against visitation records
ggplot(data = model1, aes(x = minorityportion, y = visitor_count, color = covid_era)) +geom_point() +labs(title = "minority portion and monthly national parks visitations (model 1)",x = "minority portion from visitor census block group",y = "monthly visitations to distinct national parks from each census block group",color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

# for model 1
## plotting distance against visitation records
ggplot(data = model1, aes(x = dist, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "distance and monthly national parks visitations (model 1)",
             x = "distance (meters) to distinct national parks from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())


## plotting age against visitation records
ggplot(data = model1, aes(x = median.age, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "age and monthly national parks visitations (model 1)",
             x = "median age from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

## plotting income against visitation records
ggplot(data = model1, aes(x = MEDIAN.INCOME, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "income and monthly national parks visitations (model 1)",
             x = "median income from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

# Model 2  

 ## plotting minority portion against visitation records
ggplot(data = model2, aes(x = minorityportion, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "minority portion and monthly national parks visitations (model 2)",
             x = "minority portion from visitor census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

## plotting distance against visitation records
ggplot(data = model2, aes(x = dist, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "distance and monthly national parks visitations (model 2)",
             x = "distance (meters) to distinct national parks from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

## plotting median age against visitation records
ggplot(data = model2, aes(x = median.age, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "age and monthly national parks visitations (model 2)",
             x = "median age from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())


## plotting median income
ggplot(data = model2, aes(x = MEDIAN.INCOME, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "income and monthly national parks visitations (model 2)",
             x = "median income from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())



# now plotting specific racial demographics against visitation records
## reading and filtering the data

model1_processed<-read.csv("processed_model_1.csv")
model2_processed<-read.csv("processed_model_2.csv")

census<-subset(census, select=c(percent_hispanic,percent_asian,Geo_FIPS,percent_indian,percent_black))
model1_processed<-merge(model1_processed,census,by.x="vistor_census_block",by.y="Geo_FIPS", all.x = T)
model2_processed<-merge(model2_processed,census,by.x="vistor_census_block",by.y="Geo_FIPS", all.x = T)


model1_processed_precovid<-model1_processed[(model1_processed$date_range_start<"2020-03-01"),]
model1_processed_precovid$covid_era<-"Before COVID19"
model1_processed_covid<-model1_processed[(model1_processed$date_range_start>="2020-03-01"),]
model1_processed_covid$covid_era<-"During / After COVID"
model1_processed<-rbind(model1_processed_covid,model1_processed_precovid)

model2_processed_precovid<-model2_processed[(model2_processed$date_range_start<"2020-03-01"),]
model2_processed_precovid$covid_era<-"Before COVID19"
model2_processed_covid<-model2_processed[(model2_processed$date_range_start>="2020-03-01"),]
model2_processed_covid$covid_era<-"During / After COVID"
model2_processed<-rbind(model2_processed_covid,model2_processed_precovid)

## for model 1


### African Americans and COVID visitations

ggplot(data = model1_processed, aes(x = percent_black, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "percent of African Americans and monthly national parks visitations (model 1)",
             x = "percent of African Americans from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())


### Hispanics and COVID visitations
ggplot(data = model1_processed, aes(x = percent_hispanic, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "percent of hispanics and monthly national parks visitations (model 1)",
             x = "percent of Hispanics from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

### Asian Americans and COVID visitations

ggplot(data = model1_processed, aes(x = percent_asian, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "percent of Asian Americans and monthly national parks visitations (model 1)",
             x = "percent of Asian Americans from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

### Indians and COVID visitations

ggplot(data = model1_processed, aes(x = percent_indian, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "percent of Indian Americans and monthly national parks visitations (model 1)",
             x = "percent of Indian Americans from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())


## Model 2. 
### African Americans and COVID visitations

ggplot(data = model2_processed, aes(x = percent_black, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "percent of African Americans and monthly national parks visitations (model 2)",
             x = "percent of African Americans from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

### Hispanics and COVID visitations
ggplot(data = model2_processed, aes(x = percent_hispanic, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "percent of hispanics and monthly national parks visitations (model 2)",
             x = "percent of Hispanics from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

### Asian Americans and COVID visitations

ggplot(data = model2_processed, aes(x = percent_asian, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "percent of Asian Americans and monthly national parks visitations (model 2)",
             x = "percent of Asian Americans from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())

### Indians and COVID visitations

ggplot(data = model2_processed, aes(x = percent_indian, y = visitor_count, color = covid_era)) +
        geom_point() +
        labs(title = "percent of Indian Americans and monthly national parks visitations (model 2)",
             x = "percent of Indian Americans from each census block group",
             y = "monthly visitations to distinct national parks from each census block group", color="COVID/pre-covid era")+theme(axis.title.y = element_text(size = 8),legend.title = element_blank())
