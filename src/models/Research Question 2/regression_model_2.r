# Read neccessary libraries. 

library(plm)
library(data.table)
library(bit64)
options(scipen = 999)
library(knitr)
library(broom)
library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()`
library(forecast) 
library(systemfit)
library(AER)
library(xtable)
library(jtools)
library(devtools)
library(interactions)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)


# Aggregate the data to find the total number of tourists from each census_block_group visiting all parks
data<-read.csv("national_park_visitation_analysis_w_mobile_phone_data/volume/data/processed/processed_model.csv")
data$date_range_start<-as.Date(data$date_range_start,"%Y-%m-%d")
data$date_range_end<-as.Date(data$date_range_end,"%Y-%m-%d")
data_non<-data[(data$date_range_start<"2020-03-01"),]
data_covid<-data[(data$date_range_start>="2020-03-01"),]
data_covid$covid_era<-TRUE
data_non$covid_era<-FALSE
data<-rbind(data_covid, data_non)

# find the variable dist to nearest park
dist_to_nearest_park<-subset(data,select=c(dist,vistor_census_block,PARKNAME,date_range_start))
dist_to_nearest_park <- dist_to_nearest_park[order(dist_to_nearest_park$dist, decreasing = FALSE), ]  # Top N lowest values by group
dist_to_nearest_park <- data.table(dist_to_nearest_park , key = "vistor_census_block")
dist_to_nearest_park  <- dist_to_nearest_park[ , head(.SD, 1), by = vistor_census_block]
total_visitor_count<-subset(data,select=c(vistor_census_block,covid_era,median.age,date_range_start,minorityportion,MEDIAN.INCOME,visitor_count))
aggdata <-aggregate(visitor_count~., data=total_visitor_count, sum, na.rm=TRUE)
aggdata<-merge(aggdata,dist_to_nearest_park, by.x=c("vistor_census_block","date_range_start"), by.y=c("vistor_census_block","date_range_start"), all.x=T)

# remove Hawaii, PR, Alaska, US minor Islands. 
data<-data.table(aggdata)
data$ratio<-(data$visitor_count)/(data$population)
data<-data[complete.cases(data), ]
data$length<-floor(log10(data$vistor_census_block)) + 1
data_eleven<-data[(data$length==11),]
data_eleven$first<-as.numeric(substr(data_eleven$vistor_census_block, 1, 1))
data_twelve<-data[(data$length==12),]
data_twelve$first_second<-as.numeric(substr(data_twelve$vistor_census_block, 1, 2))
data_eleven<-data_eleven[!data_eleven$first == 2, ]
cbg_unwanted<-c(60,66,69,72,78,15)
data_twelve<-data_twelve[!data_twelve$first_second %in% cbg_unwanted,]
data_eleven<-subset(data_eleven, select=-c(length, first))
data_twelve<-subset(data_twelve, select=-c(length, first_second))
data<-rbind(data_eleven,data_twelve)
data$date_range_start<-month(as.POSIXlt(data$date_range_start, format="%d/%m/%Y"))
data$date_range_start<-as.character(data$date_range_start)
data<-data[complete.cases(data), ]

# create the regression model
model <- plm(log(ratio*1000) ~ covid_era+minorityportion+median.age+(dist)+MEDIAN.INCOME+area_sqkm+covid_era:minorityportion+covid_era:(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm, data = data ,index = c("date_range_start"), model = "within")
summary(model)

# alternative way to create the model. 
fixed_2<-lm(log(ratio*1000) ~ covid_era+minorityportion+median.age+(dist)+MEDIAN.INCOME+area_sqkm+covid_era:minorityportion+covid_era:(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm+(date_range_start-1),data = data, na.rm=T)
vif(fixed_2) # checking for collinearity. VIF<10 for all values. 

# Plotting the interaction terms for significant variables. 
minority_plot<-interact_plot(fixed_2, pred = minorityportion, modx = covid_era,  x.label = "Proportion of Minority", y.label = "log(visitation/population)")
income_plot<-interact_plot(fixed_2, pred = MEDIAN.INCOME, modx = covid_era,x.label = "Median Income", y.label = "log(visitation/population)",int.width = 0.9)
age_plot<-interact_plot(fixed_2, pred = median.age, modx = covid_era,x.label = "Median Age", y.label = "log(visitation/population)",int.width = 0.9)

# Organizing the interaction terms. 
grid.arrange(
  minority_plot,
  income_plot,
  age_plot,
  nrow = 2,
  top=textGrob(
    "Interact Plots",
    gp = gpar(fontsize = 12),hjust=1
  )
)


