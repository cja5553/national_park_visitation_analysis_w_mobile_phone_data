library(effects)
library(cowplot)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ggpubr)
library(effects)
library(stargazer)
library(purrr)
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
library(ggplot2)
library(sjPlot)
library(sjmisc)
data<-read.csv("national_park_visitation_analysis_w_mobile_phone_data/volume/data/processed/processed_model.csv")
data<-subset(data, select = c(vistor_census_block,minorityportion,median.age,dist,MEDIAN.INCOME, visitor_count, PARKNAME, date_range_start))
data<-data[complete.cases(data), ]
data$dist<-(data$dist)/1000 # Standardize distance to kilometers



## reading race data and structuring it. 


library(dplyr)
data_count<-subset(data,select=c(vistor_census_block,visitor_count,covid_era))
data_count<-aggregate(data_count$visitor_count,by=list(data_count$vistor_census_block,data_count$covid_era),FUN=sum)

data_count<-data_count %>% 
  rename(
    cbg = Group.1,
    covid_era = Group.2,
    total_count=x
  )

pre_covid<-(data_count[data_count$covid_era==F,])
post_covid<-((data_count[data_count$covid_era==T,]))
pre_covid$avg<-((pre_covid$total_count)/26)*12
post_covid$avg<-((post_covid$total_count/14))*12
data_count<-rbind(pre_covid,post_covid)
race<-read.csv("national_park_visitation_analysis_w_mobile_phone_data/volume/data/interim/racial_demographics/census_info.csv")
race<-subset(race,select=-c(X,Geo_GEOID))
race2<-race
race$covid_era<-TRUE
race2$covid_era<-FALSE
race<-rbind(race,race2)


# removing Puerto Rico, US minor Islands and Hawaii. 
race$length<-floor(log10(race$Geo_FIPS)) + 1
data_eleven<-race[(race$length==11),]
data_eleven$first<-as.numeric(substr(data_eleven$Geo_FIPS, 1, 1))
data_twelve<-race[(race$length==12),]
data_twelve$first_second<-as.numeric(substr(data_twelve$Geo_FIPS, 1, 2))
data_eleven<-data_eleven[!data_eleven$first == 2, ]
cbg_unwanted<-c(60,66,69,72,78,15)
data_twelve<-data_twelve[!data_twelve$first_second %in% cbg_unwanted,]
data_eleven<-subset(data_eleven, select=-c(length, first))
data_twelve<-subset(data_twelve, select=-c(length, first_second))
race<-rbind(data_eleven,data_twelve)


# restructuring the data
data_count<-merge(data_count,race,by.x=c("cbg","covid_era"),by.y=c("Geo_FIPS","covid_era"),all.y=T)
data_count<-subset(data_count,select=-c(white, african_american, native_american,asian_american, Hispanic))
data_count[is.na(data_count)] <- 0

DT= melt(data_count,
         measure.vars = c("percent_black", "percent_hispanic", "minorityportion","percent_indian","percent_asian"))
DT$variable<-factor(DT$variable,levels=c("minorityportion","percent_black","percent_hispanic","percent_asian","percent_indian"))

DT<-subset(DT,select=c(cbg,covid_era,avg))
DT<-DT%>%distinct()
DT<-dcast(DT, cbg ~ covid_era, value.var = "avg")
DT$before_covid<-DT[,2]
DT$after_covid<-DT[,3]
DT<-subset(DT,select=c(cbg,before_covid,after_covid))
DT$DT_diff_percent<-((DT$after_covid-DT$before_covid)/(DT$before_covid))*100
DT[is.na(DT)] <- 0



bins<-function(value){
  
  if (value==0)
  {
    bin="0%"
  }
  else if (value<0&&value>=-50)
  {
    bin="-50% to -0.001%"
    
  }
  
  else if (value<=-50)
  {
    bin="-100% to -50.0001%"
  }
  
  else if (value>0&&value<=50)
  {
    
    bin="0.001% to 50%"
  }
  else if (value>50&&value<=100)
  {
    bin="50.001% to 100%"
  }
  
  else if (value>100)
  {
    bin="> 100%"
  }
  
  return(bin)
}

percent_change=c()
for (i in 1:nrow(DT)){
  percent_change[i]=bins((DT$DT_diff_percent)[i])
  
}

# combining our data with the shapefiles so we could plot the map. 

DT1<-DT
DT1$percent_change<-percent_change
comb_cbg<-readOGR("shapefiles","shapefiles")
comb_cbg$cbg<-as.numeric(comb_cbg$GEOID)
comb_shp<-merge(comb_cbg,DT1,by.x="cbg",by.y="cbg",all.x=T)

race<-read.csv("national_park_visitation_analysis_w_mobile_phone_data/volume/data/interim/racial_demographics/census_info.csv")
race<-subset(race, select =c(Geo_FIPS,percent_hispanic,percent_asian,percent_indian,percent_black,minorityportion))
race[is.na(race)] <- 0
comb_shp<-merge(comb_cbg,race,by.x="cbg",by.y="Geo_FIPS",all.x=T)

writeOGR(comb_shp, ".", "interim/shpfiles_for_maps/race_plot_shapefiles", driver = "ESRI Shapefile") # this will be read in QGIS to map the maps. 

# this is to prepare the shapefiles for fig 3e and 7e to understand the impact of COVID-19 on non-white and native american communities based on our results
data$population_1000<-(data$population)/1000
random_model <- plm(log(visitor_count) ~covid_era+ log(percent_indian)+log(dist)+covid_era:log(percent_indian)+covid_era:log(dist)+log(dist):log(percent_indian)+covid_era:log(dist):log(percent_indian)+log(population_1000),data=data,index = c("date_range_start"), model="random")
data_predict<-subset(data,select=-c(visitor_count))
data_predict$native_predicted_visitation<-predict(random_model,newdata=data_predict)

## running the model to predict the impact 
random_model <- plm(log(visitor_count) ~covid_era+log(minorityportion)+log(dist)+covid_era:log(minorityportion)+log(minorityportion)+covid_era:log(dist)+log(dist):log(minorityportion)+covid_era:log(dist):log(minorityportion)+log(population_1000),data = data,index = c("date_range_start"), model="random")
data_predict$minority_predicted_visitation<-predict(random_model,newdata=data_predict)
data_predict<-subset(data_predict,select=c(vistor_census_block,covid_era,minority_predicted_visitation,native_predicted_visitation))

data_predict = aggregate(cbind(minority_predicted_visitation, native_predicted_visitation) ~covid_era+vistor_census_block, data=data_predict,FUN = sum)
data_predict<-subset(data_predict,select=c(covid_era, vistor_census_block,minority_predicted_visitation, native_predicted_visitation))

## aggregating the impact 
pre_covid<-(data_predict[data_predict$covid_era==F,])
post_covid<-((data_predict[data_predict$covid_era==T,]))
pre_covid$native_pred_avg<-((pre_covid$native_predicted_visitation)/26)
pre_covid$minority_pred_avg<-((pre_covid$minority_predicted_visitation)/26)
post_covid$native_pred_avg<-((post_covid$native_predicted_visitation/14))
post_covid$minority_pred_avg<-((post_covid$minority_predicted_visitation/14))
pred<-rbind(pre_covid,post_covid)

## structuring the data

race_predict<-subset(race,select=c(Geo_FIPS,covid_era))
pred<-subset(pred,select=c(vistor_census_block,covid_era,minority_pred_avg,native_pred_avg))
pred<-merge(pred,race_predict,by.x=c("vistor_census_block","covid_era"),by.y=c("Geo_FIPS","covid_era"),all.y=T,all.x=F)
pred[is.na(pred)] <- 0
pred<-subset(pred,select=c(vistor_census_block,covid_era,minority_pred_avg,native_pred_avg))
pred$covid_era[pred$covid_era == TRUE] <- "covid"
pred$covid_era[pred$covid_era == FALSE] <- "no_covid"
#pred<-dcast(pred, vistor_census_block ~ covid_era, value.var = c("minority_pred_avg","native_pred_avg"))
pred1<-subset(pred,select=c(vistor_census_block,covid_era,minority_pred_avg))
pred1<-dcast(pred1, vistor_census_block ~ covid_era, value.var ="minority_pred_avg")
pred1<-pred1 %>% 
  rename(
    non_white_covid=covid ,
    non_white_no=no_covid 
  )


pred2<-subset(pred,select=c(vistor_census_block,covid_era,native_pred_avg))
pred2<-dcast(pred2, vistor_census_block ~ covid_era, value.var ="native_pred_avg")
pred2<-pred2 %>% 
  rename(
    native_covid=covid ,
    native_no=no_covid
  )
pred<-merge(pred1,pred2,by.x="vistor_census_block",by.y="vistor_census_block")
pred$minority_diff<-((pred$non_white_covid-pred$non_white_no)/pred$non_white_no)*100
pred$native_diff<-((pred$native_covid-pred$native_no)/pred$native_no)*100
pred[is.na(pred)] <- 0

pred<-subset(pred,select=c(vistor_census_block,minority_diff,native_diff))

## placing them nicely in bins 
change_non_white=c()
change_native=c()
for (i in 1:nrow(pred)){
  change_non_white[i]=bins((pred$minority_diff)[i])
  change_native[i]=bins((pred$native_diff)[i])
}
pred$change_non_white<-change_non_white
pred$change_native<-change_native

comb_cbg<-readOGR("shapefiles","shapefiles")
comb_cbg$cbg<-as.numeric(comb_cbg$GEOID)
comb_shp<-merge(comb_cbg,pred,by.x="cbg",by.y="vistor_census_block",all.x=T)
writeOGR(comb_shp, ".", "interim/shpfiles_for_maps/fig_3e_7e", driver = "ESRI Shapefile") # this will be read in QGIS to map the maps. 



