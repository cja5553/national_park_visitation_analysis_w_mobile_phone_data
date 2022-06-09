# Reading the data and preparing it for the regression analysis. 
## Download library packages and load datasets

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


## Remove Puerto Rico, Hawaii, Alaska, US minor Islands. 

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

data_count<-merge(data_count,race,by.x=c("cbg","covid_era"),by.y=c("Geo_FIPS","covid_era"),all.y=T)
data_count<-subset(data_count,select=-c(white, african_american, native_american,asian_american, Hispanic))
data_count[is.na(data_count)] <- 0

DT= melt(data_count,
                measure.vars = c("percent_black", "percent_hispanic", "minorityportion","percent_indian","percent_asian"))
DT$variable<-factor(DT$variable,levels=c("minorityportion","percent_black","percent_hispanic","percent_asian","percent_indian"))

race.labs <-c("non whites","African Americans","Hispanics","Asian Americans","Native Americans") 
names(race.labs) <- c("minorityportion","percent_black","percent_hispanic","percent_asian","percent_indian")
DT$value<-(DT$value)*100


racial_plot<-ggplot(DT,aes(y=avg,x=value,group=covid_era))+geom_smooth(show.legend=TRUE,aes(linetype=covid_era))+labs(linetype="COVID-19 era")+facet_wrap(~variable,labeller = labeller(variable = race.labs))+ylab("Average annual visitations")+xlab("Racial Demographics (%)")+ggtitle("Comparison of aggregated visitations before and during/after COVID-19\n of all census blocks groups across distinct racial demographics")+scale_linetype_discrete(labels=c("Before COVID-19", "During/After COVID-19")) +coord_cartesian(ylim = c(0, 2.4)) 
racial_plot
