
# Download libraries
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
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

# read dataset
data<-read.csv("national_park_visitation_analysis_w_mobile_phone_data/volume/data/processed/processed_model.csv")

# classify COVID and non_covid era
data$date_range_start<-as.Date(data$date_range_start,"%Y-%m-%d")
data$date_range_end<-as.Date(data$date_range_end,"%Y-%m-%d")
data_non<-data[(data$date_range_start<"2020-03-01"),]
data_covid<-data[(data$date_range_start>="2020-03-01"),]
data_covid$covid_era<-TRUE
data_non$covid_era<-FALSE
data<-rbind(data_covid, data_non)
data<-subset(data, select = c(vistor_census_block,minorityportion,median.age,dist,MEDIAN.INCOME, visitor_count, PARKNAME, covid_era, date_range_start, population, area_sqkm))
#write.csv(data, file="model.csv")

# calculate visitation per population ratio
data$ratio<-(data$visitor_count)/(data$population)
data<-data[complete.cases(data), ]

# remove Hawaii, PR, Alaska, and US minor Islands
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

# Plotting the distribution plot

hist(data$visitor_count,breaks=200, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Visitor Count",
     main = "Density distribution Plot")
lines(density(data$visitor_count), # density plot
      lwd = 2, # thickness of line
      col = "red")

# Creating Different models and testing if they are suitable

## Creating the models  

### OLS model

ols<-lm(log(ratio*1000) ~ covid_era+minorityportion+median.age+log(dist)+MEDIAN.INCOME+area_sqkm+covid_era:minorityportion+covid_era:log(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm,data = data, na.rm=T)



### Fixed model
fixed_model <- plm(log(ratio*1000) ~ covid_era+minorityportion+median.age+log(dist)+MEDIAN.INCOME+area_sqkm+covid_era:minorityportion+covid_era:log(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm,data = data,index = c("date_range_start"), model="within")


### pooled model 

pooled_model <- plm(log(ratio*1000) ~ covid_era+minorityportion+median.age+log(dist)+MEDIAN.INCOME+area_sqkm+covid_era:minorityportion+covid_era:log(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm,data = data,index = c("date_range_start"), model="pooling")



### random model
random_model <- plm(log(ratio*1000) ~covid_era+minorityportion+median.age+log(dist)+MEDIAN.INCOME+area_sqkm+covid_era:minorityportion+covid_era:log(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm,data=data,index = c("date_range_start"), model="random")


## Testing to see which model is better
### Test for ols vs fixed_model
pFtest(ols, fixed_model) #reject ols


### test for fixed vs pooled_model

phtest(fixed_model, pooled_model) # REJECT POOLED MODEL

## Test for fixed vs random effects  

phtest(fixed_model, random_model) # fixed model is better


# Fixed model is the best so we can the fixed model results

summary(fixed_model)

# Alternative way of doing fixed model using lm()
fixed_2<-lm(log(ratio*1000) ~ covid_era+minorityportion+median.age+log(dist)+MEDIAN.INCOME+area_sqkm+covid_era:minorityportion+covid_era:log(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm+(date_range_start-1),data = data, na.rm=T)
summary(fixed_2)
#plot_model(ols, type = "pred", terms = c("covid_era", "MEDIAN.INCOME"))

# checking for collinearity
vif(fixed_2) # All VIF values < 10



# Getting the interaction plots
## Minority proportion

#devtools::install_github("jacob-long/jtools")
minority_plot<-interact_plot(fixed_2, pred = minorityportion, modx = covid_era,  x.label = "Proportion of Minority", y.label = "log(visitation/population)")


## Distance 
distance_plot<-interact_plot(fixed_2, pred = dist, modx = covid_era,  x.label = "Distance", y.label = "log(visitation/population)")

## Income 
income_plot<-interact_plot(fixed_2, pred = MEDIAN.INCOME, modx = covid_era,x.label = "Median Income", y.label = "log(visitation/population)",int.width = 0.9)


## median Age
age_plot<-interact_plot(fixed_2, pred = median.age, modx = covid_era,x.label = "Median Age", y.label = "log(visitation/population)",int.width = 0.9)



# Plotting the plots nicely in a grid

grid.arrange(
  minority_plot,
  distance_plot,
  income_plot,
  age_plot,
  nrow = 2,
  top=textGrob(
    "Interact Plots",
    gp = gpar(fontsize = 12),hjust=1
  )
)

