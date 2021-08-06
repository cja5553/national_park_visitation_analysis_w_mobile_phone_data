# Read necessary libraries and files

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



# Regression Analysis 
## African American Population  

model <- plm(log(ratio*1000) ~ covid_era+percent_black+median.age+(dist)+MEDIAN.INCOME+area_sqkm+covid_era:percent_black+covid_era:(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm, data = data ,index = c("date_range_start"), model = "within")

summary(model)

ols<-lm(log(ratio*1000) ~ covid_era+percent_black+median.age+(dist)+MEDIAN.INCOME+area_sqkm+covid_era:percent_black+covid_era:(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm+(date_range_start-1),data = data, na.rm=T)
summary(ols)
vif(ols) #VIF<10 for all

### Interaction plot 
interact_plot(ols, pred =percent_black, modx = covid_era, x.label = "Proportion of African Americans", y.label="log(visitation/population)")


## Hispanic Population  

model <- plm(log(ratio*1000) ~ covid_era+percent_hispanic+median.age+(dist)+MEDIAN.INCOME+area_sqkm+covid_era:percent_hispanic+covid_era:(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm, data = data ,index = c("date_range_start"), model = "within")
summary(model)

ols<-lm(log(ratio*1000) ~ covid_era+percent_hispanic+median.age+(dist)+MEDIAN.INCOME+area_sqkm+covid_era:percent_hispanic+covid_era:(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm+(date_range_start-1),data = data, na.rm=T)
summary(ols)

vif(ols) # VIF<10 for all
### no interaction plot since not significant 

## Asian-American Population  



model <- plm(log(ratio*1000) ~ covid_era+percent_asian+median.age+(dist)+MEDIAN.INCOME+area_sqkm+covid_era:percent_asian+covid_era:(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm, data = data ,index = c("date_range_start"), model = "within")


summary(model)
ols<-lm(log(ratio*1000) ~ covid_era+percent_asian+median.age+(dist)+MEDIAN.INCOME+area_sqkm+covid_era:percent_asian+covid_era:(dist)+covid_era:MEDIAN.INCOME+covid_era:median.age+covid_era:area_sqkm+(date_range_start-1),data = data, na.rm=T)
summary(ols)

vif(ols) # VIF < 10 for all

### no interaction plot since not significant 



