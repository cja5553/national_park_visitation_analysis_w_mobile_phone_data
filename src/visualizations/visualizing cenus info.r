library(data.table)
library(ggplot2)
library(scales) 
library(bit64)
options(scipen = 999)
census<-read.csv("census_info.csv")
model1<-read.csv("processed_model_1.csv")
model2<-read.csv("processed_model_2.csv")

# Race Data

boxplot(census$minorityportion, main="Range Proportion of Minority of census block groups", ylab="Proportion of Minority")


# income data
boxplot(census$MEDIAN.INCOME, main="Range Median Income of census block groups", ylab="Median Income")



# Distance Data
model_1=model1$dist
model_2=model2$dist
boxplot(model_1,model_2,names = c("model 1", "model 2"),main="Range distance from park to census block group", ylab="distance")

# Age Data
boxplot(census$median.age, main="Range median age of census block groups", ylab="median age")


# African American Data  

boxplot(census$percent_black, main="Range proportion of African Americans of census block groups", ylab="Percent of African Americans")

# Hispanic Data
boxplot(census$percent_hispanic, main="Range proportion of Hispanic of census block groups", ylab="percent of Hispanics")


# Asian Data
boxplot(census$percent_asian, main="Range proportion of Asian Americans of census block groups", ylab="Percent of Asian Americans")


# Indian Data 
boxplot(census$percent_indian, main="Range proportion of Indian Americans Americans of census block groups", ylab="Percent of Indian Americans")


