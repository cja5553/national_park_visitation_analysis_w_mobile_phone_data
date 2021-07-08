
require(sp)  
require(rgdal)
require(maps)
args = commandArgs(trailingOnly=TRUE)


# visualization 

data<-read.csv("small_filtered_pois.csv")
data<-subset(data,!is.na(data$latitude))
data<-subset(data,!is.na(data$longitude))
coordinates(data)<-c("longitude","latitude")
poly <- readOGR("National_Parks","National_Parks")


poly_wgs84 <- spTransform(poly, CRS("+proj=longlat +datum=WGS84"))
proj4string(data)<-proj4string(poly_wgs84)
plot(poly_wgs84)
plot(data, add=TRUE, col="red")
#plot(data, col="red")

