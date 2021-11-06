#new_data<-data[!complete.cases(data), ]
data$date_range_start<-month(as.POSIXlt(data$date_range_start, format="%d/%m/%Y"))
data$date_range_start<-as.character(data$date_range_start)
data<-data[complete.cases(data), ]






curve(10*(0.00144-0.000250*log(x)), xlim=c(min(data$dist),max(data$dist)), xlab="Distance in Km", ylab="\n\n% Change in Visitations",col="Red", lwd=8, main="% change in visitations for every 10% increase\nin % non-whites due to COVID-19", cex.lab=1.7,cex.axis=1.7,cex.main=2)
p <- c(317,0)
points(t(p), pch=20)

clip(min(-200), max(319), min(-0.2), max(0.0002))
abline(v=317,h=0,lty="dashed",col = "blue", lwd=8)
axis(1, at = seq(317, 317), las=1, cex.axis=1.7, cex.lab=1.7, col="blue")
axis(2, at = seq(0, 0), cex.axis=1.7, cex.lab=1.7)
#abdata <- data.frame(x=317, y=-0.01:0)
#Map(abline, abdata$a, abdata$b, lty="dotted",col = "gray60")


curve(10*(-0.0166-0.000250*log(x)), xlim=c(0,1), xlab="% non-whites", ylab="% Change in Visitations",col="Red", lwd=8, main="% change in visitations for every 10% increase\nin distance due to COVID-19", cex.lab=1.7,cex.axis=1.7,cex.main=2)

curve(10*(-0.00189+0.000235*log(x)), xlim=c(min(data$dist),max(data$dist)), xlab="Distance in Km", ylab="% Change in Visitations",col="Red", lwd=8, main="% change in visitations for every 10% increase\nin % of African Americans", cex.lab=1.7,cex.axis=1.7,cex.main=2)

curve(10*(0.00066-0.000121*log(x)), xlim=c(min(data$dist),max(data$dist)), xlab="Distance in Km", ylab="% Change in Visitations",col="Red", lwd=8, main="% change in visitations for every 10% increase\nin % of Hispanics", cex.lab=1.7,cex.axis=1.7,cex.main=2)
p <- c(234,0)
points(t(p), pch=16)

clip(min(-200), max(240), min(-0.2), max(0.000004))
abline(v=234,h=0,lty="dashed",col = "blue", lwd=8)
axis(1, at = seq(234, 234), las=1, cex.axis=1.7, cex.lab=1.7)

curve(10*(0.00124-0.0154*log(x)), xlim=c(min(data$dist),max(data$dist)), xlab="Distance in Km", ylab="% Change in Visitations",col="Red", lwd=8, main="% change in visitations for every 10% increase\n% of Asian-Americans", cex.lab=1.7,cex.axis=1.7,cex.main=2)

curve(10*(0.00249-0.000403*log(x)), xlim=c(min(data$dist),max(data$dist)), xlab="Distance in Km", ylab="% Change in Visitations",col="Red", lwd=8, main="% change in visitations for every 10% increase\n% of Native-Americans due to COVID-19", cex.lab=1.7,cex.axis=1.7,cex.main=2)
p <- c(482,0)
points(t(p), pch=16)

clip(min(-200), max(489), min(-0.2), max(0.0002))
abline(v=482,h=0,lty="dashed",col = "blue", lwd=8)
axis(1, at = seq(482, 482), las=1, cex.axis=1.7, cex.lab=1.7)


curve(10*(-0.0166-0.000250*log(x)), xlim=c(0,1), xlab="% Native Americans", ylab="% Change in Visitations",col="Red", lwd=8, main="% change in visitations for every 10% increase\n in distance due to COVID-19", cex.lab=1.7,cex.axis=1.7,cex.main=2)
