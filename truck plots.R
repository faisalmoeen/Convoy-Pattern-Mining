library(hexbin)

#plot trucks for days of month
plot(day(trucksFiltered$time),trucksFiltered$x,main="Trucks movement per day of month",
     xlab="days",ylab="x movement in meters")
grid(31,NA)
axis(1,c(1:31))

#3d plot of trucks
library(scatterplot3d)
scatterplot3d(trucksFiltered$x,trucksFiltered$y,day(trucksFiltered$time),main="Trucks movement per day of month",
              xlab="x",ylab="y",zlab="days")

#plot trucks for days of week
plot(wday(trucksFiltered$time),trucksFiltered$x,main="Trucks movement per day of week",
     xlab="days",ylab="x movement in meters")

#plot per hour of day
plot(hour(trucksFiltered$time),trucksFiltered$x,main="Trucks movement x per hour of day",
     xlab="hours",ylab="x movement in meters")

#plot per day of year
plot(yday(trucksFiltered$time),trucksFiltered$x,main="Trucks movement x per day of year",
     xlab="days",ylab="x movement in meters")
axis(1,c(219:259),las=3)
grid(259-219,NA)

#plot interpolated truck data from t=89129 to t=116617
trucks2plot=trucksSortedNumeric[trucksSortedNumeric$t>=89129,]
plot(trucks2plot$t,trucks2plot$x,main="Interpolated Trucks movement x ",
     xlab="time",ylab="x movement in meters",cex=0.2)

bin=hexbin(x=trucksSortedNumeric$t,y=trucksSortedNumeric$x,xbins=300)
plot(bin,main="binning")
