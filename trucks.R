library(zoo)

trucksRaw=read.csv2(file="Trucks.txt",header=FALSE,sep=";",stringsAsFactors=F);
names(trucksRaw)=c("oid","tid","date","time","lat","long","x","y");
#combine date and time columns into one string col
dtStr=paste(trucksRaw$date,trucksRaw$time)
#convert string time column into POSIXct object
dt=as.POSIXct( strptime(x=dtStr,format="%d/%m/%Y %H:%M:%S") )
#filter out unnecessary columns and add time object into the dataframe
trucksFiltered=data.frame(time=dt,oid=trucksRaw$oid,x=as.numeric(trucksRaw$x),y=as.numeric(trucksRaw$y),lat=as.numeric(trucksRaw$lat),long=as.numeric(trucksRaw$long))
#trucksFilteredll=data.frame(time=dt,oid=trucks$oid,x=as.numeric(trucks$long),y=as.numeric(trucks$lat))
#write.csv(file='trucks-filtered.txt',trucksFiltered,row.names=FALSE)



#Adjust to 30 sec sampling interval
trucksRounded=trucksFiltered
previousOid=0
currentOid=0
require(lubridate)
for(i in 1:nrow(trucksFiltered)){
        row=trucksFiltered[i,]
        if(previousOid==0 | currentOid!=row$oid){
                previousOid=currentOid
                currentOid=row$oid
                print(currentOid)
        }
        if(second(row$time)>=16 & second(row$time)<=45){
                newtime=row$time-second(row$time)+30
        }else {
                newtime=round(row$time,'mins')
        }
        print(paste(i,":old=",row$time,"::new=",newtime))
        trucksRounded[i,"time"]=as.POSIXct(x=newtime)
}
#write the trucksRounded to the file as it takes a lot of time to recompute
write.csv(file='trucksRounded.txt',trucksRounded,row.names=FALSE)

###############################interpolation#################################

oids=unique(trucksRounded[,"oid"])
trucksIp=data.frame()
timeoids=data.frame()
for(o in oids){
        currentTruck = trucksRounded[trucksRounded$oid==o,]
        mintime=min(currentTruck[,"time"])
        maxtime=max(currentTruck[,"time"])
        timeseq=seq(from=mintime,to=maxtime,by=30)
        #print(data.frame(head(timeseq),o))
        timeoids=data.frame(stringsAsFactors=FALSE,time=timeseq,oid=o)
        mer=merge(timeoids,currentTruck,all=TRUE)
        zooX=zoo(mer$x,mer$time)
        zooY=zoo(mer$y,mer$time)
        zooLat=zoo(mer$lat,mer$time)
        zooLong=zoo(mer$long,mer$time)
        ipX= na.approx(zooX)
        ipY= na.approx(zooY)
        ipLat= na.approx(zooLat)
        ipLong= na.approx(zooLong)
        currentIpTruck=data.frame(time=mer$time,oid=mer$oid,x=ipX,y=ipY,lat=ipLat,long=ipLong)
        trucksIp=rbind(trucksIp,currentIpTruck)
}


#********************making trajectory of each day a new object***********************
trucks = trucksIp;
#make a new oid column with month-day concatenated
oidd=paste(trucks$oid,month(trucks$time),day(trucks$time),sep='')
trucksOidd=data.frame(oidd,time=trucks$time,x=trucks$x,y=trucks$y,lat=trucks$lat, long=trucks$long, stringsAsFactors=FALSE)        
#sort the data according to time
trucksSorted=trucksOidd[order(trucksOidd$time,trucksOidd$oidd),]
write.csv(file='trucksSorted.txt',trucksSorted,row.names=FALSE)

#add a numeric tvcolumn
min=min(trucksSorted$time)
trucksSortedNumeric=data.frame(trucksSorted,t=(trucksSorted$time-min)/30)
write.csv(file='trucksSortedNumeric.txt',trucksSortedNumeric,row.names=FALSE,quote=FALSE)

##############################Partitioning#################
partitionSize=500
trucks2partition=trucksSortedNumeric[trucksSortedNumeric$t>=105000 & trucksSortedNumeric$t<=111000,]
start=min(trucks2partition$t)
end=max(trucks2partition$t)
pointer=start
for(i in 1:200){
     partition=trucks2partition[trucks2partition$t>=pointer & trucks2partition$t<(pointer+partitionSize),]
     pointer=pointer+partitionSize
     if(pointer>end){
             break;
     }
     fname=paste('partitions/',i,'.txt',sep='')
     write.csv(file=fname,partition,row.names=FALSE,quote=FALSE)
}


################end for now###########################

oids=unique(trucksOidd[,"oidd"])
timeoids=data.frame()
for(o in oids){
        mintime=min(trucksOidd[trucksOidd$oidd==o,"time"])
        maxtime=max(trucksOidd[trucksOidd$oidd==o,"time"])
        timeseq=seq(from=mintime,to=maxtime,by=30)
        #print(data.frame(head(timeseq),o))
        timeoids=rbind(timeoids,data.frame(stringsAsFactors=FALSE,time=timeseq,oidd=o))
}



#trucksFiltered1=data.frame(time=dt,oid=trucks$oid,x=trucks$x,y=trucks$y)
#sort the data according to time
trucksSorted=trucksFiltered[order(trucksFiltered$oid,trucksFiltered$time),]

#***********************************gantt chart of convoy output**********************************
library(ggplot2)
convoys=read.csv(file="convoysOutput.txt",header=TRUE,sep=",",stringsAsFactors=F);
convoysplus=data.frame(convoys,startHour=hour(min+(convoys$start*30)),endHour=hour(min+(convoys$end*30)))

ch1<-ggplot(data=convoysplus,aes(color=id)) +
        geom_segment(aes(x=startHour, xend=endHour, y=id, yend=id), size=3) +
        xlab("Duration")
ch2<-ch1 + theme(panel.grid.minor = element_line(colour="white", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 24, 1), breaks = seq(0, 24, 4))
ch2

ch1<-ggplot(data=convoysplus,aes(color=id)) +
        geom_segment(aes(x=startHour, xend=endHour, y=id, yend=id), size=3) +
        xlab("Duration")
ch2<-ch1 + theme(panel.grid.minor = element_line(colour="white", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 24, 1), breaks = seq(0, 24, 4))
ch2
