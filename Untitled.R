data=read.csv("repdata-data-StormData.csv")
tail(data)
str(data)
attach(data)
library(lubridate)

result=tapply(FATALITIES,EVTYPE,sum)
result2=tapply(INJURIES,EVTYPE,sum)
result3=tapply(PROPDMG,EVTYPE,sum)
result4=tapply(CROPDMG,EVTYPE,sum)

data$start=mdy_hms(data$BGN_DATE)

result5=xtabs(~EVTYPE+month(data$start),data=data)
head(result5)
class(result5)
result5=addmargins(result5)
tail(result5)
str(result5)

result6=data.frame(result5)
str(result6)
head(result6)
result6=result5[order(result5$sum,decreasing=TRUE),]
head(result6)
start=mdy_hms(data[,"BGN_DATE"])
year(start)
month(start)

date=as.POSIXct(as.character(data[1,"BGN_DATE"]))


data1=data.frame(FATALITIES=result,INJURIES=result2,
                 PROPDMG=result3,CROPDMG=result4)
head(data1)
str(data1)
colnames(data1)
data1=data1[order(data1$FATALITIES,data1$INJURIES,decreasing=TRUE),]
head(data1)
data2=data1[order(data1$PROPDMG,data1$CROPDMG,decreasing=TRUE),]
head(data2)
data2$DMG=data2$PROPDMG+data2$CROPDMG
data3=data2[order(data2$DMG,decreasing=TRUE),]
head(data3)

plot(FATALITIES+INJURIES,data=head(data1))

health=head(data1,10)
health    
class(health)
str(health)
colnames(health)
rownames(health)
health$event=factor(rownames(health),levels=rownames(health))
qplot(health$event,health$FATALITIES,geom="bar",stat="identity")
ggplot(health,aes(x=event,y=FATALITIES)) +geom_bar(stat="identity")
plot(FATALITIES+INJURIES,data=health)
barplot(health$FATALITIES,col="red")    
barplot(health$INJURIES)    

barplot(as.matrix(t(health[,1:2])),beside=TRUE)
library(ggplot2)
library(lattice)
library(reshape)
ggplot(health,aes(x=event,y=FATALITIES)) +geom_bar(stat="identity")
ggplot(health,aes(x=event,y=INJURIES)) +geom_bar(stat="identity")
barchart(FATALITIES~event,data=health,scales=list(x=list(rot=45)))
barchart(INJURIES~event,data=health,scales=list(x=list(rot=45)))

barchart(FATALITIES~event,data=health,scales=list(x=list(rot=45)))
barchart(INJURIES~event,data=health,scales=list(x=list(rot=45)))
health
meltedhealth=melt(health,id="event")
meltedhealth
barchart(value~event|variable,data=meltedhealth[1:20,],ylab="",
         scales=list(relation="free",x=list(rot=45)))

barchart(value~event|variable,data=meltedhealth[1:20,],ylab="",layout=c(1,2),
         scales=list(relation="free",x=list(rot=45)))

barchart(value/1000~event|variable,data=meltedhealth[21:40,],ylab="",
         scales=list(x=list(rot=45)))