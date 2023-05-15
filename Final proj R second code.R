library(ggplot2)
library(ggmap)
library(maps)
library(data.table)

GlobalLandTemperaturesByCity <- fread("C:/Users/shahr/OneDrive/Desktop/phd/spring2023/data viz/final proj/archive (3)/GlobalLandTemperaturesByCity.csv")

#Create some useful data points
GlobalLandTemperaturesByCity$dt<-as.Date(GlobalLandTemperaturesByCity$dt,"%Y-%m-%d")
GlobalLandTemperaturesByCity$lat<-as.numeric(gsub("N|E|S|W", "",GlobalLandTemperaturesByCity$Latitude))*ifelse(grepl("S",GlobalLandTemperaturesByCity$Latitude),-1,1)
GlobalLandTemperaturesByCity$long<-as.numeric(gsub("N|E|S|W", "", GlobalLandTemperaturesByCity$Longitude))*ifelse(grepl("W",GlobalLandTemperaturesByCity$Longitude),-1,1)
GlobalLandTemperaturesByCity$Month<-as.numeric(format(GlobalLandTemperaturesByCity$dt,"%m"))
GlobalLandTemperaturesByCity$Year<-as.numeric(format(GlobalLandTemperaturesByCity$dt,"%Y"))

setkey(GlobalLandTemperaturesByCity,long,lat,Month,Year)


meta.city<-unique(GlobalLandTemperaturesByCity[,c(4,8:10),with=FALSE],by=c("Month","long","lat","City"))
setkey(meta.city,long,lat,Month)

meta.city.length<-length(meta.city$City)
meta.city$intercept.coef<-numeric(meta.city.length)
meta.city$year.coef<-numeric(meta.city.length)


dt <- as.data.table(na.omit(subset(GlobalLandTemperaturesByCity,Year>1880)))

#This loop will fill in the columns of the meta.city table.
for(i in 1:meta.city.length){
  dt.subset<-dt[list(meta.city$long[i],meta.city$lat[i],meta.city$Month[i]),]
  lmfit<-with(dt.subset,lm.fit(x=cbind(1,Year),y=AverageTemperature))
  meta.city$intercept.coef[i]<-lmfit$coefficients[1]
  meta.city$year.coef[i]<-lmfit$coefficients[2]
}

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==1),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - Winter")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")



ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==5),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - Spring")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")


ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==8),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - Summer")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==11),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - Fall")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
