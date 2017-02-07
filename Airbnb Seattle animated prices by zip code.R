rm(list=ls())
library(dplyr)
library("geojsonio")
library( RCurl )
library(rgdal)
library(ggplot2)
library(rgeos)
library(maptools)
# download data from Kaggle 
# https://www.kaggle.com/airbnb/seattle
calendar<-read.csv("calendar.csv")
listings<-read.csv("listings.csv")
reviews<-read.csv("reviews.csv")
# download geojson file from
# https://ida-johnsson-hwgt.squarespace.com/s/seattle_zip.geojson

seattle<-geojson_read("~/Dropbox/ML/Airbnb Seattle/seattle_zip.geojson",what='sp')

points<-fortify(seattle,region = 'GEOID10')

calendar$id<-calendar$listing_id
listing.loc<-subset(listings,select=c("id","zipcode","latitude","longitude"))
merged<-merge(calendar, listing.loc,by="id")

merged$date<-as.Date(merged$date,format="%Y-%m-%d")
merged$month<-format(merged$date,"%Y-%m")
merged$price<-as.character(merged$price)
merged$p<-as.numeric(gsub("\\$","",merged$price))

m0<- merged %>% filter(!is.na(p), !is.na(zipcode))

# calculate average price by month and zip code
m1<-subset(m0,select = c("zipcode","month","p"))
m2<-aggregate(p~zipcode+month,data=m1,FUN=mean)

t<-m2[m2$month=="2016-01",]
seattle@data<-merge(seattle@data,t,by.x="GEOID10",by.y="zipcode",all=TRUE)
pal<-colorRampPalette(c("white","blue"))(100)
palette(pal)
#scale data
pr<-seattle@data$p
cols<-(pr-min(pr))/diff(range(pr))*99+1
plot(seattle,col=cols)

# using ggmap
library(animation)
library(qmap)
library(plyr)
library(ggmap)
s.map<-qmap("Seattle, Washington", zoom=11)
# plot zip code contours
s.map+geom_polygon(aes(x=long, y=lat,grou=group,alpha=0.25),data=points,fill='white')+
  geom_polygon(aes(x=long,y=lat,group=group),data=points,color='black',fill=NA)

t<-t[t$zipcode!="",]
map.df<-left_join(points,t,by=c('id'='zipcode'))
head(points)
s.map+geom_polygon(aes(x=long, y=lat,group=group,fill=p),data=map.df)+
  scale_fill_gradient(low='white',high='red')

s.map+geom_polygon(aes(x=long, y=lat,group=group,fill=p),data=map.df)+
  scale_fill_distiller(palette = 'Spectral')

#### animation

#plot map by hours 
mapfunc <- function(x) {
  t<-m2[m2$month==x,]
  t<-t[t$zipcode!="",]
  
  # plot base map
  s.map<-qmap("Seattle, Washington", zoom=11)
  
  map.df<-left_join(points,t,by=c('id'='zipcode'))
  p<- s.map+geom_polygon(aes(x=long, y=lat,group=group,fill=p),data=map.df)+
    scale_fill_gradient(low='white',high='red',name='Average price in $')+
    ggtitle(paste("Average Airbnb prices by zipcode in ",x, sep=""))
}

#get the hours of probes
months <- sort(unique (m2$month))

#generate and save the animation in HTML
saveHTML({ 
  for( i in months) print (mapfunc(i))
  ani.options(interval = 0.5)
  
}, 
img.name = "animated_plot", title = "Monthly average Airbnb prices in Seattle",
description=c("average prices by zip code")
)
