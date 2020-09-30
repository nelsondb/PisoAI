
library(rts)
library(data.table)
library(ncdf4)
library(lubridate)
library(dplyr)
library(reshape2)



fuzz=0.5 #parameter to be used with the fuzzy.extract function - the amount of shift to use (in degreees Longitude/Latitude) to look for an alternate pixel in each of the 4 cardinal directions
#directory for csv
output.directory<-output.directory.path
#filename for csv
output.filename<- output.filename.path

pred_data<-pred_data_out_path

site.data<-read.csv(site.data.path,
                    stringsAsFactors = F, row.names = 1)
site.data$Date<-as.Date(site.data$Date)


df.points<-as.data.frame(cbind(site.data$Longitude,site.data$Latitude,site.data$Site))
df.points<-unique(df.points)
df.points<-na.omit(df.points)
colnames(df.points)<-c("Longitude","Latitude","Site")
df.points$Longitude<-as.numeric(as.character(df.points$Longitude))
df.points$Latitude<-as.numeric(as.character(df.points$Latitude))
df.points$Site<-as.character(df.points$Site)
ex.points<-SpatialPoints(df.points[,1:2])


###BE CARFEUL TO ENSRURE THAT THE ENVIRONMENT WAS EMPTY WHEN STARTING THE SCRIPT TO READ IN SPATIAL DATA###
vars.class<-NA
vars.name<-ls()
vars.class<-eapply(.GlobalEnv,class)
vars<-as.data.frame(cbind(names(vars.class),as.character(vars.class[])))
colnames(vars)<-c("object","type")

CRU.rasters<-vars[vars$object%like%"CRU"&vars$type=="RasterLayer",]
CRU.rasters<-CRU.rasters[CRU.rasters$object!="CRU.DEM.nc",]
CRU.rts<-vars[vars$object%like%"CRU"&vars$type=="RasterBrickTS",]
EOBS.rts<-vars[vars$object%like%"EOBS"&vars$type=="RasterBrickTS",]
NCEP.rts<-vars[vars$object%like%"NCEP"&vars$type=="RasterBrickTS",]


i<-NA
for (i in 1:length(CRU.rasters$object)) {
  x<-get(as.character(CRU.rasters$object[i]))
  x<-raster::extract(x,ex.points,method="bilinear")%>%cbind(df.points)
  colnames(x)[1]<-as.character(CRU.rasters$object[i])
  site.data<-merge(site.data,x, all.x=T)
}



site.data.CRUTS<-site.data
i<-NA
for (i in 1:length(CRU.rts$object)) {
  x<-get(as.character(CRU.rts$object[i]))
  x<-fuzzy.extract(x,ex.points,df.points,fuzz=fuzz)%>%fuzzy.extract.mean()  
  colnames(x)[3]<-as.character(CRU.rts$object[i])
  site.data.CRUTS<-merge(site.data.CRUTS,x[,1:3], all.x=T)
}


site.data.EOBSTS<-site.data.CRUTS
i<-NA
for (i in 1:length(EOBS.rts$object)) {
  x<-get(as.character(EOBS.rts$object[i]))
  x<-fuzzy.extract(x,ex.points,df.points,fuzz=fuzz)%>%fuzzy.extract.mean()  
  colnames(x)[3]<-as.character(EOBS.rts$object[i])
  site.data.EOBSTS<-merge(site.data.EOBSTS,x[,1:3], all.x=T)
}


site.data.NCEPTS<-site.data.EOBSTS
i<-NA
for (i in 1:length(NCEP.rts$object)) {
  x<-get(as.character(NCEP.rts$object[i]))
  x<-simple.extract(x,ex.points,df.points)
  colnames(x)[3]<-as.character(NCEP.rts$object[i])
  site.data.NCEPTS<-merge(site.data.NCEPTS,x, all.x=T)
}


site.data.TELE<-site.data.NCEPTS
site.data.TELE<-merge(site.data.TELE,TELE,all.x=T)


write.csv(site.data.TELE, file=paste0(output.directory,output.filename,".csv"))

