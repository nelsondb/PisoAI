

library(rts)
library(ncdf4)
library(lubridate)
library(dplyr)

#This file transforms all data from the original format into the used raster time series (rts) format 
#It also crops the spatial extent to Europe


#Global terms
pred_src_data<-pred_src_data_path
pred_data_out<-pred_data_out_path
e<-extent(-30,60,20,80) #set an initial data extent to reduce file size

#Primary variables
#CRU
CRUfiles<-list.files(paste0(pred_src_data,"CRU"),full.names=TRUE)
CRUnames<-list.files(paste0(pred_src_data,"CRU"),full.names=FALSE)
CRUnames<-paste0("CRU",substr(CRUnames,21,24))

i<-NA
for (i in 1:length(CRUfiles)) {
  x<-nc_open(CRUfiles[[i]])
  y<-raster::brick(CRUfiles[[i]])
  y.t<-seq(
    as.Date(paste(lubridate::year(min(y@z$Date)),lubridate::month(min(y@z$Date)),15,sep="-")),
    as.Date(paste(lubridate::year(max(y@z$Date)),lubridate::month(max(y@z$Date)),15,sep="-")),
    by="month")
  y<-crop(y,e)
  y<-rts(y,y.t)
  write.rts(y,filename = paste0(pred_data_out,"CRU/",CRUnames[i],".rts"),overwrite=TRUE)
  nc_close(x)
}


#EOBS
#convert from daily to monthly data using CDO in a bash shell. https://code.mpimet.mpg.de/projects/cdo
#this assumes that CDO is already installed on the system. 
#This requires a mac user to install macports, xcode, and then cdo in that order, outside of R
#there is no need to crop these data because they are already Europe-only. 
#Due to the nature of the conversion, it's easier to convert from netcdf to rts when the data are read in below
setwd(pred_data_out) #this is required here because system commands will not accept spaces in filepath names 
EOBSfiles<-list.files(paste0(pred_src_data,"EOBS"),full.names=TRUE)
EOBSnames<-list.files(paste0(pred_src_data,"EOBS"),full.names=FALSE)
EOBSnames<-paste0("EOBS.",toupper(substr(EOBSnames,1,2)),".mo.nc")
i<-NA
for (i in 1:length(EOBSfiles)) {
  system(paste0("cdo monmean ",EOBSfiles[i], " EOBS/",EOBSnames[i]))
}

#NCEP
NCEPfiles<-list.files(paste0(pred_src_data,"NCEP/"),full.names=TRUE)
NCEPnames<-list.files(paste0(pred_src_data,"NCEP/"),full.names=FALSE)
NCEP.level.n<-c(1,3,6)
NCEP.level.names<-c("1000mb","850mb","500mb")
start.time <- Sys.time()
i<-NA
j<-NA
for (i in 1:length(NCEPfiles)) {
  x<-nc_open(NCEPfiles[[i]])
  if (length(x$dim)==3) {
    y<-raster::rotate(brick(NCEPfiles[[i]]))
    crs(y)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    y.t<-seq(
      as.Date(paste(lubridate::year(min(y@z$`Date/time`)),lubridate::month(min(y@z$`Date/time`)),15,sep="-")),
      as.Date(paste(lubridate::year(max(y@z$`Date/time`)),lubridate::month(max(y@z$`Date/time`)),15,sep="-")), 
      by="month")
    y<-crop(y,e)
    y<-stack(y)
    y<-rts(y,y.t)
    write.rts(y,filename = paste0(pred_data_out,"NCEP/NCEP.",NCEPnames[i],".rts"),
              overwrite=TRUE)
  } else {
    for (j in 1:length(NCEP.level.n)) {
      y<-raster::rotate(brick(NCEPfiles[[i]],level=NCEP.level.n[j]))
      crs(y)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      y.t<-seq(
        as.Date(paste(lubridate::year(min(y@z$`Date/time`)),lubridate::month(min(y@z$`Date/time`)),15,sep="-")),
        as.Date(paste(lubridate::year(max(y@z$`Date/time`)),lubridate::month(max(y@z$`Date/time`)),15,sep="-")), 
        by="month")
      y<-crop(y,e)
      y<-stack(y)
      y<-rts(y,y.t)  
      write.rts(y,filename = paste0(pred_data_out,"NCEP/NCEP.",NCEPnames[i],NCEP.level.names[j],".rts"),
                overwrite=TRUE)  
    }
  }
  nc_close(x)
}


#DEM
#this DEM is a locally produced mosaic of 4 required GTOPO30 DEM panels
#https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-global-30-arc-second-elevation-gtopo30?qt-science_center_objects=0#qt-science_center_objects
DEM<-raster(paste0(pred_src_data,"EurC_DEM.tif"))
crs(DEM)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
DEM<-crop(DEM, extent(e))
DEM[is.na(DEM)]<-0
writeRaster(DEM,filename = paste0(pred_data_out,"EurC_DEM.nc"),
            overwrite=TRUE,varname = "Elevation",varunit = "masl")


#climate indices
TELE<-read.csv(paste0(pred_src_data,"TELE/TELE.csv"))
TELE$day<-15
TELE$Date<-as.Date(paste(TELE$year,TELE$month,TELE$day,sep="-"))
i<-dplyr::select(TELE,c(year,month,Date,NAO,EA,WP,PNA,EA.WR,SCA,POL))
write.csv(i, file=paste0(pred_data_out,"TELE.csv"))


#Derived variables
#CRU-derived
CRU.tmp<-rts::read.rts(paste0(pred_data_out,"CRU/CRU.tmp.rts"))
x<-CRU.tmp
CRU.t<-index(x)
CRU.DEM<-projectRaster(DEM,x@raster,res=0.5)
CRU.DEM[is.na(CRU.DEM)]<-0
writeRaster(CRU.DEM,filename = paste0(pred_data_out,"CRU/CRU.DEM.nc"),
            overwrite=TRUE,varname = "Elevation",varunit = "masl")

CRU.atp.e<-1013.25*exp(-1*(CRU.DEM/7990))
writeRaster(CRU.atp.e,filename = paste0(pred_data_out,"CRU/CRU.atp.e.nc"),
            overwrite=TRUE,varname = "generic.atmospheric.pressure",varunit = "hPa")

CRU.tmp<-rts::read.rts(paste0(pred_data_out,"CRU/CRU.tmp.rts"))
CRU.svp <- (1.0007+3.46*CRU.atp.e/1000000)*(6.1121*exp(17.502*CRU.tmp@raster/(240.97+CRU.tmp@raster)))
CRU.svp <-rts(CRU.svp,CRU.t)
write.rts(CRU.svp,filename = paste0(pred_data_out,"CRU/CRU.svp.rts"),overwrite=TRUE)

CRU.vap<-rts::read.rts(paste0(pred_data_out,"CRU/CRU.vap.rts"))
CRU.vpd <- CRU.svp@raster - CRU.vap@raster
CRU.vpd <-rts(CRU.vpd,CRU.t)
write.rts(CRU.vpd,filename = paste0(pred_data_out,"CRU/CRU.vpd.rts"),overwrite=TRUE)

CRU.pre<-rts::read.rts(paste0(pred_data_out,"CRU/CRU.pre.rts"))
CRU.pet<-rts::read.rts(paste0(pred_data_out,"CRU/CRU.pet.rts"))
CRU.cwb <- CRU.pre@raster - CRU.pet@raster
CRU.cwb <-rts(CRU.cwb,CRU.t)
write.rts(CRU.cwb,filename = paste0(pred_data_out,"CRU/CRU.cwb.rts"),overwrite=TRUE)

CRU.recordhigh<- calc(CRU.tmp@raster, function(x) max(x, na.rm = TRUE))
CRU.recordlow<- calc(CRU.tmp@raster, function(x) min(x, na.rm = TRUE))
CRU.trng<-CRU.recordhigh-CRU.recordlow

writeRaster(CRU.trng, filename= paste0(pred_data_out,"CRU/CRU.trng.nc"),overwrite=TRUE, 
            format="CDF",varname="temperature.range", varunit="degrees", 
            xname="lon", yname="lat",zname="trng",zunit="degrees")
CRU.trng<-raster(paste0(pred_data_out,"CRU/CRU.trng.nc"))

i<-rasterToPoints(CRU.trng)
CRU.lat<-as.data.frame(i[,1:2])
colnames(CRU.lat)<-c("Longitude","Latitude")
CRU.lat$lat<-CRU.lat$Latitude
CRU.lat<-rasterFromXYZ(CRU.lat)
crs(CRU.lat)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
CRU.lat<-projectRaster(CRU.lat,CRU.trng)

CRU.cont<-((1.7*CRU.trng)/(sin((abs(CRU.lat)+10)*pi/180)))-14
writeRaster(CRU.cont, filename=paste0(pred_data_out,"CRU/CRU.cont.nc"),overwrite=TRUE, 
            format="CDF",varname="continentality", varunit="percent", 
            xname="lon", yname="lat",zname="cont",zunit="percent")
CRU.cont<-raster(paste0(pred_data_out,"CRU/CRU.cont.nc"))


#EOBS-derived
EOBS.PP.mo<-raster::brick(paste0(pred_data_out,"EOBS/EOBS.PP.mo.nc"))
EOBS.t<-seq(
  as.Date(paste(lubridate::year(min(EOBS.PP.mo@z$Date)),lubridate::month(min(EOBS.PP.mo@z$Date)),15,sep="-")),
  as.Date(paste(lubridate::year(max(EOBS.PP.mo@z$Date)),lubridate::month(max(EOBS.PP.mo@z$Date)),15,sep="-")), 
  by="month")

EOBS.DEM<-projectRaster(DEM,EOBS.PP.mo[[1]],res=0.1)
EOBS.DEM[is.na(EOBS.DEM)]<-0
writeRaster(EOBS.DEM,filename = paste0(pred_data_out,"EOBS/EOBS.DEM.nc"),
            overwrite=TRUE,varname = "Elevation",varunit = "masl")

EOBS.atp.e<-EOBS.PP.mo*exp(-1*(EOBS.DEM/7990))
EOBS.atp.e<-rts(EOBS.atp.e,EOBS.t)
write.rts(EOBS.atp.e,filename = paste0(pred_data_out,"EOBS/EOBS.atp.e.rts"),overwrite=TRUE)

EOBS.TG.mo<-raster::brick(paste0(pred_data_out,"EOBS/EOBS.TG.mo.nc"))
EOBS.svp <- (1.0007+3.46*EOBS.atp.e@raster/1000000)*(6.1121*exp(17.502*EOBS.TG.mo/
                                                                (240.97+EOBS.TG.mo)))
EOBS.svp<-rts(EOBS.svp,EOBS.t)
write.rts(EOBS.svp,filename = paste0(pred_data_out,"EOBS/EOBS.svp.rts"),overwrite=TRUE)




