
library(rts)
library(data.table)
library(ncdf4)
library(lubridate)

#This file reads in the cropped rts files
#This method of extracting point data from rasters is slower than the method used in the Piso.AI application, 
#but it allows for bilinear smoothing of the extracted data


pred_data<-pred_data_out_path

DEM <- raster(paste0(pred_data,"EurC_DEM.nc"))

CRUfiles<-list.files(paste0(pred_data,"CRU"),full.names=TRUE)
CRUnames<-list.files(paste0(pred_data,"CRU"),full.names=FALSE)
i<-NA
for (i in 1:length(CRUfiles)) {
  if(CRUnames[i]%like%".nc"==TRUE) {
    assign(CRUnames[i],raster::raster(CRUfiles[i]))
  } else {assign(CRUnames[i],rts::read.rts(CRUfiles[i]))}
}

EOBSfiles<-list.files(paste0(pred_data,"EOBS"),full.names=TRUE)
EOBSnames<-list.files(paste0(pred_data,"EOBS"),full.names=FALSE)
i<-NA
for (i in 1:length(EOBSfiles)) {
  if(EOBSnames[i]%like%".nc"==TRUE) {
    x<-ncdf4::nc_open(EOBSfiles[i])
    if (length(x$dim)==2) {assign(EOBSnames[i],raster::raster(EOBSfiles[i]))}
    else {
      y<-raster::brick(EOBSfiles[i])
      y.t<-seq(
        as.Date(paste(lubridate::year(min(y@z$Date)),lubridate::month(min(y@z$Date)),15,sep="-")),
        as.Date(paste(lubridate::year(max(y@z$Date)),lubridate::month(max(y@z$Date)),15,sep="-")), 
        by="month")
      assign(EOBSnames[i],rts::rts(y,y.t))
      }
  } else {assign(EOBSnames[i],rts::read.rts(EOBSfiles[i]))}
}
y<-NULL
nc_close(x)

NCEPfiles<-list.files(paste0(pred_data,"NCEP"),full.names=TRUE)
NCEPnames<-list.files(paste0(pred_data,"NCEP"),full.names=FALSE)
i<-NA
for (i in 1:length(NCEPfiles)) {assign(NCEPnames[i],rts::read.rts(NCEPfiles[i]))}

TELE<-read.csv(paste0(pred_data,"TELE.csv"),row.names=1)

koep.clim <- raster(paste0(pred_data,"koepclim/koepclim.grd"))
koep.cat<-as.data.frame(read.csv(paste0(pred_data,"koepclim/koep.cat.csv")))
