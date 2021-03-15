library(rts)
library(ff)
library(ncdf4)
library(caret)
library(dplyr)
library(data.table)
library(reshape2)
library(abind)


#transform required input predictors to ff objects for use with Piso.AI application

setwd("INSERT FILE PATH HERE") #this should be the directory for the application

###this script assumes that script "2- read in spatial data.R" has already been run

model.folder<-"INSERT FILE PATH HERE"


###this script assumes that script "2-read_in_spatial_data.R" has already been run


xgb.vars<-read.csv("PisoAI_master_data/data/modelRobjects/v1.01/xgb.model.inputs.2.csv")

#DEM layer to ff object 
DEM <- raster(paste0(pred_data,"EurC_DEM.nc"))
x<-DEM
e<-extent(-30,60,20,80)
x<-crop(x,e)

#set coordinate reference system
crs(x)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
DEM.ID_Raster <- raster(x[[1]])
DEM.ID_Raster[] <- 1:ncell(x[[1]])
writeRaster(DEM.ID_Raster,"data/predictors/FFmats/DEM.GTOPO30.ID_Raster",overwrite=T)


DEM <- ff(vmode="double",dim=c(ncell(x),nlayers(x)),
          file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/DEM_GTOPO30"))
DEM[,1] <- x[]


ffsave(DEM,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/DEM_GTOPO30"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))


model.area.raster <- raster(paste0(pred_data,"model.domain.gri"))
crs(model.area.raster)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
writeRaster(model.area.raster,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/model.area.raster"),overwrite=T)

####CRU####
CRU.var<-xgb.vars[xgb.vars%like%"CRU"]

#convert CRU layers to ff objects 
x<-get(CRU.var[1]) #pick any from the representative group

CRU.ID_Raster <- raster(x@raster[[1]])
CRU.ID_Raster[] <- 1:ncell(x@raster[[1]])
writeRaster(CRU.ID_Raster,paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/CRU.ID_Raster"),overwrite=T)


CRU.x.y<-rasterToPoints(CRU.ID_Raster)[,1:2]
colnames(CRU.x.y)<-c("Longitude","Latitude")
CRU.coords<-paste(CRU.x.y[,1],CRU.x.y[,2],sep="_")
CRU.t<-x@time
CRU.t<-seq(time(CRU.t[1]),time(CRU.t[length(CRU.t)]), by="month")
CRU.var
i<-NA
z<-NA
for(i in 1:length(CRU.var)) {
  y<-get(as.character(CRU.var[i]))
  if (class(y)=="RasterLayer") {
    y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))%>%as.data.frame()
    y<-y[,3]%>%as.data.frame()
    y[,1:length(CRU.t)]<-y
    y<-as.matrix(y) 
  } else {
    y<-subset(y,CRU.t)
    y<-y@raster
    y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))
    y<-y[,3:ncol(y)]
  }
  if (is.na(z)) {
    z<-y
  } else {
    z<-abind::abind(z,y,along=3)
  }
} 
dimnames(z)<-list(CRU.coords,CRU.t,CRU.var)
CRU.mat<-z


CRU.mat<-as.ff(CRU.mat,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/CRU.mat.FF"),overwrite = T)
ffsave(list="CRU.mat",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/CRU.mat.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))

saveRDS(CRU.t,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/CRU.t"))
saveRDS(CRU.x.y,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/CRU.x.y"))



#EOBS
EOBS.var<-xgb.vars[xgb.vars%like%"EOBS"]

#convert EOBS layers to ff objects (can be done once in separate script)
x<-get(EOBS.var[1]) #pick any from the representative group

EOBS.ID_Raster <- raster(x@raster[[1]])
EOBS.ID_Raster[] <- 1:ncell(x@raster[[1]])
writeRaster(EOBS.ID_Raster,"data/predictors/FFmats/EOBS.ID_Raster",overwrite=T)


EOBS.x.y<-rasterToPoints(EOBS.ID_Raster)[,1:2]
colnames(EOBS.x.y)<-c("Longitude","Latitude")
EOBS.coords<-paste(EOBS.x.y[,1],EOBS.x.y[,2],sep="_")

EOBS.t<-x@time
EOBS.t<-seq(time(EOBS.t[1]),time(EOBS.t[length(EOBS.t)]), by="month")



#EOBS data has to be converted in sections due to RAM limitations 

z<-get(as.character(EOBS.var[1]))
z<-subset(z,EOBS.t)
z<-z@raster
z<-cbind(xyFromCell(z, 1:ncell(z)), values(z))
z<-z[,3:ncol(z)]
z<-as.matrix(z)  

y<-get(as.character(EOBS.var[2]))
y<-subset(y,EOBS.t)
y<-y@raster
y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))
y<-y[,3:ncol(y)]

z<-abind(z,y,along=3)

dimnames(z)<-list(EOBS.coords,EOBS.t,EOBS.var[1:2])
EOBS.mat.A<-z
EOBS.mat.A<-as.ff(EOBS.mat.A,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/EOBS.mat.A.FF"),overwrite = T)
ffsave(list="EOBS.mat.A",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/EOBS.mat.A.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))



z<-get(as.character(EOBS.var[3]))
z<-subset(z,EOBS.t)
z<-z@raster
z<-cbind(xyFromCell(z, 1:ncell(z)), values(z))
z<-z[,3:ncol(z)]

y<-get(as.character(EOBS.var[4]))
y<-subset(y,EOBS.t)
y<-y@raster
y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))
y<-y[,3:ncol(y)]
y<-as.matrix(y)  

z<-abind(z,y,along=3)

dimnames(z)<-list(EOBS.coords,EOBS.t,EOBS.var[3:4])
EOBS.mat.B<-z
EOBS.mat.B<-as.ff(EOBS.mat.B,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/EOBS.mat.B.FF"),overwrite = T)
ffsave(list="EOBS.mat.B",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/EOBS.mat.B.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))


z<-get(as.character(EOBS.var[5]))
z<-subset(z,EOBS.t)
z<-z@raster
z<-cbind(xyFromCell(z, 1:ncell(z)), values(z))
z<-z[,3:ncol(z)]

y<-get(as.character(EOBS.var[6]))
y<-subset(y,EOBS.t)
y<-y@raster
y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))
y<-y[,3:ncol(y)]

z<-abind(z,y,along=3)

dimnames(z)<-list(EOBS.coords,EOBS.t,EOBS.var[5:6])
EOBS.mat.C<-z
EOBS.mat.C<-as.ff(EOBS.mat.C,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/EOBS.mat.C.FF"),overwrite = T)
ffsave(list="EOBS.mat.C",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/EOBS.mat.C.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))


z<-get(as.character(EOBS.var[7]))
z<-subset(z,EOBS.t)
z<-z@raster
z<-cbind(xyFromCell(z, 1:ncell(z)), values(z))
z<-z[,3:ncol(z)]


dimnames(z)<-list(EOBS.coords,EOBS.t)
EOBS.mat.D<-z
EOBS.mat.D<-as.ff(EOBS.mat.D,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/EOBS.mat.D.FF"),overwrite = T)
ffsave(list="EOBS.mat.D",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/EOBS.mat.D.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))

saveRDS(EOBS.t,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/EOBS.t"))
saveRDS(EOBS.x.y,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/EOBS.x.y"))



#NCEP
#NCEP data has to be converted in sections due to RAM limitations

NCEP.var<-xgb.vars[xgb.vars%like%"NCEP"]
#convert EOBS layers to ff objects (can be done once in separate script)
x<-get(NCEP.var[1]) #pick any from the representative group
i<-NA
for (i in 2:length(NCEP.var[i])) {
  y<-get(NCEP.var[i])
  if (y@raster@ncols>x@raster@ncols) {
    x<-y
  }
}
NCEP.ID_Raster <- raster(x@raster[[1]])
NCEP.ID_Raster[] <- 1:ncell(x@raster[[1]])
writeRaster(NCEP.ID_Raster,"data/predictors/FFmats/NCEP.ID_Raster",overwrite=T)


NCEP.x.y<-rasterToPoints(NCEP.ID_Raster)[,1:2]
colnames(NCEP.x.y)<-c("Longitude","Latitude")
NCEP.coords<-paste(NCEP.x.y[,1],NCEP.x.y[,2],sep="_")
# NCEP.t<-x@time
# NCEP.t<-seq(time(NCEP.t[1]),time(NCEP.t[length(NCEP.t)]), by="month")
NCEP.t<-x@time
NCEP.t<-seq(time(NCEP.t[1]),time(NCEP.t[length(NCEP.t)]), by="month")

NCEP.cut.pattern<-c(1,2,8)
NCEP.cut.size<-8

NCEP.var.cuts<-c(NCEP.cut.pattern+NCEP.cut.size*0, #matA
                 NCEP.cut.pattern+NCEP.cut.size*1, #matB
                 NCEP.cut.pattern+NCEP.cut.size*2, #matC
                 NCEP.cut.pattern+NCEP.cut.size*3, #matD
                 NCEP.cut.pattern+NCEP.cut.size*4) #matE
NCEP.var.cuts[length(NCEP.var.cuts)]<-length(NCEP.var)
length(NCEP.var.cuts)
# NCEP.var.cuts<-c(1,2,8,9,10,16,17,18,25)
for (i in 1:length(NCEP.var)) {print(class(get(NCEP.var[i])))}

z<-get(as.character(NCEP.var[NCEP.var.cuts[1]]))
z<-subset(z,NCEP.t)
z<-z@raster
z<-projectRaster(z,NCEP.ID_Raster)
z<-cbind(xyFromCell(z, 1:ncell(z)), values(z))
z<-z[,3:ncol(z)]
z<-as.matrix(z)
i<-NA
for(i in NCEP.var.cuts[2]:NCEP.var.cuts[3]) {
  y<-get(as.character(NCEP.var[i]))
    y<-subset(y,NCEP.t)
    y<-y@raster
    y<-projectRaster(y,NCEP.ID_Raster)
    y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))
    y<-y[,3:ncol(y)]
    z<-abind(z,y,along=3)
} 
dim(z)
dim(y)
dimnames(z)<-list(NCEP.coords,NCEP.t,NCEP.var[NCEP.var.cuts[1]:NCEP.var.cuts[3]])
NCEP.mat.A<-z
NCEP.mat.A<-as.ff(NCEP.mat.A,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.A.FF"),overwrite = T)
ffsave(list="NCEP.mat.A",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.A.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))


z<-get(as.character(NCEP.var[NCEP.var.cuts[4]]))
z<-subset(z,NCEP.t)
z<-z@raster
z<-projectRaster(z,NCEP.ID_Raster)
z<-cbind(xyFromCell(z, 1:ncell(z)), values(z))
z<-z[,3:ncol(z)]
z<-as.matrix(z)
i<-NA
for(i in NCEP.var.cuts[5]:NCEP.var.cuts[6]) {
  y<-get(as.character(NCEP.var[i]))
  y<-subset(y,NCEP.t)
  y<-y@raster
  y<-projectRaster(y,NCEP.ID_Raster)
  y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))
  y<-y[,3:ncol(y)]
  z<-abind(z,y,along=3)
} 
dimnames(z)<-list(NCEP.coords,NCEP.t,NCEP.var[NCEP.var.cuts[4]:NCEP.var.cuts[6]])
NCEP.mat.B<-z
NCEP.mat.B<-as.ff(NCEP.mat.B,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.B.FF"),overwrite = T)
ffsave(list="NCEP.mat.B",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.B.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))


z<-get(as.character(NCEP.var[NCEP.var.cuts[7]]))
z<-subset(z,NCEP.t)
z<-z@raster
z<-projectRaster(z,NCEP.ID_Raster)
z<-cbind(xyFromCell(z, 1:ncell(z)), values(z))
z<-z[,3:ncol(z)]
z<-as.matrix(z)
i<-NA
for(i in NCEP.var.cuts[8]:NCEP.var.cuts[9]) {
  y<-get(as.character(NCEP.var[i]))
  y<-subset(y,NCEP.t)
  y<-y@raster
  y<-projectRaster(y,NCEP.ID_Raster)
  y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))
  y<-y[,3:ncol(y)]
  z<-abind(z,y,along=3)
} 
dimnames(z)<-list(NCEP.coords,NCEP.t,NCEP.var[NCEP.var.cuts[7]:NCEP.var.cuts[9]])
NCEP.mat.C<-z
NCEP.mat.C<-as.ff(NCEP.mat.C,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.C.FF"),overwrite = T)
ffsave(list="NCEP.mat.C",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.C.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))


z<-get(as.character(NCEP.var[NCEP.var.cuts[10]]))
z<-subset(z,NCEP.t)
z<-z@raster
z<-projectRaster(z,NCEP.ID_Raster)
z<-cbind(xyFromCell(z, 1:ncell(z)), values(z))
z<-z[,3:ncol(z)]
z<-as.matrix(z)
i<-NA
for(i in NCEP.var.cuts[11]:NCEP.var.cuts[12]) {
  y<-get(as.character(NCEP.var[i]))
  y<-subset(y,NCEP.t)
  y<-y@raster
  y<-projectRaster(y,NCEP.ID_Raster)
  y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))
  y<-y[,3:ncol(y)]
  z<-abind(z,y,along=3)
} 
dimnames(z)<-list(NCEP.coords,NCEP.t,NCEP.var[NCEP.var.cuts[10]:NCEP.var.cuts[12]])
NCEP.mat.D<-z
NCEP.mat.D<-as.ff(NCEP.mat.D,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.D.FF"),overwrite = T)
ffsave(list="NCEP.mat.D",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.D.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))


z<-get(as.character(NCEP.var[NCEP.var.cuts[13]]))
z<-subset(z,NCEP.t)
z<-z@raster
z<-projectRaster(z,NCEP.ID_Raster)
z<-cbind(xyFromCell(z, 1:ncell(z)), values(z))
z<-z[,3:ncol(z)]
z<-as.matrix(z)
i<-NA
for(i in NCEP.var.cuts[14]:NCEP.var.cuts[15]) {
  y<-get(as.character(NCEP.var[i]))
  y<-subset(y,NCEP.t)
  y<-y@raster
  y<-projectRaster(y,NCEP.ID_Raster)
  y<-cbind(xyFromCell(y, 1:ncell(y)), values(y))
  y<-y[,3:ncol(y)]
  z<-abind(z,y,along=3)
} 
dimnames(z)<-list(NCEP.coords,NCEP.t,NCEP.var[NCEP.var.cuts[13]:NCEP.var.cuts[15]])
NCEP.mat.E<-z
NCEP.mat.E<-as.ff(NCEP.mat.E,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.E.FF"),overwrite = T)
ffsave(list="NCEP.mat.E",
       file=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats/NCEP.mat.E.FF"),
       rootpath=paste0(getwd(),"/PisoAI_master_data/data/predictors/FFmats"))









saveRDS(NCEP.t,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/NCEP.t"))
saveRDS(NCEP.x.y,file=paste0(getwd(),"/PisoAI_master_data/data/predictors/NCEP.x.y"))


TELE<-read.csv(paste0(pred_data,"TELE.csv"),row.names=1)
write.csv(TELE, file=paste0(getwd(),"/PisoAI_master_data/data/predictors/TELE.csv"))




