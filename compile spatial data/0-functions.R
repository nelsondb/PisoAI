
library(raster)
library(rts)
library(dplyr)

#This script defines various functions that are called to extract data from the input predictor variable datasets

simple.extract<-function(x, #a rts object to extract
                         ex.points, #a SpatialPoints object
                         df.points) { #a df with columns "Longitude","Latitude", and "Site" - 1st 2 are numeric and 3rd is character) 
  time.pointers<-data.frame(x@time)
  time.pointers<-as.Date(rownames(time.pointers))
  i<-raster::extract(x,ex.points,time.pointers) %>% as.data.frame()
  colnames(i)<-df.points[,3]
  i$Date<-rownames(i)
  i<-reshape2::melt(i,id.vars = "Date",measure.vars = colnames(i[,-ncol(i)]))
  colnames(i)<-c(colnames(i)[1],colnames(df.points)[3],paste(deparse(substitute(x))))
  i[,1]<-as.Date(i[,1])
  i[,2]<-as.character(i[,2])
  return(i) 
}

simple.extract.single<-function(x, #a rts object to extract
                                ex.points, #a SpatialPoints object
                                df.points) { #a df with columns "Longitude","Latitude", and "Site" - 1st 2 are numeric and 3rd is character) 
  time.pointers<-data.frame(x@time)
  time.pointers<-as.Date(rownames(time.pointers))
  i<-raster::extract(x,ex.points,time.pointers) %>% as.data.frame()
  colnames(i)<-df.points[,3]
  i$Date<-rownames(i)
  i<-reshape2::melt(i,id.vars = "Date",measure.vars = colnames(i[-ncol(i)]))
  colnames(i)<-c(colnames(i)[1],colnames(df.points)[3],paste(deparse(substitute(x))))
  i[,1]<-as.Date(i[,1])
  i[,2]<-as.character(i[,2])
  return(i) 
}


fuzzy.extract<-function(x, #a rts object to extract
                        ex.points, #a SpatialPoints object
                        df.points, #a df with columns "Longitude","Latitude", and "Site" - 1st 2 are numeric and 3rd is character
                        fuzz=0.5 #the amount of shift to use (in degreees Longitude/Latitude) to look for an alternate pixel in each of the 4 cardinal directions
) { 
  time.pointers<-data.frame(x@time)
  time.pointers<-as.Date(rownames(time.pointers))
  i<-raster::extract(x,ex.points,time.pointers)
  i<-as.data.frame(i)
  colnames(i)<-df.points[,3]
  i$Date<-rownames(i)
  i<-reshape2::melt(i,id.vars = "Date",measure.vars = colnames(i[,-ncol(i)]))
  colnames(i)<-c(colnames(i)[1],colnames(df.points)[3],paste(deparse(substitute(x))))
  i[,1]<-as.Date(i[,1])
  i[,2]<-as.character(i[,2])
  
  df.points.Ltp1<-df.points
  df.points.Ltp1$Latitude<-df.points.Ltp1$Latitude+fuzz
  pts.Ltp1<-SpatialPoints(df.points.Ltp1[,1:2])
  i.Ltp1<-raster::extract(x,pts.Ltp1,time.pointers)
  i.Ltp1<-as.data.frame(i.Ltp1)
  colnames(i.Ltp1)<-df.points[,3]
  i.Ltp1$Date<-rownames(i.Ltp1)
  i.Ltp1<-reshape2::melt(i.Ltp1,id.vars = "Date",measure.vars = colnames(i.Ltp1[,-ncol(i.Ltp1)]))
  colnames(i.Ltp1)<-c(colnames(i.Ltp1)[1],colnames(df.points)[3],paste(deparse(substitute(x)),"Ltp1",sep="."))
  i.Ltp1[,1]<-as.Date(i.Ltp1[,1])
  i.Ltp1[,2]<-as.character(i.Ltp1[,2])
  
  j<-merge(i,i.Ltp1,all=T)
  
  df.points.Ltm1<-df.points
  df.points.Ltm1$Latitude<-df.points.Ltm1$Latitude-fuzz
  pts.Ltm1<-SpatialPoints(df.points.Ltm1[,1:2])
  i.Ltm1<-raster::extract(x,pts.Ltm1,time.pointers)
  i.Ltm1<-as.data.frame(i.Ltm1)
  colnames(i.Ltm1)<-df.points[,3]
  i.Ltm1$Date<-rownames(i.Ltm1)
  i.Ltm1<-reshape2::melt(i.Ltm1,id.vars = "Date",measure.vars = colnames(i.Ltm1[,-ncol(i.Ltm1)]))
  colnames(i.Ltm1)<-c(colnames(i.Ltm1)[1],colnames(df.points)[3],paste(deparse(substitute(x)),"Ltm1",sep="."))
  i.Ltm1[,1]<-as.Date(i.Ltm1[,1])
  i.Ltm1[,2]<-as.character(i.Ltm1[,2])
  
  j<-merge(j,i.Ltm1,all=T)
  
  df.points.Lop1<-df.points
  df.points.Lop1$Longitude<-df.points.Lop1$Longitude+fuzz
  pts.Lop1<-SpatialPoints(df.points.Lop1[,1:2])
  i.Lop1<-raster::extract(x,pts.Lop1,time.pointers)
  i.Lop1<-as.data.frame(i.Lop1)
  colnames(i.Lop1)<-df.points[,3]
  i.Lop1$Date<-rownames(i.Lop1)
  i.Lop1<-reshape2::melt(i.Lop1,id.vars = "Date",measure.vars = colnames(i.Lop1[,-ncol(i.Lop1)]))
  colnames(i.Lop1)<-c(colnames(i.Lop1)[1],colnames(df.points)[3],paste(deparse(substitute(x)),"Lop1",sep="."))
  i.Lop1[,1]<-as.Date(i.Lop1[,1])
  i.Lop1[,2]<-as.character(i.Lop1[,2])
  
  j<-merge(j,i.Lop1,all=T)
  
  df.points.Lom1<-df.points
  df.points.Lom1$Longitude<-df.points.Lom1$Longitude-fuzz
  pts.Lom1<-SpatialPoints(df.points.Lom1[,1:2])
  i.Lom1<-raster::extract(x,pts.Lom1,time.pointers)
  i.Lom1<-as.data.frame(i.Lom1)
  colnames(i.Lom1)<-df.points[,3]
  i.Lom1$Date<-rownames(i.Lom1)
  i.Lom1<-reshape2::melt(i.Lom1,id.vars = "Date",measure.vars = colnames(i.Lom1[,-ncol(i.Lom1)]))
  colnames(i.Lom1)<-c(colnames(i.Lom1)[1],colnames(df.points)[3],paste(deparse(substitute(x)),"Lom1",sep="."))
  i.Lom1[,1]<-as.Date(i.Lom1[,1])
  i.Lom1[,2]<-as.character(i.Lom1[,2])
  
  j<-merge(j,i.Lom1,all=T)
  
  return(j)
}


fuzzy.extract.single.point<-function(x, #a rts object to extract
                                     ex.points, #a SpatialPoints object
                                     df.points, #a df with columns "Longitude","Latitude", and "Site" - 1st 2 are numeric and 3rd is character
                                     fuzz=0.5 #the amount of shift to use (in degreees Longitude/Latitude) to look for an alternate pixel in each of the 4 cardinal directions
) { 
  time.pointers<-data.frame(x@time)
  time.pointers<-as.Date(rownames(time.pointers))
  i<-raster::extract(x,ex.points,time.pointers)
  i<-as.data.frame(i)
  colnames(i)<-df.points[,3]
  i$Date<-rownames(i)
  i<-reshape2::melt(i,id.vars = "Date",measure.vars = colnames(i[-ncol(i)]))
  colnames(i)<-c(colnames(i)[1],colnames(df.points)[3],paste(deparse(substitute(x))))
  i[,1]<-as.Date(i[,1])
  i[,2]<-as.character(i[,2])
  
  df.points.Ltp1<-df.points
  df.points.Ltp1$Latitude<-df.points.Ltp1$Latitude+fuzz
  pts.Ltp1<-SpatialPoints(df.points.Ltp1[,1:2])
  i.Ltp1<-raster::extract(x,pts.Ltp1,time.pointers)
  i.Ltp1<-as.data.frame(i.Ltp1)
  colnames(i.Ltp1)<-df.points[,3]
  i.Ltp1$Date<-rownames(i.Ltp1)
  i.Ltp1<-reshape2::melt(i.Ltp1,id.vars = "Date",measure.vars = colnames(i.Ltp1[-ncol(i.Ltp1)]))
  colnames(i.Ltp1)<-c(colnames(i.Ltp1)[1],colnames(df.points)[3],paste(deparse(substitute(x)),"Ltp1",sep="."))
  i.Ltp1[,1]<-as.Date(i.Ltp1[,1])
  i.Ltp1[,2]<-as.character(i.Ltp1[,2])
  
  j<-merge(i,i.Ltp1,all=T)
  
  df.points.Ltm1<-df.points
  df.points.Ltm1$Latitude<-df.points.Ltm1$Latitude-fuzz
  pts.Ltm1<-SpatialPoints(df.points.Ltm1[,1:2])
  i.Ltm1<-raster::extract(x,pts.Ltm1,time.pointers)
  i.Ltm1<-as.data.frame(i.Ltm1)
  colnames(i.Ltm1)<-df.points[,3]
  i.Ltm1$Date<-rownames(i.Ltm1)
  i.Ltm1<-reshape2::melt(i.Ltm1,id.vars = "Date",measure.vars = colnames(i.Ltm1[-ncol(i.Ltm1)]))
  colnames(i.Ltm1)<-c(colnames(i.Ltm1)[1],colnames(df.points)[3],paste(deparse(substitute(x)),"Ltm1",sep="."))
  i.Ltm1[,1]<-as.Date(i.Ltm1[,1])
  i.Ltm1[,2]<-as.character(i.Ltm1[,2])
  
  j<-merge(j,i.Ltm1,all=T)
  
  df.points.Lop1<-df.points
  df.points.Lop1$Longitude<-df.points.Lop1$Longitude+fuzz
  pts.Lop1<-SpatialPoints(df.points.Lop1[,1:2])
  i.Lop1<-raster::extract(x,pts.Lop1,time.pointers)
  i.Lop1<-as.data.frame(i.Lop1)
  colnames(i.Lop1)<-df.points[,3]
  i.Lop1$Date<-rownames(i.Lop1)
  i.Lop1<-reshape2::melt(i.Lop1,id.vars = "Date",measure.vars = colnames(i.Lop1[-ncol(i.Lop1)]))
  colnames(i.Lop1)<-c(colnames(i.Lop1)[1],colnames(df.points)[3],paste(deparse(substitute(x)),"Lop1",sep="."))
  i.Lop1[,1]<-as.Date(i.Lop1[,1])
  i.Lop1[,2]<-as.character(i.Lop1[,2])
  
  j<-merge(j,i.Lop1,all=T)
  
  df.points.Lom1<-df.points
  df.points.Lom1$Longitude<-df.points.Lom1$Longitude-fuzz
  pts.Lom1<-SpatialPoints(df.points.Lom1[,1:2])
  i.Lom1<-raster::extract(x,pts.Lom1,time.pointers)
  i.Lom1<-as.data.frame(i.Lom1)
  colnames(i.Lom1)<-df.points[,3]
  i.Lom1$Date<-rownames(i.Lom1)
  i.Lom1<-reshape2::melt(i.Lom1,id.vars = "Date",measure.vars = colnames(i.Lom1[-ncol(i.Lom1)]))
  colnames(i.Lom1)<-c(colnames(i.Lom1)[1],colnames(df.points)[3],paste(deparse(substitute(x)),"Lom1",sep="."))
  i.Lom1[,1]<-as.Date(i.Lom1[,1])
  i.Lom1[,2]<-as.character(i.Lom1[,2])
  
  j<-merge(j,i.Lom1,all=T)
  
  return(j)
}


fuzzy.extract.mean<-function(x) { #a fuzzy.extract object
  x$fill<-rowMeans(x[,4:7],na.rm=T)
  x$fill[is.nan(x$fill)]<-NA
  x[,3][is.na(x[,3])]<-x$fill[is.na(x[,3])]
  return(x)
} 


fuzzy.extract.single<-function(x, #a rts object to extract
                               ex.points, #a SpatialPoints object
                               df.points, #a df with columns "Longitude","Latitude", and "Site" - 1st 2 are numeric and 3rd is character
                               fuzz=0.5 #the amount of shift to use (in degreees Longitude/Latitude) to look for an alternate pixel in each of the 4 cardinal directions
) { #the global name of the source data, typically "EOBS"
  i<-raster::extract(x,ex.points,method="bilinear")%>%cbind(df.points)
  colnames(i)[1]<-deparse(substitute(x))
  
  df.points.Ltp1<-df.points
  df.points.Ltp1$Latitude<-df.points.Ltp1$Latitude+fuzz
  pts.Ltp1<-SpatialPoints(df.points.Ltp1[,1:2])
  i.Ltp1<-raster::extract(x,pts.Ltp1,method="bilinear")%>%cbind(df.points)
  colnames(i.Ltp1)[1]<-paste(deparse(substitute(x)),"Ltp1",sep=".")
  
  j<-merge(i,i.Ltp1,all=T)
  
  df.points.Ltm1<-df.points
  df.points.Ltm1$Latitude<-df.points.Ltm1$Latitude-fuzz
  pts.Ltm1<-SpatialPoints(df.points.Ltm1[,1:2])
  i.Ltm1<-raster::extract(x,pts.Ltm1,method="bilinear")%>%cbind(df.points)
  colnames(i.Ltm1)[1]<-paste(deparse(substitute(x)),"Ltm1",sep=".")
  
  j<-merge(j,i.Ltm1,all=T)
  
  df.points.Lop1<-df.points
  df.points.Lop1$Longitude<-df.points.Lop1$Longitude+fuzz
  pts.Lop1<-SpatialPoints(df.points.Lop1[,1:2])
  i.Lop1<-raster::extract(x,pts.Lop1,method="bilinear")%>%cbind(df.points)
  colnames(i.Lop1)[1]<-paste(deparse(substitute(x)),"Lop1",sep=".")
  
  j<-merge(j,i.Lop1,all=T)
  
  df.points.Lom1<-df.points
  df.points.Lom1$Longitude<-df.points.Lom1$Longitude-fuzz
  pts.Lom1<-SpatialPoints(df.points.Lom1[,1:2])
  i.Lom1<-raster::extract(x,pts.Lom1,method="bilinear")%>%cbind(df.points)
  colnames(i.Lom1)[1]<-paste(deparse(substitute(x)),"Lom1",sep=".")
  
  j<-merge(j,i.Lom1,all=T)
  
  return(j)
}

fuzzy.extract.mean.single<-function(x) { #a fuzzy.extract object
  x$fill<-rowMeans(x[,5:8],na.rm=T)
  x$fill[is.nan(x$fill)]<-NA
  x[,4][is.na(x[,4])]<-x$fill[is.na(x[,4])]
  return(x)
} 
