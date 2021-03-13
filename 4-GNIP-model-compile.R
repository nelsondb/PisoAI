
library(rts)
library(data.table)
library(ncdf4)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)
library(rsample)
library(caret)
library(xgboost)


setwd("INSERT FILE PATH HERE")

GNIP.climate.obs<-read.csv("GNIP.climate.obs_clean.csv",
                           stringsAsFactors = F,row.names = 1)
GNIP.climate.obs$Date<-as.Date(GNIP.climate.obs$Date)
GNIP.md<-GNIP.climate.obs

GNIP.d2H<-read.csv("d2H/GNIP.d2H.csv",stringsAsFactors = F,row.names = 1)
GNIP.d2H$Date<-as.Date(GNIP.d2H$Date)
GNIP.d18O<-read.csv("d18O/GNIP.d18O.csv",stringsAsFactors = F,row.names = 1)
GNIP.d18O$Date<-as.Date(GNIP.d18O$Date)
GNIP.dxs<-read.csv("dxs/GNIP.dxs.csv",stringsAsFactors = F,row.names = 1)
GNIP.dxs$Date<-as.Date(GNIP.dxs$Date)

i<-GNIP.d2H
i<-i[names(i)%in%c("Site","Date","d2H.m","model.group")]
colnames(i)[colnames(i)=="model.group"]<-"model.group.d2H.m"
View( i[duplicated(i[,c("Site","Date")])==T,]   )


j<-GNIP.d18O
j<-j[names(j)%in%c("Site","Date","d18O.m","model.group")]
colnames(j)[colnames(j)=="model.group"]<-"model.group.d18O.m"
View( j[duplicated(j[,c("Site","Date")])==T,]   )
str(j)

k<-GNIP.dxs
k<-k[names(k)%in%c("Site","Date","dxs.m","model.group")]
colnames(k)[colnames(k)=="model.group"]<-"model.group.dxs.m"
View( k[duplicated(k[,c("Site","Date")])==T,]   )
str(k)

GNIP.md<-merge(GNIP.md,i,all=T)%>%
  merge(j,all=T)%>%merge(k,all=T)



GNIP.d2H.2<-read.csv("d2H/GNIP.d2H.2.csv",stringsAsFactors = F,row.names = 1)
GNIP.d2H.2$Date<-as.Date(GNIP.d2H.2$Date)
GNIP.d18O.2<-read.csv("d18O/GNIP.d18O.2.csv",stringsAsFactors = F,row.names = 1)
GNIP.d18O.2$Date<-as.Date(GNIP.d18O.2$Date)
GNIP.dxs.2<-read.csv("dxs/GNIP.dxs.2.csv",stringsAsFactors = F,row.names = 1)
GNIP.dxs.2$Date<-as.Date(GNIP.dxs.2$Date)

i<-GNIP.d2H.2
i<-i[names(i)%in%c("Site","Date","d2H.m2","model.group")]
colnames(i)[colnames(i)=="model.group"]<-"model.group.d2H.m2"
View( i[duplicated(i[,c("Site","Date")])==T,]   )
str(i)


j<-GNIP.d18O.2
j<-j[names(j)%in%c("Site","Date","d18O.m2","model.group")]
colnames(j)[colnames(j)=="model.group"]<-"model.group.d18O.m2"
View( j[duplicated(j[,c("Site","Date")])==T,]   )
str(j)

k<-GNIP.dxs.2
k<-k[names(k)%in%c("Site","Date","dxs.m2")]
View( k[duplicated(k[,c("Site","Date")])==T,]   )
str(k)

GNIP.md<-merge(GNIP.md,i,all=T)%>%
  merge(j,all=T)%>%merge(k,all=T)



GNIP.d2H.3<-read.csv("d2H/GNIP.d2H.3.csv",stringsAsFactors = F,row.names = 1)
GNIP.d2H.3$Date<-as.Date(GNIP.d2H.3$Date)
GNIP.d18O.3<-read.csv("d18O/GNIP.d18O.3.csv",stringsAsFactors = F,row.names = 1)
GNIP.d18O.3$Date<-as.Date(GNIP.d18O.3$Date)
GNIP.dxs.3<-read.csv("dxs/GNIP.dxs.3.csv",stringsAsFactors = F,row.names = 1)
GNIP.dxs.3$Date<-as.Date(GNIP.dxs.3$Date)

i<-GNIP.d2H.3
i<-i[names(i)%in%c("Site","Date","d2H.m3","model.group")]
colnames(i)[colnames(i)=="model.group"]<-"model.group.d2H.m3"
View( i[duplicated(i[,c("Site","Date")])==T,]   )
str(i)


j<-GNIP.d18O.3
j<-j[names(j)%in%c("Site","Date","d18O.m3","model.group")]
colnames(j)[colnames(j)=="model.group"]<-"model.group.d18O.m3"
View( j[duplicated(j[,c("Site","Date")])==T,]   )
str(j)

k<-GNIP.dxs.3
k<-k[names(k)%in%c("Site","Date","dxs.m3")]
View( k[duplicated(k[,c("Site","Date")])==T,]   )
str(k)

GNIP.md<-merge(GNIP.md,i,all=T)%>%
  merge(j,all=T)%>%merge(k,all=T)


GNIP.md<-GNIP.md[!is.na(GNIP.md$d2H.m),]

GNIP.md<-GNIP.md %>% rename(
  model.group.d2H.d18O = model.group.d18O.m,
  model.group.dxs = model.group.dxs.m
  )
GNIP.md<-GNIP.md%>%subset(select= -c(model.group.d2H.m,model.group.d2H.m2,model.group.d2H.m3,
                                     model.group.d18O.m2,model.group.d18O.m3))


write.csv(GNIP.md, file="GNIP.md.csv")
