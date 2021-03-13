
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



training.import<-read.csv("training.csv",stringsAsFactors = F,row.names = 1)
training.import$Date<-as.Date(training.import$Date)
test.import<-read.csv("test.csv",stringsAsFactors = F,row.names = 1)
test.import$Date<-as.Date(test.import$Date)
watch.import<-read.csv("watch.csv",stringsAsFactors = F,row.names = 1)
watch.import$Date<-as.Date(watch.import$Date)


GNIP.climate.obs<-read.csv("GNIP.climate.obs_clean.csv",
                           stringsAsFactors = F,row.names = 1)

training<-training.import
test<-test.import
watch<-watch.import

GNIP.d2H<-read.csv("d2H/GNIP.d2H.csv",stringsAsFactors = F,row.names = 1)
GNIP.d18O<-read.csv("d18O/GNIP.d18O.csv",stringsAsFactors = F,row.names = 1)
GNIP.dxs<-read.csv("dxs/GNIP.dxs.csv",stringsAsFactors = F,row.names = 1)

i<-GNIP.d2H

j<-GNIP.d18O
j<-j[names(j)%in%c("Site","Date","d18O.m","model.group")]

k<-GNIP.dxs
k<-k[names(k)%in%c("Site","Date","dxs.m","model.group")]
colnames(k)[colnames(k)=="model.group"]<-"model.group.dxs"


GNIP.md<-merge(i,j,all=T)%>%merge(k,all=T)


GNIP.d2H.2<-read.csv("d2H/GNIP.d2H.2.csv",stringsAsFactors = F,row.names = 1)
GNIP.d18O.2<-read.csv("d18O/GNIP.d18O.2.csv",stringsAsFactors = F,row.names = 1)
GNIP.dxs.2<-read.csv("dxs/GNIP.dxs.2.csv",stringsAsFactors = F,row.names = 1)


i<-GNIP.d2H.2

j<-GNIP.d18O.2
j<-j[names(j)%in%c("Site","Date","d18O.m2","model.group")]

k<-GNIP.dxs.2
k<-k[names(k)%in%c("Site","Date","dxs.m2","model.group")]
colnames(k)[colnames(k)=="model.group"]<-"model.group.dxs"


GNIP.md2<-merge(i,j,all=T)%>%merge(k,all=T)
i<-subset(GNIP.md2,select=c("d2H.m2","d18O.m2","dxs.m2","model.group","model.group.dxs"))
colnames(i)[colnames(i)=="model.group"]<-"model.group2"
colnames(i)[colnames(i)=="model.group.dxs"]<-"model.group.dxs2"

x<-cbind(GNIP.md,i)
GNIP.md<-x

GNIP.md<-GNIP.md[!is.na(GNIP.md$Site),]


i<-GNIP.md[GNIP.md$model.group=="training.data",]
i<-i[!is.na(i$d2H.m2),]
i<-i[!is.na(i$d18O.m2),]
i<-subset(i,select=c(Site,Date,d2H.m,d2H.m2,d18O.m,d18O.m2,dxs.m,dxs.m2,model.group,model.group.dxs))
i$Date<-as.Date(i$Date)
training.3<-merge(training.import,i,all.x=T)


i<-GNIP.md[GNIP.md$model.group=="test.data",]
i<-i[!is.na(i$d2H.m2),]
i<-i[!is.na(i$d18O.m2),]
i<-subset(i,select=c(Site,Date,d2H.m,d2H.m2,d18O.m,d18O.m2,dxs.m,dxs.m2,model.group,model.group.dxs))
i$Date<-as.Date(i$Date)
test.3<-merge(test.import,i,all.x=T)


i<-GNIP.md[GNIP.md$model.group=="watch.data",]
i<-i[!is.na(i$d2H.m2),]
i<-i[!is.na(i$d18O.m2),]
i<-subset(i,select=c(Site,Date,d2H.m,d2H.m2,d18O.m,d18O.m2,dxs.m,dxs.m2,model.group,model.group.dxs))
i$Date<-as.Date(i$Date)
watch.3<-merge(watch.import,i,all.x=T)

write.csv(training.3, file="training.3.csv")
write.csv(test.3, file="test.3.csv")
write.csv(watch.3, file="watch.3.csv")

