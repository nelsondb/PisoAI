
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

GNIP.climate.obs<-read.csv(file=paste0(output.directory,output.filename,".csv"),
                           stringsAsFactors = F,row.names = 1)

GNIP.climate.obs$Date<-as.Date(GNIP.climate.obs$Date)
GNIP.climate.obs$lat2<-GNIP.climate.obs$Latitude^2

#calculate MWL for these GNIP data and use it to gap fill if either isotope is missing 
md<-lm(H2~O18,data=GNIP.climate.obs[GNIP.climate.obs$Latitude>=0,])
LMWL.b<-md$coefficients[[1]]
LMWL.m<-md$coefficients[[2]]
GNIP.climate.obs$d2Hf<-NA
GNIP.climate.obs$d18Of<-NA
GNIP.climate.obs$d2Hf[is.na(GNIP.climate.obs$H2)&is.na(GNIP.climate.obs$O18)]<-NA
GNIP.climate.obs$d2Hf[!is.na(GNIP.climate.obs$H2)]<-GNIP.climate.obs$H2[!is.na(GNIP.climate.obs$H2)]
GNIP.climate.obs$d2Hf[is.na(GNIP.climate.obs$H2)&!is.na(GNIP.climate.obs$O18)]<-GNIP.climate.obs$O18[is.na(GNIP.climate.obs$H2)&!is.na(GNIP.climate.obs$O18)]*LMWL.m+LMWL.b
GNIP.climate.obs$d18Of[is.na(GNIP.climate.obs$H2)&is.na(GNIP.climate.obs$O18)]<-NA
GNIP.climate.obs$d18Of[!is.na(GNIP.climate.obs$O18)]<-GNIP.climate.obs$O18[!is.na(GNIP.climate.obs$O18)]
GNIP.climate.obs$d18Of[!is.na(GNIP.climate.obs$H2)&is.na(GNIP.climate.obs$O18)]<-(GNIP.climate.obs$H2[!is.na(GNIP.climate.obs$H2)&is.na(GNIP.climate.obs$O18)]-LMWL.b)/LMWL.m

#remove out extreme outliers
GNIP.climate.obs$d2Hf[GNIP.climate.obs$dxs<(-30)|GNIP.climate.obs$dxs>50]<-NA
GNIP.climate.obs$d18Of[GNIP.climate.obs$dxs<(-30)|GNIP.climate.obs$dxs>50]<-NA


GNIP.climate.obs<-GNIP.climate.obs%>%distinct(Site, Date, .keep_all = TRUE)

GNIP.climate.obs<-GNIP.climate.obs[!is.na(GNIP.climate.obs$d2Hf),]
GNIP.climate.obs<-GNIP.climate.obs[!is.na(GNIP.climate.obs$d18Of),]

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

GNIP.climate.obs<-completeFun(GNIP.climate.obs, names(GNIP.climate.obs)[names(GNIP.climate.obs)%like%"EOBS"])
GNIP.climate.obs<-completeFun(GNIP.climate.obs, names(GNIP.climate.obs)[names(GNIP.climate.obs)%like%"CRU"])
GNIP.climate.obs<-completeFun(GNIP.climate.obs, names(GNIP.climate.obs)[names(GNIP.climate.obs)%like%"NCEP"])

write.csv(GNIP.climate.obs, file="GNIP.climate.obs_clean.csv")

CRU.vars<-colnames(GNIP.climate.obs)[colnames(GNIP.climate.obs)%like%"CRU"]
EOBS.vars<-colnames(GNIP.climate.obs)[colnames(GNIP.climate.obs)%like%"EOBS"]
NCEP.vars<-colnames(GNIP.climate.obs)[colnames(GNIP.climate.obs)%like%"NCEP"]

x<-GNIP.climate.obs[,c("Elevation","Longitude","Latitude","lat2","Site",#direct GNIP station data or derived products that can be replicated in gridded data sets like CRU, EOBS, DEM
                       "Date",
                       CRU.vars,EOBS.vars,NCEP.vars,
                       "NAO","EA","WP","PNA","EA.WR","SCA","POL",#climate indices
                       "month", #categorical variable to one hot encode, from station data,
                       "climate", #categorical variable to one hot encode - Koeppen climate regions
                       "d2Hf","d18Of","dxs" )] #response variables

xgb.model.inputs<-colnames(dplyr::select(x,-c("Date","d2Hf","d18Of","dxs")))
write.csv(xgb.model.inputs,
          file="xgb.model.inputs.csv")

x$season[x$month%in%c(12,1,2)]<-"DJF"
x$season[x$month%in%c(3,4,5)]<-"MAM"
x$season[x$month%in%c(6,7,8)]<-"JJA"
x$season[x$month%in%c(9,10,11)]<-"SON"

x$month<-as.factor(x$month)
x$season<-as.factor(x$season)
x$climate<-as.factor(x$climate)

dummies <- caret::dummyVars(~ ., data = subset(x, select = -c(Site))) #model for one hot encoding of categorical variables
x2<-as.data.frame(predict(dummies, newdata = x)) #one hot encoding of categorical variables

x<-subset(x, select = c(Longitude,Latitude,Site))
x2<-cbind(x2,x$Site)
names(x2)[names(x2) == "x$Site"] <- "Site"


omit.sites<-c("BASEL","ROVANIEMI","FLEAM DYKE","CHARCHES (VALLE DEL ZALABI)","MINSK","KOCBEYLI (ISPARTA)")
'%!in%' <- function(x,y)!('%in%'(x,y))
x2.sites.out<-x2[x2$Site%!in%omit.sites,]
x2.omitted.sites<-x2[x2$Site%in%omit.sites,]

#remove NAs and save as separate data splits
x2.sites.out.nas<-(x2.sites.out[!complete.cases(x2.sites.out),])
x2.sites.out.nas<-subset(x2.sites.out.nas,select=-c(dxs))
x2.sites.out.nas<-x2.sites.out.nas[complete.cases(x2.sites.out.nas),]
x2.sites.out<-(x2.sites.out[complete.cases(x2.sites.out),])


#split data into thirds based on d2H "ISd2H","ISd18O","ISdxs"
set.seed(1234)
split.ISa  <- rsample::initial_split(x2.sites.out, prop = 0.3333, strata = "d2Hf",breaks = 7)
ISd2H  <- rsample::training(split.ISa) 
ISi   <- rsample::testing(split.ISa) 
set.seed(1234)
split.ISb  <- rsample::initial_split(ISi, prop = 0.5, strata = "d2Hf",breaks = 7)
ISd18O  <- rsample::training(split.ISb) 
ISdxs   <- rsample::testing(split.ISb) 


#split partition 1 into testing, training, watch using d2H, partiiton 2 using d18O, partition 3 using dxs
#d2H
set.seed(1234)
split1.d2H  <- rsample::initial_split(ISd2H, prop = 0.9, strata = "d2Hf",breaks = 7)
ISd2H.tr1  <- rsample::training(split1.d2H) 
ISd2H.test   <- rsample::testing(split1.d2H) 
set.seed(1234)
split2.d2H<- rsample::initial_split(ISd2H.tr1, prop = 0.9, strata = "d2Hf", breaks = 7)
ISd2H.training  <- rsample::training(split2.d2H) 
ISd2H.watch   <- rsample::testing(split2.d2H) 


#d18O
set.seed(1234)
split1.d18O  <- rsample::initial_split(ISd18O, prop = 0.9, strata = "d18Of",breaks = 7)
ISd18O.tr1  <- rsample::training(split1.d18O) 
ISd18O.test   <- rsample::testing(split1.d18O) 
set.seed(1234)
split2.d18O<- rsample::initial_split(ISd18O.tr1, prop = 0.9, strata = "d18Of", breaks = 7)
ISd18O.training  <- rsample::training(split2.d18O) 
ISd18O.watch   <- rsample::testing(split2.d18O) 


#dxs
set.seed(1234)
split1.dxs  <- rsample::initial_split(ISdxs, prop = 0.9, strata = "dxs",breaks = 7)
ISdxs.tr1  <- rsample::training(split1.dxs) 
ISdxs.test   <- rsample::testing(split1.dxs) 
set.seed(1234)
split2.dxs<- rsample::initial_split(ISdxs.tr1, prop = 0.9, strata = "dxs", breaks = 7)
ISdxs.training  <- rsample::training(split2.dxs) 
ISdxs.watch   <- rsample::testing(split2.dxs) 


#split dxs NAs in half based on d2H
set.seed(1234)
split.nas  <- rsample::initial_split(x2.sites.out.nas, prop = 0.5, strata = "d2Hf",breaks = 7)
split.nasd18O  <- rsample::training(split.nas) 
split.nasd2H   <- rsample::testing(split.nas) 

#split each half into training, testing, watch
#d2H
set.seed(1234)
split1.d2Hnas  <- rsample::initial_split(split.nasd2H, prop = 0.9, strata = "d2Hf",breaks = 7)
split.nasd2H.tr1  <- rsample::training(split1.d2Hnas) 
split.nasd2H.test   <- rsample::testing(split1.d2Hnas) 
set.seed(1234)
split2.d2Hnas<- rsample::initial_split(split.nasd2H.tr1, prop = 0.9, strata = "d2Hf", breaks = 7)
split.nasd2H.training  <- rsample::training(split2.d2Hnas) 
split.nasd2H.watch   <- rsample::testing(split2.d2Hnas) 

#d18O
set.seed(1234)
split1.d18Onas  <- rsample::initial_split(split.nasd18O, prop = 0.9, strata = "d18Of",breaks = 7)
split.nasd18O.tr1  <- rsample::training(split1.d18Onas) 
split.nasd18O.test   <- rsample::testing(split1.d18Onas) 
set.seed(1234)
split2.d18Onas<- rsample::initial_split(split.nasd18O.tr1, prop = 0.9, strata = "d18Of", breaks = 7)
split.nasd18O.training  <- rsample::training(split2.d18Onas) 
split.nasd18O.watch   <- rsample::testing(split2.d18Onas) 


test<-merge(x2.omitted.sites,ISd2H.test,all=T)%>%merge(ISd18O.test,all=T)%>%merge(ISdxs.test,all=T)%>%
  merge(split.nasd2H.test,all=T)%>%merge(split.nasd18O.test,all=T)
test<-test[complete.cases(subset(test,select= -c(dxs))),]

training<-merge(ISd2H.training,ISd18O.training,all=T)%>%merge(ISdxs.training,all=T)%>%
  merge(split.nasd2H.training,all=T)%>%merge(split.nasd18O.training,all=T)
training<-training[complete.cases(subset(training,select= -c(dxs))),]

watch<-merge(ISd2H.watch,ISd18O.watch,all=T)%>%merge(ISdxs.watch,all=T)%>%
  merge(split.nasd2H.watch,all=T)%>%merge(split.nasd18O.watch,all=T)
watch<-watch[complete.cases(subset(watch,select= -c(dxs))),]


full<-rbind(test,training,watch)
validation<-rbind(test,watch)


write.csv(training, file="training.csv")
write.csv(test, file="test.csv")
write.csv(watch, file="watch.csv")
write.csv(full, file="full.csv")




