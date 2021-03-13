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

d2H.importance<-read.csv("d2H/xgb.importance 1-center,scale - dxs cut -30 50 - test 0.9-watch 0.9 split .csv",
                         stringsAsFactors = F,row.names = 1)
d2H.importance<-mutate(group_by(d2H.importance), cumsum=cumsum(Importance))

d18O.importance<-read.csv("d18O/xgb.importance 1-center,scale - dxs cut -30 50 - test 0.9-watch 0.9 split .csv",
                          stringsAsFactors = F,row.names = 1)
d18O.importance<-mutate(group_by(d18O.importance), cumsum=cumsum(Importance))

dxs.importance<-read.csv("dxs/xgb.importance 1-center,scale - dxs cut -30 50 - test 0.9-watch 0.9 split .csv",
                          stringsAsFactors = F,row.names = 1)
dxs.importance<-mutate(group_by(dxs.importance), cumsum=cumsum(Importance))



iA<-d2H.importance[d2H.importance$cumsum<=0.9,]
iB<-d18O.importance[d18O.importance$cumsum<=0.9,]
iC<-dxs.importance[dxs.importance$cumsum<=0.9,]

xgb.vars.2<-c(as.character(iA$Feature),as.character(iB$Feature),as.character(iC$Feature))%>%unique
write.csv(xgb.vars.2, file="xgb.model.inputs.2.csv")




