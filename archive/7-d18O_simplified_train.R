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

getwd()
setwd("INSERT FILE PATH HERE")
d2H.importance<-read.csv("d2H/xgb.importance 1-center,scale,nzv -full-loctrain.csv",
                         stringsAsFactors = F,row.names = 1)
d2H.importance<-mutate(group_by(d2H.importance), cumsum=cumsum(Importance))

d18O.importance<-read.csv("d18O/xgb.importance 1-center,scale,nzv -full-loctrain.csv",
                          stringsAsFactors = F,row.names = 1)
d18O.importance<-mutate(group_by(d18O.importance), cumsum=cumsum(Importance))

i<-d2H.importance[d2H.importance$cumsum<=0.9,]
j<-d18O.importance[d18O.importance$cumsum<=0.9,]
xgb.vars<-c(as.character(i$Feature),as.character(j$Feature))%>%unique


GNIP.climate.obs<-read.csv(file=paste0(output.directory,output.filename,".csv"),
                           stringsAsFactors = F,row.names = 1)
GNIP.climate.obs$Date<-as.Date(GNIP.climate.obs$Date)
GNIP.climate.obs$lat2<-GNIP.climate.obs$Latitude^2

#remove out extreme outliers
GNIP.climate.obs$d2Hf[GNIP.climate.obs$dxs<(-30)|GNIP.climate.obs$dxs>50]<-NA
GNIP.climate.obs$d18Of[GNIP.climate.obs$dxs<(-30)|GNIP.climate.obs$dxs>50]<-NA

x<-GNIP.climate.obs[,c("Site",xgb.vars,
                       "d18Of" )]

d18O.xgb.vl.model.inputs<-colnames(dplyr::select(x,-c("d18Of")))
write.csv(d18O.xgb.vl.model.inputs,
          file="d18O/d18O.xgb.vl.model.inputs.csv")

x<-x[complete.cases(x),] #select only complete cases

x2<-x

omit.sites<-c("BASEL","ROVANIEMI","FLEAM DYKE","CHARCHES (VALLE DEL ZALABI)","MINSK","KOCBEYLI (ISPARTA)")
'%!in%' <- function(x,y)!('%in%'(x,y))
x2.sites.out<-x2[x2$Site%!in%omit.sites,]
x2.omitted.sites<-x2[x2$Site%in%omit.sites,]

set.seed(1234)
split  <- rsample::initial_split(x2.sites.out, prop = 0.9, strata = "d18Of",breaks = 7)
training  <- rsample::training(split) #puts about 31500 obs here
test   <- rsample::testing(split) #puts about 3500 obs here, plus the omitted sites
test <- rbind(test,x2.omitted.sites)

set.seed(1234)
split.2<- rsample::initial_split(training, prop = 0.9, strata = "d18Of", breaks = 7)
training  <- rsample::training(split.2) #puts about 28000 obs here
watch   <- rsample::testing(split.2) #puts about 3200 obs here

training<-subset(training, select = -c(Site))
test<-subset(test, select = -c(Site))
watch<-subset(watch, select = -c(Site))


d18O.xgb28vl.pp <- caret::preProcess(subset(training,select=-c(d18Of)), # last column is the outcome 
                                  method = c("center", "scale", "nzv"))
saveRDS(d18O.xgb28vl.pp,file="d18O/d18O.xgb28vl.pp")

training.tr <- predict(d18O.xgb28vl.pp, newdata = subset(training,select=-c(d18Of)))
training.target<-subset(training,select=c(d18Of))

test.tr <- predict(d18O.xgb28vl.pp, newdata = subset(test,select=-c(d18Of)))
test.target<-subset(test,select=c(d18Of))

watch.tr <- predict(d18O.xgb28vl.pp, newdata = subset(watch,select=-c(d18Of)))
watch.target<-subset(watch,select=c(d18Of))

full.tr <- rbind(training.tr,test.tr,watch.tr)
full.target<-rbind(training.target,test.target,watch.target)



training.target<-as.numeric(training.target$d18Of)
test.target<-as.numeric(test.target$d18Of)
watch.target<-as.numeric(watch.target$d18Of)
full.target<-as.numeric(full.target$d18Of)


dtrain <- xgboost::xgb.DMatrix(data = as.matrix(training.tr), label = training.target)
dtest <- xgboost::xgb.DMatrix(data = as.matrix(test.tr), label = test.target)
dwatch <- xgboost::xgb.DMatrix(data = as.matrix(watch.tr), label = watch.target)
dfull<-xgboost::xgb.DMatrix(data = as.matrix(full.tr), label = full.target)
watchlist <- list(train = dtrain, eval = dwatch)


cv.ctrl <- caret::trainControl(method = "repeatedcv", repeats = 1,number = 3,
                               search = "random",
                               allowParallel=T)

grid.number<-1
start.time <- Sys.time()
xgbGrid <- expand.grid(max_depth = c(6,8,10),
                       eta = c(0.1),
                       rate_drop = c(0.01),
                       skip_drop = c(0),
                       min_child_weight = c(7,8,9),
                       subsample = c(0.9),
                       colsample_bytree = c(0.85),
                       gamma = c(0),
                       nrounds = c(250)
)

set.seed(1234)
xgb_tune <-caret::train(training.tr,training.target,
                        method="xgbDART",
                        trControl=cv.ctrl,
                        tuneGrid=xgbGrid,
                        verbose=T,
                        metric="RMSE"
)

xgb_tune
xgb.tune.df<-xgb_tune$results
xgb.tune.df$grid.number<-grid.number
#use the following line after the first grid only
xgb_grid28vl<-xgb.tune.df

#run the following line to append results from later rounds of grid searching
#xgb_grid28vl<-rbind(xgb_grid28vl,xgb.tune.df)

write.csv(xgb_grid28vl, file = "d18O/d18O.xgb_grid28vl.csv")

tuneround="1-center,scale,nzv" #tune round is the number and can be used to export comments 
param <- list(booster = "dart",
              verbosity = 1,
              max_depth = 8, 
              eta = 0.1,
              rate_drop = 0.01,
              skip_drop = 0,
              min_child_weight = 8,
              subsample = 0.85,
              colsample_bytree = 0.9,
              gamma = 0,
              objective = "reg:linear", 
              eval_metric = "rmse")


nrounds = 1500
set.seed(1234)
bst <- xgb.train(param, dtrain, nrounds = nrounds, watchlist)
i<-bst$evaluation_log %>%
  select(-contains("std"))
training.plot.d18O.xgb28vl<-
  ggplot(i) + 
  geom_line(aes(x = iter, y = train_rmse),colour="red") + 
  geom_line(aes(x = iter, y = eval_rmse),colour="green") + 
  scale_y_continuous(limits = c(5,20))+
  theme_bw()
training.plot.d18O.xgb28vl
pdf("d18O/training.plot.d18O.xgb28vl.pdf",width=8,height=8)
training.plot.d18O.xgb28vl
dev.off()

saveRDS(bst,file="d18O/d18O.xgb28vl.model")

set.seed(1234)
pred <- predict(bst, dtest, nrounds = nrounds)
pred.test<-as.data.frame(cbind(pred, test.target))
param$test.rmse<-RMSE(pred.test$pred,pred.test$test.target)
param$test.rmse

set.seed(1234)
pred <- predict(bst, dwatch, nrounds = nrounds)
pred.watch<-as.data.frame(cbind(pred, watch.target))
param$watch.rmse<-RMSE(pred.watch$pred,pred.watch$watch.target)
param$watch.rmse

set.seed(1234)
pred <- predict(bst, dtrain, nrounds = nrounds)
pred.train<-as.data.frame(cbind(pred, training.target))
param$train.rmse<-RMSE(pred.train$pred,pred.train$training.target)
param$train.rmse

set.seed(1234)
pred <- predict(bst, dfull, nrounds = nrounds)
pred.full<-as.data.frame(cbind(pred, full.target))
param$full.rmse<-RMSE(pred.full$pred,pred.full$full.target)
param$full.rmse

param$nrounds<-nrounds
param$tuneround<-tuneround

param<-as.data.frame(param)

#use the following line after the first full training only
xgbDart.tuning28vl<-param
#if more than one set of hyperparameters is used for full model training, the line above can be deactivated and the line below can be activated 
#xgbDart.tuning28vl<-rbind(xgbDart.tuning28vl,param)

importance <- xgb.importance(feature_names = names(training.tr), model = bst)

write.csv(importance, file = paste("d18O/xgb.importance",tuneround,".csv"))

write.csv(xgbDart.tuning28vl, file = "d18O/d18O.xgbDart.tuning28vl.csv")  



set.seed(1234)
iA<-test
iA$xgb28vl.d18O.m<-predict(bst, dtest, nrounds = nrounds)
set.seed(1234)
iB<-watch
iB$xgb28vl.d18O.m<-predict(bst, dwatch, nrounds = nrounds)
set.seed(1234)
iC<-training
iC$xgb28vl.d18O.m<-predict(bst, dtrain, nrounds = nrounds)


iA$xgb28vl.model.group<-"test.data"
iB$xgb28vl.model.group<-"watch.data"
iC$xgb28vl.model.group<-"training.data"

GNIP.xgb28vl<-rbind(iA,iB,iC)
GNIP.xgb28vl<-merge(GNIP.climate.obs,GNIP.xgb28vl,all=T)
write.csv(GNIP.xgb28vl, file="d18O/GNIP.d18O.xgb28vl.csv")



