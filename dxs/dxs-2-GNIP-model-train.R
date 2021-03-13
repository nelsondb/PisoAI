
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
test.import<-read.csv("test.csv",stringsAsFactors = F,row.names = 1)
watch.import<-read.csv("watch.csv",stringsAsFactors = F,row.names = 1)

GNIP.climate.obs<-read.csv("GNIP.climate.obs_clean.csv",
                           stringsAsFactors = F,row.names = 1)

training<-training.import
test<-test.import
watch<-watch.import

xgb.vars.2<-read.csv("xgb.model.inputs.2.csv",stringsAsFactors = F,row.names = 1)

predictors<-as.character(xgb.vars.2$x)

response<-"dxs"

training<-training[,c("Site",predictors,response)]
test<-test[,c("Site",predictors,response)]
watch<-watch[,c("Site",predictors,response)]

xgb.model.inputs<-colnames(dplyr::select(training,-c(all_of(response))))


training<-training[complete.cases(training),] #select only complete cases
test<-test[complete.cases(test),] #select only complete cases
watch<-watch[complete.cases(watch),] #select only complete cases

training<-subset(training, select = -c(Site))
test<-subset(test, select = -c(Site))
watch<-subset(watch, select = -c(Site))


dxs.pp.2 <- caret::preProcess(subset(training,select=-c(dxs)), # last column is the outcome 
                            method = c("center", "scale"))
dxs.pp.2 #this will show which columns were centered and scaled
saveRDS(dxs.pp.2,file="dxs/dxs.pp.2")


training.tr <- predict(dxs.pp.2, newdata = subset(training,select=-c(dxs)))
training.target<-subset(training,select=c(dxs))

test.tr <- predict(dxs.pp.2, newdata = subset(test,select=-c(dxs)))
test.target<-subset(test,select=c(dxs))

watch.tr <- predict(dxs.pp.2, newdata = subset(watch,select=-c(dxs)))
watch.target<-subset(watch,select=c(dxs))

full.tr <- rbind(training.tr,test.tr,watch.tr)
full.target<-rbind(training.target,test.target,watch.target)

training.target<-as.numeric(training.target$dxs)
test.target<-as.numeric(test.target$dxs)
watch.target<-as.numeric(watch.target$dxs)
full.target<-as.numeric(full.target$dxs)


### directly with xgboost package
dtrain <- xgboost::xgb.DMatrix(data = as.matrix(training.tr), label = training.target)
dtest <- xgboost::xgb.DMatrix(data = as.matrix(test.tr), label = test.target)
dwatch <- xgboost::xgb.DMatrix(data = as.matrix(watch.tr), label = watch.target)
dfull<-xgboost::xgb.DMatrix(data = as.matrix(full.tr), label = full.target)
watchlist <- list(train = dtrain, eval = dwatch)




cv.ctrl <- caret::trainControl(method = "cv",number = 4,
                               verboseIter=TRUE,
                               allowParallel=T)



#run repeated tune grids to optimize hyperparamters, then select best combination for full training
grid.number<-1
#hyperparameter fitting - full grid is much to large to process 
xgbGrid <- expand.grid(max_depth = c(2,4,7,8,9,12,16),
                       eta = c(0.05,0.1,0.3),
                       rate_drop = c(0,0.01,0.02),
                       skip_drop = c(0,0.01,0.02),
                       min_child_weight = c(0,2,4,8,12,14,16,20),
                       subsample = c(0.7,0.8,0.85,0.9,0.95,1),
                       colsample_bytree = c(0.7,0.8,0.85,0.9,0.95,1),
                       gamma = c(0,2,5,10,20,100,500,1000),
                       nrounds = c(250,300,400)
)



set.seed(1234)
xgb_tune <-caret::train(training.tr,training.target,
                        method="xgbDART",
                        trControl=cv.ctrl,
                        tuneGrid=xgbGrid[sample(1:nrow(xgbGrid), 200),], #limit number of samples to try from grid
                        verbose=T,
                        metric="RMSE"
)


xgb.tune.df<-xgb_tune$results
xgb.tune.df$grid.number<-grid.number
#use the following line after the first grid only
xgb_grid30<-xgb.tune.df

#run the following line to append results from later rounds of grid searching
# xgb_grid30<-rbind(xgb_grid30,xgb.tune.df)


write.csv(xgb_grid30, file = "dxs/dxs.xgb_grid30.2_srv.csv",row.names=F)

tuneround="1-center,scale - dxs cut -30 50 - test 0.9-watch 0.9 split" #tune round is the number and can be used to export comments 
param <- list(booster = "dart",
              verbosity = 1,
              max_depth = 8, 
              eta = 0.1,
              rate_drop = 0.01,
              skip_drop = 0,
              min_child_weight = 60,
              subsample = 0.9,
              colsample_bytree = 0.9,
              gamma = 50,
              objective = "reg:linear", 
              eval_metric = "rmse")


nrounds = 1500
set.seed(1234)
bst <- xgb.train(param, dtrain, nrounds = nrounds, watchlist)

#save model in 2 formats as a precaution
saveRDS(bst,file="dxs/dxs.model2RDS")
dxs.featureIDs<-as.character(bst$feature_names)
saveRDS(dxs.featureIDs,file="dxs/dxs.featureIDs.2")
xgb.save(bst,"dxs/dxs.model2XGB")


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
xgbDart.tuning30<-param

#importance
importance <- xgb.importance(feature_names = names(training.tr), model = bst)
write.csv(importance, file = paste("dxs/xgb.importance.2",tuneround,".csv"))

write.csv(xgbDart.tuning30, file = "dxs/dxs.xgbDart.tuning30.2.csv")  




##### make predictions for all sites, even if original dxs value wasn't available 

training<-training.import
test<-test.import
watch<-watch.import

training<-training[,c("Site",predictors,response)]
test<-test[,c("Site",predictors,response)]
watch<-watch[,c("Site",predictors,response)]

training<-subset(training, select = -c(Site))
test<-subset(test, select = -c(Site))
watch<-subset(watch, select = -c(Site))

training.tr <- predict(dxs.pp.2, newdata = subset(training,select=-c(dxs)))
test.tr <- predict(dxs.pp.2, newdata = subset(test,select=-c(dxs)))
watch.tr <- predict(dxs.pp.2, newdata = subset(watch,select=-c(dxs)))
full.tr <- rbind(training.tr,test.tr,watch.tr)


dtrain <- xgboost::xgb.DMatrix(data = as.matrix(training.tr))
dtest <- xgboost::xgb.DMatrix(data = as.matrix(test.tr))
dwatch <- xgboost::xgb.DMatrix(data = as.matrix(watch.tr))
watchlist <- list(train = dtrain, eval = dwatch)



set.seed(1234)
iA<-test.import
iA$dxs.m2<-predict(bst, dtest, nrounds = nrounds)
set.seed(1234)
iB<-watch.import
iB$dxs.m2<-predict(bst, dwatch, nrounds = nrounds)
set.seed(1234)
iC<-training.import
iC$dxs.m2<-predict(bst, dtrain, nrounds = nrounds)


iA$model.group<-"test.data"
iB$model.group<-"watch.data"
iC$model.group<-"training.data"

i<-rbind(iA,iB,iC)
i<-i[names(i)%in%c("Site","Date","dxs.m2","model.group")]

GNIP.climate.obs$Date<-as.Date(GNIP.climate.obs$Date)
i$Date<-as.Date(i$Date)


GNIP<-merge(GNIP.climate.obs,i,all.x=T)



