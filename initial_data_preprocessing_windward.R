library(data.table)
library(dplyr)
setwd("~/data/datahack2016")

meetings <- fread("./input/meetings_train.csv", data.table = F,stringsAsFactors = F)
port_visits <- fread("./input/port_visits_train.csv", data.table = F,stringsAsFactors = F)
labels <- fread("./input/vessels_labels_train.csv", data.table = F,stringsAsFactors = F)

unique_ves<- as.data.frame(unique(c(unique(meetings$ves_id1),
                                      unique(meetings$ves_id2),unique(port_visits$ves_id))))

uni_labeled<- merge(unique_ves,labels,by = 1,all.x = T)
uni_labeled$`unique(c(unique(meetings$ves_id1), unique(meetings$ves_id2), unique(port_visits$ves_id)))`<- 
  as.character(uni_labeled$`unique(c(unique(meetings$ves_id1), unique(meetings$ves_id2), unique(port_visits$ves_id)))`)

names(uni_labeled)[1]<-'ves_id'

num_ves_by_label<-uni_labeled %>%
  group_by(type) %>%
  summarise(counts = length(ves_id))

library(reshape2)
ports_by_ves<- dcast(port_visits,port_visits$ves_id~port_visits$port_id,fun.aggregate = length)

#library(CatEncoders)
#ohenc<- OneHotEncoder.fit(as.data.frame(port_visits[,c(1,5)]))

#write.csv(uni_labeled,"unique_vessels.csv",row.names = F)
#write.csv(ports_by_ves,"ports_by_vessel.csv",row.names = F)

ports_by_ves <- fread("./ports_by_vessel.csv", data.table = F,stringsAsFactors = F)
uni_labeled <- fread("./unique_vessels.csv", data.table = F,stringsAsFactors = F)

temp<- merge(uni_labeled,ports_by_ves,by = 1)

temp$`5358fc77b68ca120a07dab3a`<- as.numeric(temp$`5358fc77b68ca120a07dab3a`)
types<-as.integer(as.factor(temp$type)) 
types = types -1
set.seed(12345)
h<- sample(nrow(ports_by_ves),2500)
trdata<- temp[-h,]
valdata<- temp[h,]
library(xgboost)
dval<-xgb.DMatrix(data=data.matrix(valdata[,3:3592]),label=types[h])
dtrain<-xgb.DMatrix(data=data.matrix(trdata[,3:3592]),label=types[-h])
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "multi:softprob", 
                eval_metric         = "mlogloss",
                num_class           = 7,
                booster             = "gbtree",
                eta                 = 0.025, # 0.06, #0.01,
                max_depth           = 5, #changed from default of 8
                subsample           = 0.8, # 0.7
                colsample_bytree    = 0.7, # 0.7
                alpha               = 4, 
                min.child.weight    = 1
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000, #300, #280, #125, #250, # changed from 300
                    verbose             = 1,
                    early.stop.round    = 20,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    print.every.n       = 2
)

xgb.cv(params = param,data = xgb.DMatrix(as.matrix(temp[,3:3592]),label = types),nfold = 10,nrounds = 2500,
       print.every.n = 2)
#[2498]	train-mlogloss:0.474984+0.001520	test-mlogloss:0.564473+0.019004