#this file is based on initial preproc. file and is used to prep. meetings data

library(data.table)
library(dplyr)
setwd("~/data/datahack2016")

heatmap_by_cluster<- function(meetings,cls){
  heatmapData<- matrix(0,nrow = 8,ncol = 8)
  for (i in 1:7)
    for (j in 1:7)
      heatmapData[i,j]<- sum(meetings$num_types.x==i-1 & 
                               meetings$num_types.y == j-1 & meetings$clst==cls,na.rm = T)
    
    for (i in 1:8) {
      heatmapData[i,8]<- sum(meetings$num_types.x==i & is.na(meetings$num_types.y) & meetings$clst==cls)
      heatmapData[8,i]<- sum(meetings$num_types.y==i & is.na(meetings$num_types.x) & meetings$clst==cls)
    }  
    
    heatmapData<-as.data.frame(heatmapData)
    
    all_explicit_meetings<-sum(heatmapData[1:7,1:7])
    heatmapDataDensity<- round(heatmapData/all_explicit_meetings,digits = 3)
    
    return (heatmapDataDensity)
    
}


meetings <- fread("./input/meetings_train.csv", data.table = F,stringsAsFactors = F)
port_visits <- fread("./input/port_visits_train.csv", data.table = F,stringsAsFactors = F)
uni_labeled <- fread("./unique_vessels.csv", data.table = F,stringsAsFactors = F)
uni_labeled$num_types<-as.integer(as.factor(uni_labeled$type))-1 
meetings<- merge(meetings,uni_labeled[,c(1,3)],by = 1)
meetings<- merge(meetings,uni_labeled[,c(1,3)],by.x = 2,by.y = 1)
num_clust<-150
clst<- kmeans(meetings[,c(5:6)],centers = num_clust,iter.max = 5000)
plot(clst$centers,col = clst$cluster)
meetings$clst<- clst$cluster


library(d3heatmap)  
heatmapDataDensity <- heatmap_by_cluster(meetings = meetings,cls = 5)
d3heatmap(heatmapDataDensity[1:7,1:7], scale = 'none', dendrogram = "none")

#heatmap(heatmapData,Rowv = NA,Colv = NA,scale = 'none')

num_ves_by_label<-uni_labeled %>%
  group_by(type) %>%
  summarise(counts = length(ves_id))

library(reshape2)
meetPoints_by_vessels1<- dcast(meetings,meetings$ves_id1~meetings$clst,fun.aggregate = length)
meetPoints_by_vessels2<- dcast(meetings,meetings$ves_id2~meetings$clst,fun.aggregate = length)
names(meetPoints_by_vessels1)[1]<-'ves_id'
names(meetPoints_by_vessels2)[1]<-'ves_id'
meetPoints_by_vessels<- rbind(meetPoints_by_vessels1,meetPoints_by_vessels2)
rm(meetPoints_by_vessels1,meetPoints_by_vessels2)
#write.csv(meetPoints_by_vessels,"meetPoints_by_vessels.csv",row.names = F)
#write.csv(ports_by_ves,"ports_by_vessel.csv",row.names = F)

#meetPoints_by_vessels <- fread("./meetPoints_by_vessels.csv", data.table = F,stringsAsFactors = F)


temp<- merge(uni_labeled,meetPoints_by_vessels,by = 1)
temp<- temp[complete.cases(temp),]
temp$`1`<- as.numeric(temp$`1`)
#types<-as.integer(as.factor(temp$type)) 
#types = types -1
set.seed(12345)
h<- sample(nrow(meetPoints_by_vessels),2500)
trdata<- temp[-h,]
valdata<- temp[h,]
library(xgboost)
dval<-xgb.DMatrix(data=data.matrix(valdata[,4:(num_clust+3)]),label=types[h])
dtrain<-xgb.DMatrix(data=data.matrix(trdata[,4:(num_clust+3)]),label=types[-h])
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "multi:softprob", 
                eval_metric         = "mlogloss",
                num_class           = 7,
                booster             = "gbtree",
                eta                 = 0.25, # 0.06, #0.01,
                max_depth           = 5, #changed from default of 8
                subsample           = 0.8, # 0.7
                colsample_bytree    = 0.7, # 0.7
                alpha               = 0, 
                min.child.weight    = 1
                # lambda = 1
)



cv_score <-xgb.cv(params = param,data = xgb.DMatrix(as.matrix(temp[,4:(num_clust+3)]),label = temp$num_types),nfold = 10,nrounds = 2500,
       print.every.n = 2)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000, #300, #280, #125, #250, # changed from 300
                    verbose             = 1,
                    early.stop.round    = 20,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    print.every.n       = 2
)
#[2498]	train-mlogloss:0.474984+0.001520	test-mlogloss:0.564473+0.019004