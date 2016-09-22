library(data.table)
library(dplyr)
setwd("~/data/datahack2016")


meetings_train <- fread("./input/meetings_train.csv", data.table = F,stringsAsFactors = F)
port_visits_train <- fread("./input/port_visits_train.csv", data.table = F,stringsAsFactors = F)
meetings_test <- fread("./input/test/meetings_test.csv", data.table = F,stringsAsFactors = F)
port_visits_test <- fread("./input/test/port_visits_test.csv", data.table = F,stringsAsFactors = F)
labels <- fread("./input/vessels_labels_train.csv", data.table = F,stringsAsFactors = F)
test_vessels<- fread("./input/test/vessels_to_label.csv", data.table = F,stringsAsFactors = F)

meetings<- rbind(meetings_train,meetings_test)
port_visits<- rbind(port_visits_train,port_visits_test)

labels$num_type<- as.integer(as.factor(labels$type))
labels$num_type<- labels$num_type - 1

port_visits <- merge(port_visits,labels[,c(1,3)],by = 1,all.x = T)

unique_ves<- as.data.frame(unique(c(unique(meetings$ves_id1),
                                    unique(meetings$ves_id2),unique(port_visits$ves_id))))

uni_labeled<- merge(unique_ves,labels,by = 1,all.x = T)
uni_labeled$`unique(c(unique(meetings$ves_id1), unique(meetings$ves_id2), unique(port_visits$ves_id)))`<- 
  as.character(uni_labeled$`unique(c(unique(meetings$ves_id1), unique(meetings$ves_id2), unique(port_visits$ves_id)))`)

names(uni_labeled)[1]<-'ves_id'

library(reshape2)
ports_by_ves<- dcast(port_visits,port_visits$ves_id~port_visits$port_id,fun.aggregate = length)
vessels_ports_types<- merge(uni_labeled,ports_by_ves,by = 1)
write.csv(vessels_ports_types,"ports_by_ves.csv",row.names = F)

rm(meetings_test,meetings_train,port_visits_test,port_visits_train,labels,unique_ves)
gc()
dates<- as.Date(port_visits$start_time)

port_visits$dayOfWeek<- format(dates,format="%a")
port_visits$dayOfWeek<- as.integer(as.factor(port_visits$dayOfWeek))
port_visits$dayOfMonth<- format(dates,format="%d")
port_visits$dayOfMonth<- as.integer(as.factor(port_visits$dayOfMonth))
port_visits$Month<- format(dates,format="%m")
port_visits$Month<- as.integer(as.factor(port_visits$Month))
port_visits$hour<- format(dates,format="%H")
port_visits$hour<- as.integer(as.factor(port_visits$hour))

write.csv(port_visits,"port_visits.csv",row.names = F)
write.csv(meetings,"meetings.csv",row.names = F)
write.csv(uni_labeled,"uni_labeled.csv",row.names = F)
write.csv(ports_by_ves,"ports_by_ves.csv",row.names = F)

#start here to read preprocessed files

meetings <- fread("meetings.csv", data.table = F,stringsAsFactors = F)
uni_labeled <- fread("uni_labeled.csv", data.table = F,stringsAsFactors = F)
ports_by_ves <- fread("ports_by_ves.csv", data.table = F,stringsAsFactors = F)
port_visits <- fread("port_visits.csv", data.table = F,stringsAsFactors = F)

ports_by_ves[,3724:3905]<-NULL

ext_feat_weekday<- dcast(port_visits,port_visits$ves_id~port_visits$dayOfWeek)
ext_feat_day<- dcast(port_visits,port_visits$ves_id~port_visits$dayOfMonth)
ext_feat_hour<- dcast(port_visits,port_visits$ves_id~port_visits$hour)
ext_feat_Month<- dcast(port_visits,port_visits$ves_id~port_visits$Month)
ext_feat_country<- dcast(port_visits,port_visits$ves_id~port_visits$country)

ports_by_ves<-merge(ports_by_ves,ext_feat_day,by=1)
ports_by_ves<-merge(ports_by_ves,ext_feat_weekday,by=1)
ports_by_ves<-merge(ports_by_ves,ext_feat_hour,by=1)
ports_by_ves<-merge(ports_by_ves,ext_feat_Month,by=1)
ports_by_ves<-merge(ports_by_ves,ext_feat_country,by=1)

time_in_ports<-port_visits %>%
  group_by(ves_id) %>%
  summarise(total_time_in_ports = sum(duration_min),avg_time_in_port=mean(duration_min))

all_data<- merge(ports_by_ves,time_in_ports,by=1)

all_data$port_ent_counts<-rowSums(all_data[,4:3723])
all_data$port_ent_non_zero<-rowSums(all_data[,4:3723]>0)

write.csv(all_data,"all_data.csv",row.names = F)
all_data<-fread("all_data.csv",data.table = F,stringsAsFactors = F)

rm(ext_feat_Month,ext_feat_hour,ext_feat_day,ext_feat_weekday,time_in_ports,vessels_ports_types)

features<-as.data.frame(names(all_data))


#-------
all_data$`5358fc77b68ca120a07dab3a`<- as.numeric(all_data$`5358fc77b68ca120a07dab3a`)

#all_data<-full_data
full_data<- all_data
test_data<- all_data[is.na(all_data$type),]
all_data<- all_data[!is.na(all_data$type),]

types<-all_data$num_type 

set.seed(12345)

h<- sample(nrow(all_data),2500)

all_data[is.na(all_data)]<-0
  #data_types<- NULL
  #for (i in 1:ncol(all_data)) data_types[i]<-class(all_data[,i])
  #which(data_types=='character')
  #for (f in which(data_types=='character'))
  #  all_data[,f]<- as.integer(as.factor(all_data[,f]))

  #all_data[,3593]<-NULL

trdata<- all_data[-h,]
valdata<- all_data[h,]

library(xgboost)
dval<-xgb.DMatrix(data=data.matrix(valdata[,4:ncol(all_data)]),label= types[h])
dtrain<-xgb.DMatrix(data=data.matrix(trdata[,4:ncol(all_data)]),label= types[-h])
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "multi:softmax", 
                eval_metric         = "merror",
                num_class           = 7,
                booster             = "gbtree",
                eta                 = 0.025, # 0.06, #0.01,
                max_depth           = 5, #changed from default of 8
                subsample           = 0.6, # 0.7
                colsample_bytree    = 0.8, # 0.7
                alpha               = 4, 
                min.child.weight    = 5
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000, #300, #280, #125, #250, # changed from 300
                    verbose             = 1,
                    early.stop.round    = 200,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    print.every.n       = 10
)

preds <- predict(clf,as.matrix(valdata[,c(4:ncol(all_data))]))
err_func<- function(tru,pred){
  return(sum(tru==pred)/length(tru))
}

err_func(preds,(types[h]))

#------

uports<-unique(port_visits$port_id)
uports<- as.data.frame(uports)

port_categories<-port_visits %>%
  group_by(port_id,num_type) %>%
  summarise(quan = length(ves_id))

temp_port_categories<- dcast(port_categories,port_categories$port_id~port_categories$num_type)

write.csv(port_visits,"port_visits_all.csv",row.names = F)
write.csv(meetings,"meetings_all.csv",row.names = F)
