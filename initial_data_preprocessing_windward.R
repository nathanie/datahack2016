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

write.csv(uni_labeled,"unique_vessels.csv",row.names = F)