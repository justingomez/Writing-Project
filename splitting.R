setwd("~/Writing Project")
library(splitstackshape)

all.data<-read.csv("weeklynflstats.csv")[,-1] #dim: 5258 x 13
all.data$id<-as.numeric(rownames(all.data))
all.data$season<-as.factor(all.data$season)
all.data$week<-as.factor(all.data$week)
table(all.data$season,all.data$week) #min: 22, max: 30
margin.table(table(all.data$season,all.data$week),1) #by year; min: 466, max: 494
margin.table(table(all.data$season,all.data$week),2) #by week; min: 288, max: 324

#train/test/validate
#50/30/20
#2629/1577/1052
#from each season-
#239/143 (.36)/95 (.63)

set.seed(4141993)
train<-stratified(all.data,"season",239) #dim: 2629 x 14
train.rows<-sort(train$id) 
margin.table(table(train$season,train$week),1)
margin.table(table(train$season,train$week),2)

leftover<-all.data[-train.rows,]
margin.table(table(leftover$season,leftover$week),1)
margin.table(table(leftover$season,leftover$week),2)

set.seed(4141993)
test<-stratified(leftover,"season",143) #dim: 1573 x 14
test.rows<-sort(test$id)
margin.table(table(test$season,test$week),1)
margin.table(table(test$season,test$week),2)

valid<-all.data[-c(test.rows,train.rows),] #dim: 1056 x 14
margin.table(table(valid$season,valid$week),1)
margin.table(table(valid$season,valid$week),2)


write.csv(train,"trainingdata.csv")
write.csv(test,"testingdata.csv")
write.csv(valid,"validationdata.csv")





