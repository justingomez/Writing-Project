setwd("~/Writing Project")
library(randomForest)

nfl<-read.csv("trainingdata.csv")[,-c(1,15)]

#lm