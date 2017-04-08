#new models chosen by AIC
library(corrplot)
library(randomForest)
library(rpart)
library(car)
library(mgcv)
library(gbm)
library(rattle)
library(MuMIn)
library(caret)

train<-read.csv("trainingdata.csv")[,-c(1,15)]
train$compp<-train$comp/train$att
train$result<-factor(train$result,levels=c("T","L","W"))

test<-read.csv("testingdata.csv")[,-c(1,15)]
test$compp<-test$comp/test$att
test$result<-factor(test$result,levels=c("T","L","W"))

oop<-options(na.action = "na.fail")
#best interaction models
mod.global<-lm(qbr~.+result*yds+result*td+result*int+result*sack+result*fum+result*compp+
                 yds*td+yds*int+yds*sack+yds*fum+yds*compp+td*int+td*sack+td*fum+td*compp+
                 int*sack+int*fum+int*compp+sack*fum+sack*compp+fum*compp
                 ,data=train[-c(1,2,3,6,7,13)])
d<-pdredge(mod.global,m.lim=c(7,13),fixed=~yds+td+int+sack+fum+compp,trace=TRUE)

#best additive models
mod.global1<-lm(qbr~.,data=train[-c(1,2,3,6,7,13)])
d1<-pdredge(mod.global1,trace=TRUE)

#or just specify my own models and check AIC...
mod1<-qbr~compp+fum+int+sack+td+yds+result
mod2<-qbr~compp+fum+int+sack+td+yds
mod3<-qbr~compp+int+td+yds
mod4<-qbr~compp+fum+int+sack+td+yds+result+compp:fum+compp:int+compp:td
mod5<-qbr~compp+fum+int+sack+td+yds+result+sack:result+sack:td+sack:int
mod6<-qbr~compp+fum+int+sack+td+yds+result+yds:td+yds:int+yds:fum

#six models we will use
mod1<-qbr~compp+fum+int+result+sack+td+yds
mod2<-qbr~compp+fum+int+sack+td+yds
mod3<-qbr~compp+int+sack+td+yds
mod4<-qbr~compp+fum+int+result+sack+td+yds+compp*sack+fum*int+fum*sack+int*sack+sack*yds+td*yds
mod5<-qbr~compp+fum+int+result+sack+td+yds+fum*result+fum*int+fum*sack+int*sack+td*yds
mod6<-qbr~compp+fum+int+result+sack+td+yds+compp*yds+fum*int+fum*sack+sack*yds+td*yds



#new fitted lms
lm1<-lm(mod1,data=train)
lm2<-lm(mod2,data=train)
lm3<-lm(mod3,data=train)
lm4<-lm(mod4,data=train)
lm5<-lm(mod5,data=train)
lm6<-lm(mod6,data=train)
#AIC comparison
aic<-AIC(lm1,lm2,lm3,lm4,lm5,lm6)[,2]
name<-c("Model 1","Model 2","Model 3","Model 4","Model 5","Model 6")
s<-order(aic)
name<-name[s]
m<-min(aic)
del<-round(aic[s]-m,2)
tab.aic<-data.frame(Model=name,AIC=aic,delta=del)

#lm RMSE
rmse<-function(a){
  error<-a-test$qbr
  sqrt(mean(error^2))
}
lm.rmse1<-rmse(predict(lm1,newdata=test,type="response"))
lm.rmse2<-rmse(predict(lm2,newdata=test,type="response"))
lm.rmse3<-rmse(predict(lm3,newdata=test,type="response"))
lm.rmse4<-rmse(predict(lm4,newdata=test,type="response"))
lm.rmse5<-rmse(predict(lm5,newdata=test,type="response"))
lm.rmse6<-rmse(predict(lm6,newdata=test,type="response"))

#bagged lms
n<-1000
#mod1
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141993)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  lm.boot<-lm(mod1,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test,type="response")
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse1<-rmse(mean.pred)
#mod2
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141994)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  lm.boot<-lm(mod2,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test,type="response")
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse2<-rmse(mean.pred)
#mod3
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141995)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  lm.boot<-lm(mod3,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test,type="response")
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse3<-rmse(mean.pred)
#mod4
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141996)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  lm.boot<-lm(mod4,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test,type="response")
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse4<-rmse(mean.pred)
#mod5
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141997)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  lm.boot<-lm(mod5,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test,type="response")
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse5<-rmse(mean.pred)
#mod6
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141997)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  lm.boot<-lm(mod6,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test,type="response")
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse6<-rmse(mean.pred)

#fitted gams
gam1<-gam(qbr~s(compp,k=10,bs="ts")+s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+
            s(int,k=3,bs="ts")+s(sack,k=3,bs="ts")+fum+result,data=train)
gam2<-gam(qbr~s(compp,k=10,bs="ts")+s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+
            s(int,k=3,bs="ts")+s(sack,k=3,bs="ts")+fum,data=train)
gam3<-gam(qbr~s(compp,k=10,bs="ts")+s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+
            s(int,k=3,bs="ts"),data=train)

#gam rmse
gam.rmse1<-rmse(predict(gam1,newdata=test),type="response")
gam.rmse2<-rmse(predict(gam2,newdata=test),type="response")
gam.rmse3<-rmse(predict(gam3,newdata=test),type="response")

#bagged gam
#gam1
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141997)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  gam.boot<-gam(qbr~s(compp,k=10,bs="ts")+s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+
                  s(int,k=3,bs="ts")+s(sack,k=3,bs="ts")+fum+result,data=boot)
  boot.pred[,i]<-predict(gam.boot,newdata=test,type="response")
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
gam.boot.rmse1<-rmse(mean.pred)
#gam2
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141997)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  gam.boot<-gam(qbr~s(compp,k=10,bs="ts")+s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+
                  s(int,k=3,bs="ts")+s(sack,k=3,bs="ts")+fum,data=boot)
  boot.pred[,i]<-predict(gam.boot,newdata=test,type="response")
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
gam.boot.rmse2<-rmse(mean.pred)
#gam3
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141997)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  gam.boot<-gam(qbr~s(compp,k=10,bs="ts")+s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+
                  s(int,k=3,bs="ts"),data=boot)
  boot.pred[,i]<-predict(gam.boot,newdata=test,type="response")
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
gam.boot.rmse3<-rmse(mean.pred)

#trees
tree1<-rpart(mod1,data=train,method="anova")
plotcp(tree1)
tree1.prune<-prune(tree1,cp=0.014)
tree2<-rpart(mod2,data=train,method="anova")
plotcp(tree2)
tree2.prune<-prune(tree2,cp=0.0172)
tree3<-rpart(mod3,data=train,method="anova")
plotcp(tree3)
tree3.prune<-prune(tree3,cp=0.0172)

#pruned trees rmse
tree.rmse1<-rmse(predict(tree1.prune,newdata=test),type="response")
tree.rmse2<-rmse(predict(tree2.prune,newdata=test),type="response")
tree.rmse3<-rmse(predict(tree3.prune,newdata=test),type="response")

#tree visualization
fancyRpartPlot(tree1.prune)
fancyRpartPlot(tree2.prune)

#variable importance (trees)
plot(tree1.prune$variable.importance)
plot(tree2.prune$variable.importance)
plot(tree3.prune$variable.importance)

#boosting (trees)
set.seed(4141993)
gbm1<-gbm(mod1,data=train,distribution="gaussian",n.trees=1000,interaction.depth=2
          ,shrinkage=.01)
set.seed(4141994)
gbm2<-gbm(mod2,data=train,distribution="gaussian",n.trees=1000,interaction.depth=2
          ,shrinkage=.01)
set.seed(4141995)
gbm3<-gbm(mod3,data=train,distribution="gaussian",n.trees=1000,interaction.depth=2
          ,shrinkage=.01)

gbm.rmse1<-rmse(predict(gbm1,newdata=test,n.trees=1000,type="response"))
gbm.rmse2<-rmse(predict(gbm2,newdata=test,n.trees=1000,type="response"))
gbm.rmse3<-rmse(predict(gbm3,newdata=test,n.trees=1000,type="response"))

#bagging (trees)
#mod1
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141993)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  tree.boot<-rpart(mod1,data=boot)
  boot.pred[,i]<-predict(tree.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
tree.boot.rmse1<-rmse(mean.pred)
#mod2
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141994)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  tree.boot<-rpart(mod2,data=boot)
  boot.pred[,i]<-predict(tree.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
tree.boot.rmse2<-rmse(mean.pred)
#mod3
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141995)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  tree.boot<-rpart(mod3,data=boot)
  boot.pred[,i]<-predict(tree.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
tree.boot.rmse3<-rmse(mean.pred)

#random forest
set.seed(4141993)
forest1<-randomForest(mod1,data=train,ntree=1000,importance=TRUE)
set.seed(4141994)
forest2<-randomForest(mod2,data=train,ntree=1000,importance=TRUE)
set.seed(4141995)
forest3<-randomForest(mod3,data=train,ntree=1000,importance=TRUE)

forest.rmse1<-rmse(predict(forest1,newdata=test),type="response")
forest.rmse2<-rmse(predict(forest2,newdata=test),type="response")
forest.rmse3<-rmse(predict(forest3,newdata=test),type="response")

#variable importance (forest)
varImpPlot(forest1)
varImpPlot(forest2)
varImpPlot(forest3)

#partial plots
#forest 1
par(mfrow=c(3,1))
partialPlot(forest1,pred.data=train,x.var=int)
partialPlot(forest1,pred.data=train,x.var=compp)
partialPlot(forest1,pred.data=train,x.var=td)

#forest 2
partialPlot(forest2,pred.data=train,x.var=int)
partialPlot(forest2,pred.data=train,x.var=td)
partialPlot(forest2,pred.data=train,x.var=compp)

#forest 3
partialPlot(forest3,pred.data=train,x.var=int)
partialPlot(forest3,pred.data=train,x.var=td)
partialPlot(forest3,pred.data=train,x.var=compp)















