library(corrplot)
library(randomForest)
library(rpart)
library(car)
library(mgcv)
library(gbm)
library(rattle)
library(MuMIn)
library(caret)
library(beanplot)
#read in train and test
train<-read.csv("trainingdata.csv")[,-c(1,15)]
train$compp<-train$comp/train$att
train$result<-factor(train$result,levels=c("T","L","W"))

test<-read.csv("testingdata.csv")[,-c(1,15)]
test$compp<-test$comp/test$att
test$result<-factor(test$result,levels=c("T","L","W"))

#models
mod1<-qbr~compp+fum+int+sack+td+yds+result
mod2<-qbr~compp+fum+int+sack+td+yds
mod3<-qbr~compp+int+td+yds
mod4<-qbr~compp+fum+int+sack+td+yds+result+compp:fum+compp:int+compp:td
mod5<-qbr~compp+fum+int+sack+td+yds+result+sack:result+sack:td+sack:int
mod6<-qbr~compp+fum+int+sack+td+yds+result+yds:td+yds:int+yds:fum

#fitted lms
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
set.seed(4141998)
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
gam1<-gam(qbr~s(compp,k=3,bs="ts")+s(yds,k=3,bs="ts")+s(td,k=3,bs="ts")+
            s(int,k=3,bs="ts")+s(sack,k=3,bs="ts")+fum+result,data=train)
gam2<-gam(qbr~s(compp,k=3,bs="ts")+s(yds,k=3,bs="ts")+s(td,k=3,bs="ts")+
            s(int,k=3,bs="ts")+s(sack,k=3,bs="ts")+fum,data=train)
gam3<-gam(qbr~s(compp,k=3,bs="ts")+s(yds,k=3,bs="ts")+s(td,k=3,bs="ts")+
            s(int,k=3,bs="ts"),data=train)

#gam rmse
gam.rmse1<-rmse(predict(gam1,newdata=test,type="response"))
gam.rmse2<-rmse(predict(gam2,newdata=test,type="response"))
gam.rmse3<-rmse(predict(gam3,newdata=test,type="response"))

#bagged gam
#gam1
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141993)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  gam.boot<-gam(qbr~s(compp,k=3,bs="ts")+s(yds,k=3,bs="ts")+s(td,k=3,bs="ts")+
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
set.seed(4141994)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  gam.boot<-gam(qbr~s(compp,k=3,bs="ts")+s(yds,k=3,bs="ts")+s(td,k=3,bs="ts")+
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
set.seed(4141995)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  gam.boot<-gam(qbr~s(compp,k=3,bs="ts")+s(yds,k=3,bs="ts")+s(td,k=3,bs="ts")+
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
tree1.prune<-prune(tree1,cp=0.019)
tree2<-rpart(mod2,data=train,method="anova")
plotcp(tree2)
tree2.prune<-prune(tree2,cp=0.0172)
tree3<-rpart(mod3,data=train,method="anova")
plotcp(tree3)
tree3.prune<-prune(tree3,cp=0.0172)

#pruned trees rmse
tree.rmse1<-rmse(predict(tree1.prune,newdata=test))
tree.rmse2<-rmse(predict(tree2.prune,newdata=test))
tree.rmse3<-rmse(predict(tree3.prune,newdata=test))

#tree visualization
fancyRpartPlot(tree1.prune,sub="")
fancyRpartPlot(tree2.prune,sub="")

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
plot(forest1) #only need 100
forest1.new<-randomForest(mod1,data=train,ntree=100,importance=TRUE)
plot(forest1.new)
set.seed(4141994)
forest2<-randomForest(mod2,data=train,ntree=1000,importance=TRUE)
plot(forest2) #100 trees
forest2.new<-randomForest(mod2,data=train,ntree=100,importance=TRUE)
plot(forest2.new)
set.seed(4141995)
forest3<-randomForest(mod3,data=train,ntree=1000,importance=TRUE)
plot(forest3) #same
forest3.new<-randomForest(mod3,data=train,ntree=100,importance=TRUE)
plot(forest3.new)

forest.rmse1<-rmse(predict(forest1,newdata=test,type="response"))
forest.rmse1.new<-rmse(predict(forest1.new,newdata=test,type="response"))
forest.rmse2<-rmse(predict(forest2,newdata=test,type="response"))
forest.rmse2.new<-rmse(predict(forest2.new,newdata=test,type="response"))
forest.rmse3<-rmse(predict(forest3,newdata=test,type="response"))
forest.rmse3.new<-rmse(predict(forest3.new,newdata=test,type="response"))

#checking
set.seed(4141993)
forest1<-randomForest(mod1,data=train,ntree=1000,importance=TRUE,do.trace=500)
#rmse is stil about 16.49, so we might be ok...

plot(y=test$qbr,x=predict(forest1,newdata=test,type="response"))
abline(a=0,b=1,col="red",lwd=3)
#evidence that we're ok...

lm.test<-lm(qbr~1,data=train)
pred.test<-predict(lm.test,newdata=test)
rmse(pred.test)

#variable importance (forest)
varImpPlot(forest1.new)
varImpPlot(forest2.new)
varImpPlot(forest3.new)

#partial plots
#forest 1
par(mfrow=c(3,1))
partialPlot(forest1.new,pred.data=train,x.var=int)
partialPlot(forest1.new,pred.data=train,x.var=compp)
partialPlot(forest1.new,pred.data=train,x.var=td)

#forest 2
partialPlot(forest2.new,pred.data=train,x.var=int)
partialPlot(forest2.new,pred.data=train,x.var=td)
partialPlot(forest2.new,pred.data=train,x.var=compp)

#forest 3
partialPlot(forest3.new,pred.data=train,x.var=int)
partialPlot(forest3.new,pred.data=train,x.var=td)
partialPlot(forest3.new,pred.data=train,x.var=compp)

#graphics for pres
beanplot(train$qbr~as.factor(train$td),col=c(1,2,3,4))
mtext("Number of Touchdowns",side=1,cex=1.5,line=2.5)
mtext("Total QBR",side=2,cex=1.5,line=2.5)
         
####### example tree and partition space ########
#base tree
mod.ex<-qbr~compp+td
tree.ex1<-rpart(mod.ex,data=train,method="anova",model=TRUE)
tree.prune.ex1<-prune(tree.ex1,cp=0.016)
fancyRpartPlot(tree.prune.ex1,sub="")
#rmse func
rmse<-function(a){
  error<-a-test$qbr
  sqrt(mean(error^2))
}

base.ex<-rmse(predict(tree.prune.ex1,newdata=test))

#mean only model
lm.test<-lm(qbr~1,data=train)
pred.test<-predict(lm.test,newdata=test)
rmse(pred.test)

library(tree)
tree.ex<-tree(mod.ex,data=train,model=TRUE)
partition.tree(tree.ex)
tree.prune<-prune.tree(tree.ex,best=6)
partition.tree(tree.prune,main="Partitioned Predictor Space")

#boosted tree
set.seed(4141993)
gbm.ex<-gbm(mod.ex,data=train,distribution="gaussian",n.trees=1000,interaction.depth=2
          ,shrinkage=.01)
boost.ex<-rmse(predict(gbm.ex,newdata=test,n.trees=1000,type="response"))

#bagged tree
n<-1000
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141993)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  tree.boot<-rpart(mod.ex,data=boot)
  boot.pred[,i]<-predict(tree.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
bagged.ex<-rmse(mean.pred)

plot(train$qbr~train$compp,xlab="Completion Percentage",ylab="QBR",xlim=c(0.3,1),ylim=c(0,100))
plot(train$qbr~train$td,xlab="Number of Touchdowns",ylab="QBR",ylim=c(0,100))

#boosting graphic
x<-seq(0,5,.05)
sim.y<-cos(5*x)
plot(sim.y~x,type="l",xlab="",ylab="",lwd=2)
set.seed(04141993)
o<-sort(sample.int(101,size=50))
dat.x<-x[o]
dat.y<-cos(5*dat.x)
points(dat.x,dat.y,lwd=2)
mtext("Underlying truth: y=cos(x)",side=3,cex=2)


par(mfrow=c(2,3))
boost1<-rpart(dat.y~dat.x,method="anova")
resid1<-residuals(boost1)
plot(dat.x,resid1,col="red",lwd=2,ylim=c(-1,1),main="Step 1",xlab="",ylab="")
lines(dat.x,predict(boost1),lwd=2)
mtext("x",side=1,cex=1,line=2.5)
mtext("Residuals",side=2,cex=1,line=2.5)


boost2<-rpart(resid1~dat.x,method="anova")
resid2<-residuals(boost2)
plot(dat.x,resid2,col="blue",lwd=2,ylim=c(-1,1),main="Step 2",xlab="",ylab="")
lines(dat.x,predict(boost2),lwd=2)
mtext("x",side=1,cex=1,line=2.5)
mtext("Residuals",side=2,cex=1,line=2.5)

boost3<-rpart(resid2~resid1+dat.x,method="anova")
resid3<-residuals(boost3)
plot(dat.x,resid3,col="green",lwd=2,ylim=c(-1,1),main="Step 3",xlab="",ylab="")
lines(dat.x,predict(boost3),lwd=2)
mtext("x",side=1,cex=1,line=2.5)
mtext("Residuals",side=2,cex=1,line=2.5)

boost4<-rpart(resid3~resid1+resid2+dat.x,method="anova")
resid4<-residuals(boost4)
plot(dat.x,resid4,col="hotpink",lwd=2,ylim=c(-1,1),main="Step 4",xlab="",ylab="")
lines(dat.x,predict(boost4),lwd=2)
mtext("x",side=1,cex=1,line=2.5)
mtext("Residuals",side=2,cex=1,line=2.5)

boost5<-rpart(resid4~resid1+resid2+resid3+dat.x,method="anova")
resid5<-residuals(boost5)
plot(dat.x,resid5,col="orange",lwd=2,ylim=c(-1,1),main="Step 5",xlab="",ylab="")
lines(dat.x,predict(boost5),lwd=2)
mtext("x",side=1,cex=1,line=2.5)
mtext("Residuals",side=2,cex=1,line=2.5)

boost6<-rpart(resid5~resid1+resid2+resid3+resid4+dat.x,method="anova")
resid6<-residuals(boost6)
plot(dat.x,resid6,col="gold",lwd=2,ylim=c(-1,1),main="Step 6",xlab="",ylab="")
lines(dat.x,predict(boost6),lwd=2)
mtext("x",side=1,cex=1,line=2.5)
mtext("Residuals",side=2,cex=1,line=2.5)
par(mfrow=c(1,1))

plot(sim.y~x,type="l",xlab="",ylab="",lwd=2)
lines(dat.x,predict(boost1)+predict(boost2)+predict(boost3)+
        predict(boost4)+predict(boost5)+predict(boost6),col="red",lwd=4,lty=2)
points(dat.x,dat.y,lwd=4)
mtext("Combined trees",side=3,cex=2)

par(mfrow=c(2,2))
plot(resid(lm6)~fitted(lm6),xlab="Fitted Values",ylab="Residuals",main="Top Predicting Linear Model: Model 6")

plot(resid(gam1)~fitted(gam1),xlab="Fitted Values",ylab="Residuals",main="Top Predicting GAM: Model 1")

plot(train$qbr-predict(tree1.prune)~predict(tree1.prune),xlab="Fitted Values",ylab="Residuals",main="Top Predictng Tree: Model 1")

plot(train$qbr-predict(forest1.new)~predict(forest1.new),xlab="Fitted Values",ylab="Residuals",main="Top Predictng Forest: Model 1")
par(mfrow=c(1,1))

valid<-read.csv("validationdata.csv")[,-c(1,15)]
valid$compp<-valid$comp/valid$att
valid$result<-factor(valid$result,levels=c("T","L","W"))

rmse<-function(a){
   error<-a-valid$qbr
   sqrt(mean(error^2))
}
finally<-rmse(predict(lm6,newdata=valid))
plot(valid$qbr-predict(lm6,newdata=valid)~predict(lm6,newdata=valid),xlab="Predicted Values",ylab="Residuals")
