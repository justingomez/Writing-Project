#cleaner modeling
library(corrplot)
library(randomForest)
library(rpart)
library(car)
library(mgcv)
library(gbm)
library(psych)

train<-read.csv("trainingdata.csv")[,-c(1,15)]
train$compp<-train$comp/train$att
train$result<-factor(train$result,levels=c("T","L","W"))

test<-read.csv("testingdata.csv")[,-c(1,15)]
test$compp<-test$comp/test$att
test$result<-factor(test$result,levels=c("T","L","W"))

c<-cor(train[,c(4,6:12)])

col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F"))  

corrplot.mixed(c,upper="square",title="Correlation Between Variables",mar=c(2,0,1,0),
               cl.ratio=.2,cl.align.text="l",cl.offset=.3,cl.length=11,cl.cex=.9,
               outline=TRUE,addgrid.col="black",number.cex=1,col=col4(10),tl.cex=.9)

c2<-cor(train[,c(4,8:12,14)])
corrplot.mixed(c2,upper="square",title="Correlation Between Variables",mar=c(2,0,1,0),
               cl.ratio=.2,cl.align.text="l",cl.offset=.3,cl.length=11,cl.cex=.9,
               outline=TRUE,addgrid.col="black",number.cex=1,col=col4(10),tl.cex=.75)

par(mfrow=c(2,3))
plot(train$qbr~train$compp,main="Completion Percentage",ylab="QBR",xlab="Completion Percentage")
titles<-c("Yards","Touchdowns","Interceptions","Sacks","Fumbles")
for(i in 8:12) {
   plot(train$qbr~train[,i],main=titles[i-7],xlab=titles[i-7],ylab="QBR")
}

mod1<-qbr~comp+att+yds+td+int+sack+fum
mod2<-qbr~compp+yds+td+int+sack+fum
mod3<-qbr~comp+att+yds+td+int+sack+fum+result
mod4<-qbr~compp+yds+td+int+sack+fum+result
mod5<-qbr~compp+yds+td+int+sack+fum+td*int+td*sack+td*fum+int*sack+int*fum+sack*fum
#mod5b<-qbr~compp+yds+td+int+sack+fum+int*sack+int*fum+sack*fum
#this<-qbr~compp+yds+td+int+sack+fum+yds*td+yds*int+yds*sack+yds*fum+td*int+td*sack+td*fum+int*sack+int*fum+sack*fum

#lms
lm1<-lm(mod1,data=train)
lm2<-lm(mod2,data=train)
lm3<-lm(mod3,data=train)
lm4<-lm(mod4,data=train)
lm5<-lm(mod5,data=train)
#lm5b<-lm(mod5b,data=train)
#lmthis<-lm(this,data=train)

#lm rmses
lm.rmse1<-sqrt((sum(predict(lm1,newdata=test)-test$qbr)^2)/nrow(test))
lm.rmse2<-sqrt((sum(predict(lm2,newdata=test)-test$qbr)^2)/nrow(test))
lm.rmse3<-sqrt((sum(predict(lm3,newdata=test)-test$qbr)^2)/nrow(test))
lm.rmse4<-sqrt((sum(predict(lm4,newdata=test)-test$qbr)^2)/nrow(test))
lm.rmse5<-sqrt((sum(predict(lm5,newdata=test)-test$qbr)^2)/nrow(test))
#lm.rmse5b<-sqrt((sum(predict(lm5b,newdata=test)-test$qbr)^2)/nrow(test))
#lm.rmsethis<-sqrt((sum(predict(lmthis,newdata=test)-test$qbr)^2)/nrow(test))


#bagged lms
n<-1000
#mod1
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141993)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train),replace=TRUE)
  boot<-train[these,]
  lm.boot<-lm(mod1,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse1<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))
#mod2
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141994)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train),replace=TRUE)
  boot<-train[these,]
  lm.boot<-lm(mod2,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse2<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))
#mod3
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141995)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  lm.boot<-lm(mod3,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse3<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))
#mod4
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141996)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train),replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  lm.boot<-lm(mod4,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse4<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))
#mod5
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141997)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train),replace=TRUE)
  boot<-train[these,]
  lm.boot<-lm(mod5,data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse5<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))


#gams
gam1<-gam(qbr~s(compp,k=10,bs="ts")+s(att,k=15,bs="ts")+
            s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+s(int,k=3,bs="ts")+
            s(sack,k=3,bs="ts")+fum,data=train)
gam2<-gam(qbr~result+s(compp,k=10,bs="ts")+s(att,k=15,bs="ts")+
            s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+s(int,k=3,bs="ts")+
            s(sack,k=3,bs="ts")+fum,data=train)
#plot(gam1,pages=2,scale=0)
#summary(gam1)
#gam.check(gam1)
#plot(gam2,pages=2,scale=0)
#summary(gam2)
#gam.check(gam2)

gam.rmse1<-sqrt((sum(predict(gam1,newdata=test)-test$qbr)^2)/nrow(test))
gam.rmse2<-sqrt((sum(predict(gam2,newdata=test)-test$qbr)^2)/nrow(test))

#bagged gams
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141997)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train),replace=TRUE)
  boot<-train[these,]
  gam.boot<-gam(qbr~s(compp,k=10,bs="ts")+s(att,k=15,bs="ts")+
                 s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+s(int,k=3,bs="ts")+
                 s(sack,k=3,bs="ts")+fum,data=boot)
  boot.pred[,i]<-predict(gam.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
gam.boot.rmse1<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))

boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141994)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train)-1,replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  gam.boot<-gam(qbr~result+s(compp,k=10,bs="ts")+s(att,k=15,bs="ts")+
                 s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+s(int,k=3,bs="ts")+
                 s(sack,k=3,bs="ts")+fum,data=boot)
  boot.pred[,i]<-predict(gam.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
gam.boot.rmse2<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))

#trees
tree1<-rpart(mod1,data=train,maxdepth=5)
tree2<-rpart(mod2,data=train,maxdepth=5)
tree3<-rpart(mod3,data=train,maxdepth=5)
tree4<-rpart(mod4,data=train,maxdepth=5)

plot(tree1,uniform=TRUE,margin=.1,main="Regression Tree for Model 1")
text(tree1,cex=.9)

tree.rmse1<-sqrt((sum(predict(tree1,newdata=test)-test$qbr)^2)/nrow(test))
tree.rmse2<-sqrt((sum(predict(tree2,newdata=test)-test$qbr)^2)/nrow(test))
tree.rmse3<-sqrt((sum(predict(tree3,newdata=test)-test$qbr)^2)/nrow(test))
tree.rmse4<-sqrt((sum(predict(tree4,newdata=test)-test$qbr)^2)/nrow(test))

#boosting
set.seed(4141993)
gbm1<-gbm(mod1,data=train,distribution="gaussian",n.trees=1000,interaction.depth=5
          ,shrinkage=.01)
gbm2<-gbm(mod2,data=train,distribution="gaussian",n.trees=1000,interaction.depth=5
          ,shrinkage=.01)
gbm3<-gbm(mod3,data=train,distribution="gaussian",n.trees=1000,interaction.depth=5
          ,shrinkage=.01)
gbm4<-gbm(mod4,data=train,distribution="gaussian",n.trees=1000,interaction.depth=5
          ,shrinkage=.01)

gbm.rmse1<-sqrt((sum(predict(gbm1,newdata=test,n.trees=1000,type="response")
                     -test$qbr)^2)/nrow(test))
gbm.rmse2<-sqrt((sum(predict(gbm2,newdata=test,n.trees=1000,type="response")
                     -test$qbr)^2)/nrow(test))
gbm.rmse3<-sqrt((sum(predict(gbm3,newdata=test,n.trees=1000,type="response")
                     -test$qbr)^2)/nrow(test))
gbm.rmse4<-sqrt((sum(predict(gbm4,newdata=test,n.trees=1000,type="response")
                     -test$qbr)^2)/nrow(test))

#bagging
#mod1
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141993)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train),replace=TRUE)
  boot<-train[these,]
  tree.boot<-rpart(mod1,data=boot,maxdepth=5)
  boot.pred[,i]<-predict(tree.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
tree.boot.rmse1<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))
#mod2
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141994)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train),replace=TRUE)
  boot<-train[these,]
  tree.boot<-rpart(mod2,data=boot,maxdepth=5)
  boot.pred[,i]<-predict(tree.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
tree.boot.rmse2<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))
#mod3
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141995)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train),replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  tree.boot<-rpart(mod3,data=boot,maxdepth=5)
  boot.pred[,i]<-predict(tree.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
tree.boot.rmse3<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))
#mod4
boot.pred<-matrix(12,ncol=n,nrow=nrow(test))
set.seed(4141996)
for (i in 1:n) {
  these<-sample(rownames(train),nrow(train),replace=TRUE)
  boot<-train[c(these,2463),]
  boot$result<-factor(boot$result,levels=c("T","L","W"))
  tree.boot<-rpart(mod4,data=boot,maxdepth=5)
  boot.pred[,i]<-predict(tree.boot,newdata=test)
}
mean.pred<-rep(12,nrow(test))
for(i in 1:nrow(test)){
  mean.pred[i]<-mean(boot.pred[i,])
}
tree.boot.rmse4<-sqrt((sum(mean.pred-test$qbr)^2)/nrow(test))

#random forests
set.seed(4141993)
forest1<-randomForest(mod1,data=train,ntree=1000)
forest2<-randomForest(mod2,data=train,ntree=1000)
forest3<-randomForest(mod3,data=train,ntree=1000)
forest4<-randomForest(mod4,data=train,ntree=1000)

forest.rmse1<-sqrt((sum(predict(forest1,newdata=test)-test$qbr)^2)/nrow(test))
forest.rmse2<-sqrt((sum(predict(forest2,newdata=test)-test$qbr)^2)/nrow(test))
forest.rmse3<-sqrt((sum(predict(forest3,newdata=test)-test$qbr)^2)/nrow(test))
forest.rmse4<-sqrt((sum(predict(forest4,newdata=test)-test$qbr)^2)/nrow(test))






