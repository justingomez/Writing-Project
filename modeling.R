library(randomForest)
library(rpart)
library(car)
library(mgcv)
library(gbm)

train<-read.csv("trainingdata.csv")[,-c(1,15)]
train$compp<-train$comp/train$att
train$result<-factor(train$result,levels=c("T","L","W"))

test<-read.csv("testingdata.csv")[,-c(1,15)]
test$compp<-test$comp/test$att
test$result<-factor(test$result,levels=c("T","L","W"))

#lm
nfl.lm1<-lm(qbr~comp+att+yds+td+int+sack+fum,data=train) #58% of variation explained
nfl.lm2<-lm(qbr~compp+yds+td+int+sack+fum,data=train) #58% of variation explained
nfl.lm3<-lm(qbr~comp+att+yds+td+int+sack+fum+result,data=train) #59%
nfl.lm4<-lm(qbr~compp+yds+td+int+sack+fum+result,data=train) #60%
nfl.lm5<-lm(qbr~compp+att+yds+td+int+sack+fum,data=train)  #59%
nfl.lm6<-lm(qbr~compp+yds+td+int+sack+fum+yds*td+yds*int+td*int+sack*fum,data=train) #60
lm.pred1<-predict(nfl.lm1,newdata=test)
lm.rmse1<-sqrt((sum(lm.pred1-test$qbr)^2)/dim(test)[1]) #20.32
lm.pred2<-predict(nfl.lm2,newdata=test)
lm.rmse2<-sqrt((sum(lm.pred2-test$qbr)^2)/dim(test)[1]) #16.05
lm.pred3<-predict(nfl.lm3,newdata=test)
lm.rmse3<-sqrt((sum(lm.pred3-test$qbr)^2)/dim(test)[1]) #23.78
lm.pred4<-predict(nfl.lm4,newdata=test)
lm.rmse4<-sqrt((sum(lm.pred4-test$qbr)^2)/dim(test)[1]) #20.77
lm.pred6<-predict(nfl.lm6,newdata=test)
lm.rmse6<-sqrt((sum(lm.pred6-test$qbr)^2)/dim(test)[1]) #11.26
AIC(nfl.lm1,nfl.lm2,nfl.lm3,nfl.lm4,nfl.lm5,nfl.lm6)

#lm diagnostics
par(mfrow=c(2,2))
sresid.1<-rstandard(nfl.lm1)
fitted.1<-fitted(nfl.lm1)
plot(fitted.1,sresid.1)
plot(nfl.lm1,which=c(2,3,4))

sresid.2<-rstandard(nfl.lm2)
fitted.2<-fitted(nfl.lm2)
plot(fitted.2,sresid.2)
plot(nfl.lm2,which=c(2,3,4))

sresid.3<-rstandard(nfl.lm3)
fitted.3<-fitted(nfl.lm3)
plot(fitted.3,sresid.3)
plot(nfl.lm3,which=c(2,3,4))

sresid.4<-rstandard(nfl.lm4)
fitted.4<-fitted(nfl.lm4)
plot(fitted.4,sresid.4)
plot(nfl.lm4,which=c(2,3,4))

sresid.6<-rstandard(nfl.lm6)
fitted.6<-fitted(nfl.lm6)
plot(fitted.6,sresid.6)
plot(nfl.lm6,which=c(2,3,4))

crPlots(nfl.lm1)
crPlots(nfl.lm2)
crPlots(nfl.lm3)
crPlots(nfl.lm4)
crPlots(nfl.lm5)

#trees
nfl.tree1<-rpart(qbr~comp+att+yds+td+int+sack+fum,data=train,method="anova")
nfl.tree2<-rpart(qbr~compp+yds+td+int+sack+fum,data=train,method="anova")
nfl.tree3<-rpart(qbr~comp+att+yds+td+int+sack+fum+result,data=train,method="anova")
nfl.tree4<-rpart(qbr~compp+yds+td+int+sack+fum+result,data=train,method="anova")
plot(nfl.tree1)
text(nfl.tree1)
tree.pred1<-predict(nfl.tree1,newdata=test)
tree.rmse1<-sqrt((sum(tree.pred1-test$qbr)^2)/dim(test)[1]) #23.44
tree.pred2<-predict(nfl.tree2,newdata=test)
tree.rmse2<-sqrt((sum(tree.pred2-test$qbr)^2)/dim(test)[1]) #12.17
tree.pred3<-predict(nfl.tree3,newdata=test)
tree.rmse3<-sqrt((sum(tree.pred3-test$qbr)^2)/dim(test)[1]) #31.59
tree.pred4<-predict(nfl.tree4,newdata=test)
tree.rmse4<-sqrt((sum(tree.pred4-test$qbr)^2)/dim(test)[1]) #28.26


set.seed(4141993)
nfl.forest1<-randomForest(qbr~comp+att+yds+td+int+sack+fum,data=train,ntree=1000)
nfl.forest2<-randomForest(qbr~compp+yds+td+int+sack+fum,data=train,ntree=1000)
nfl.forest3<-randomForest(qbr~comp+att+yds+td+int+sack+fum+result,data=train,
                          ntree=1000)
nfl.forest4<-randomForest(qbr~compp+yds+td+int+sack+fum+result,data=train,
                          ntree=1000)
plot(nfl.forest1)
forest.pred1<-predict(nfl.forest1,newdata=test)
forest.rmse1<-sqrt((sum(forest.pred1-test$qbr)^2)/dim(test)[1]) #6.04
forest.pred2<-predict(nfl.forest2,newdata=test)
forest.rmse2<-sqrt((sum(forest.pred2-test$qbr)^2)/dim(test)[1]) #1.57
forest.pred3<-predict(nfl.forest3,newdata=test)
forest.rmse3<-sqrt((sum(forest.pred3-test$qbr)^2)/dim(test)[1]) #15.95
forest.pred4<-predict(nfl.forest4,newdata=test)
forest.rmse4<-sqrt((sum(forest.pred4-test$qbr)^2)/dim(test)[1]) #8.37

#gam
gam1<-gam(qbr~result+s(compp,k=10,bs="ts")+s(att,k=15,bs="ts")+
            s(yds,k=15,bs="ts")+s(td,k=3,bs="ts")+s(int,k=3,bs="ts")+
            s(sack,k=3,bs="ts")+fum,data=train)
plot(gam1,pages=2,scale=0)
summary(gam1)
gam.check(gam1)


#bagging lm
n<-1000
boot.pred<-matrix(12,ncol=n,nrow=dim(test)[1])
for (i in 1:n) {
  these<-sample(rownames(train),dim(train)[1],replace=TRUE)
  boot<-train[these,]
  lm.boot<-lm(qbr~compp+yds+td+int+sack+fum+yds*td+yds*int+td*int+sack*fum,
              data=boot)
  boot.pred[,i]<-predict(lm.boot,newdata=test)
}
mean.pred<-rep(12,dim(test)[1])
for(i in 1:dim(test)[1]){
  mean.pred[i]<-mean(boot.pred[i,])
}
boot.rmse<-sqrt((sum(mean.pred-test$qbr)^2)/dim(test)[1]) #12.60 for 10; 15.92 for 1000; 16.09 for 100000
#previous #s were for lm2
#for lm6 (best) error is 11.08 for 1000; 11.14 for 10000

#boosting trees
try<-gbm(qbr~compp+yds+td+int+sack+fum,data=train,n.tree=1000,distribution="gaussian",
         cv.folds=2)
diagnostic<-gbm.perf(try)
plot(try)
