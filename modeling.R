library(randomForest)
library(rpart)

train<-read.csv("trainingdata.csv")[,-c(1,15)]
train$compp<-train$comp/train$att
train$result<-factor(train$result,levels=c("T","L","W"))

test<-read.csv("testingdata.csv")[,-c(1,15)]
test$compp<-test$comp/test$att
test$result<-factor(test$result,levels=c("T","L","W"))

#lm
nfl.lm1<-lm(qbr~comp+att+yds+td+int+sack+fum,data=train)
nfl.lm2<-lm(qbr~compp+yds+td+int+sack+fum,data=train)
nfl.lm3<-lm(qbr~comp+att+yds+td+int+sack+fum+result,data=train)
nfl.lm4<-lm(qbr~compp+yds+td+int+sack+fum+result,data=train)
lm.pred1<-predict(nfl.lm1,newdata=test)
lm.rmse1<-sqrt((sum(lm.pred1-test$qbr)^2)/dim(test)[1]) #20.32
lm.pred2<-predict(nfl.lm2,newdata=test)
lm.rmse2<-sqrt((sum(lm.pred2-test$qbr)^2)/dim(test)[1]) #16.05
lm.pred3<-predict(nfl.lm3,newdata=test)
lm.rmse3<-sqrt((sum(lm.pred3-test$qbr)^2)/dim(test)[1]) #23.78
lm.pred4<-predict(nfl.lm4,newdata=test)
lm.rmse4<-sqrt((sum(lm.pred4-test$qbr)^2)/dim(test)[1]) #20.77

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
