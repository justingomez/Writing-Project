#cleaner modeling
library(corrplot)
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

c<-cor(train[,c(4,6:12)])

col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
                           "cyan", "#007FFF", "blue","#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))  
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F"))  

corrplot.mixed(c,upper="square",title="Correlation Between Variables",mar=c(2,0,1,0),
               cl.ratio=.2,cl.align.text="l",cl.offset=.3,cl.length=11,cl.cex=.9,
               outline=TRUE,addgrid.col="black",number.cex=1,col=col4(10),tl.cex=.9)

c2<-cor(train[,c(4,8:12,14)])
corrplot.mixed(c2,upper="square",title="Correlation Between Variables",mar=c(2,0,1,0),
               cl.ratio=.2,cl.align.text="l",cl.offset=.3,cl.length=11,cl.cex=.9,
               outline=TRUE,addgrid.col="black",number.cex=1,col=col4(10),tl.cex=.75)


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















