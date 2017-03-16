#quick salary investigation
library(rvest)

spotrac <- read_html(paste("http://www.spotrac.com/nfl/contracts/"))
all.players<- data.frame(pos=spotrac %>% html_nodes("td:nth-child(2)") %>% html_text() 
                         %>% .[1:2112], avg=spotrac %>% html_nodes("td:nth-child(7)") 
                         %>% html_text() %>% .[1:2112])
all.players$avg<-lapply(all.players$avg, function(x) gsub("[,$]", "", x))
all.players$avg<-as.numeric(all.players$avg)/1000000
u<-unique(all.players$pos) #22 positions
m<-rep(12,22)
for (i in 1:22) {
  m[i]<-mean(all.players$avg[all.players$pos==u[i]])
}
o1<-order(m,decreasing=TRUE)
o2<-order(m,decreasing=FALSE)
tab<-data.frame(Position=u[o1],Average=m[o1])
plot(1:22,m[o2],xlab="Player Position",ylab="Mean Salary (in Millions)",xaxt="n",
     main="Mean Salary by Player Position",type="l",xlim=c(1,22))
ticks<-seq(1,22,1)
tck<-axis(side=1,at=ticks,labels=FALSE)
text(tck,par("usr")[3],labels=u[o2],srt=0,xpd=TRUE,cex=.8,adj=c(.5,2))
#saved as salary

m2<-rep(12,22)
for (i in 1:22) {
  m2[i]<-median(all.players$avg[all.players$pos==u[i]])
}
o11<-order(m2,decreasing=TRUE)
o22<-order(m2,decreasing=FALSE)
tab2<-data.frame(Position=u[o11],Median=m2[o11])
plot(1:22,m2[o22],xlab="Player Position",ylab="Median Salary (in Millions)",xaxt="n",
     main="Median Salary by Player Position",type="l",xlim=c(1,22))
ticks<-seq(1,22,1)
tck<-axis(side=1,at=ticks,labels=FALSE)
text(tck,par("usr")[3],labels=u[o22],srt=0,xpd=TRUE,cex=.8,adj=c(.5,2))