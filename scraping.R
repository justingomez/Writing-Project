library(rvest)
library(stringr)

#automate to grab each year 
all.qbr<-list()
for(i in 2006:2016){
  espn.qbr <- read_html(paste("http://www.espn.com/nfl/qbr/_/year/",i,sep=""))
  all.qbr[[i-2005]]<- data.frame(qb=str_split_fixed(espn.qbr %>% html_nodes("td:nth-child(2)") 
                               %>% html_text() %>% .[2:40],",",2)[,1],qbr=espn.qbr %>% html_nodes(
                                "td:nth-child(10)") %>% html_text() %>% .[2:40],stringsAsFactors=FALSE)
}

#clean up NA's
for(i in 1:11) {
  all.qbr[[i]]<-all.qbr[[i]][-c(which(is.na(all.qbr[[i]][,2]))),]
}

#get rid of unncessary rows
for(i in 1:11) {
  all.qbr[[i]]<-all.qbr[[i]][-which(all.qbr[[i]][,1]=="PLAYER"),]
}

#add year
for(i in 1:11) {
  year<-rep(i+2005,dim(all.qbr[[i]])[1])
  all.qbr[[i]]<-data.frame(qb=all.qbr[[i]][,1],season=year,qbr=all.qbr[[i]][,2])
}
#in all.qbr, 1=2006,...,11=2016


#Now apply to 2006-2016
all.stats<-list()
for(k in 2006:2016) {
  h<-c(1,41,81)
  test1<-list()
  for(i in unique(h)) {
    test1[[i]]<-read_html(paste("http://www.espn.com/nfl/statistics/player/_/stat/passing/sort/passingYards/year/",k,"/seasontype/2/qualified/false/count/",i,sep=""))
  }
  
  test2<-list()
  for(i in 1:14) {
    test2[[i]]<-test1[[1]] %>% html_nodes(paste("td:nth-child(",i,")")) %>% html_text() %>% .[1:50]
  }
  test3<-list()
  for(i in 1:14) {
    test3[[i]]<-test1[[41]] %>% html_nodes(paste("td:nth-child(",i,")")) %>% html_text() %>% .[1:50]
  }
  test4<-list()
  for(i in 1:14) {
    test4[[i]]<-test1[[81]] %>% html_nodes(paste("td:nth-child(",i,")")) %>% html_text() %>% .[1:50]
  }
  
  #now combine 3 pages of stats
  test5<-matrix(NA,nrow=50,ncol=14)
  for(i in 1:14) {
    for(j in 1:50) {
      test5[j,i]<-test2[[i]][j]
    }
  }
  test6<-matrix(NA,nrow=50,ncol=14)
  for(i in 1:14) {
    for(j in 1:50) {
      test6[j,i]<-test3[[i]][j]
    }
  }
  test7<-matrix(NA,nrow=50,ncol=14)
  for(i in 1:14) {
    for(j in 1:50) {
      test7[j,i]<-test4[[i]][j]
    }
  }
  test8<-as.data.frame(rbind(test5,test6,test7))
  colnames(test8)<-c("","player","team","comp","att","pct","yds","yds/a","long","td","int","sack","rate","yds/g")
  
  test8[,2]<-str_split_fixed(test8[,2],",",2)[,1]
  
  #get rid of unncessary rows/columns
  test8<-test8[-which(test8[,2]=="PLAYER"),]
  test8<-test8[,-1] #rank
  test8<-test8[-which(is.na(test8[,3])),] #NA's
  all.stats[[k-2005]]<-test8
}
#in all.stats: 1=2006,...,11=2016

#need to match qbr set with stats set

full.data<-list()
for(k in 1:11) {
  store<-rep(NA,dim(all.qbr[[k]])[1])
  for(i in 1:dim(all.qbr[[k]])[1]) {
    for(j in 1:dim(all.stats[[k]])[1]) {
      if(all.qbr[[k]][i,1]==all.stats[[k]][j,1]) {
        store[i]<-j
      }
    }
  }
  full.data[[k]]<-data.frame(all.qbr[[k]],all.stats[[k]][store,])
}

#clean it up a bit and add in season info
for(i in 1:11) {
  full.data[[i]]<-full.data[[i]][,-4]
}

#form into one large data frame

final<-as.data.frame(rbind(full.data[[1]],full.data[[2]],full.data[[3]],full.data[[4]],
                           full.data[[5]],full.data[[6]],full.data[[7]],full.data[[8]],
                           full.data[[9]],full.data[[10]],full.data[[11]]))

#done?
write.csv(final,"yearlynflstats.csv")
#when reading in, leave out the first column

##USED DATA BELOW


#all years, 2006-2016, weeks 1-17
all.weekly.qbr<-list()
for(j in 2006:2016) {
  standin<-list()
  for(i in 1:17){
    espn.qbr <- read_html(paste(
      "http://www.espn.com/nfl/qbr/_/year/",j,"/type/player-week/week/",i,sep=""))
    standin[[i]]<- data.frame(qb=str_split_fixed(espn.qbr %>% html_nodes("td:nth-child(2)") 
                                                        %>% html_text() %>% .[2:40],",",2)[,1],season=rep(j,39),week=rep(i,39),qbr=espn.qbr %>% html_nodes(
                                                          "td:nth-child(11)") %>% html_text() %>% .[2:40],stringsAsFactors=FALSE)
  }
  all.weekly.qbr[[j-2005]]<-standin
}

#clean up NA's
for(i in 1:11) {
  for(j in 1:17) {
    all.weekly.qbr[[i]][[j]]<-all.weekly.qbr[[i]][[j]][-c(which(is.na(
      all.weekly.qbr[[i]][[j]][,4]))),]
  }
}

#get rid of unnecessary rows
for(i in 1:11) {
  for(j in 1:17) {
    all.weekly.qbr[[i]][[j]]<-all.weekly.qbr[[i]][[j]][-which(
      all.weekly.qbr[[i]][[j]][,1]=="PLAYER"),]
  }
}

#in all.weekly.qbr, fist bracket: 1=2006,...,11=2016 second bracket: week #


#now for weekly stats...
all.weekly.stats<-list()
for(i in 2006:2016) {
  standin<-list()
  for(j in 1:17) {
    espn.stats<-read_html(paste(
      "http://www.espn.com/nfl/weekly/leaders/_/week/",j,"/year/",i,"/seasontype/2/type/passing",sep=""))
    standin[[j]]<-data.frame(qb=str_split_fixed(espn.stats %>% html_nodes("td:nth-child(2)")
                   %>% html_text() %>% .[23:52],",",2)[,1],season=rep(i,30),week=rep(j,30),
                   result=str_split_fixed(espn.stats %>% html_nodes("td:nth-child(4)") %>%
                   html_text() %>% .[3:32]," ",2)[,1],comp=espn.stats %>% html_nodes("td:nth-child(5)")
                   %>% html_text() %>% .[3:32], att=espn.stats %>% html_nodes("td:nth-child(6)") %>% 
                   html_text() %>% .[2:31], yds=espn.stats %>% html_nodes("td:nth-child(7)") %>% 
                   html_text() %>% .[2:31], td=espn.stats %>% html_nodes("td:nth-child(8)") %>%
                   html_text() %>% .[2:31], int=espn.stats %>% html_nodes("td:nth-child(9)") %>%
                   html_text() %>% .[2:31], sack=espn.stats %>% html_nodes("td:nth-child(10)") %>%
                   html_text() %>% .[2:31], fum=espn.stats %>% html_nodes("td:nth-child(11)") %>%
                   html_text() %>% .[2:31], rat=espn.stats %>% html_nodes("td:nth-child(12)") %>%
                   html_text() %>% .[2:31])
  }
  all.weekly.stats[[i-2005]]<-standin
}

#put it all together...
full.weekly.data<-list()
for(i in 1:11) {
  standin<-list()
  for(j in 1:17) {
    store<-rep(NA,dim(all.weekly.qbr[[i]][[j]])[1])
    for(k in 1:dim(all.weekly.qbr[[i]][[j]])[1]) {
      for(l in 1:dim(all.weekly.stats[[i]][[j]])[1]) {
        if(all.weekly.qbr[[i]][[j]][k,1]==all.weekly.stats[[i]][[j]][l,1]) {
          store[k]<-l
        }
      }
    }
    standin[[j]]<-data.frame(all.weekly.qbr[[i]][[j]],all.weekly.stats[[i]][[j]][store,])
  }
  full.weekly.data[[i]]<-standin
}

#clean it up

for(i in 1:11) {
  for(j in 1:17) {
    #full.weekly.data[[i]][[j]]<-full.weekly.data[[i]][[j]][-which(is.na(
      #full.weekly.data[[i]][[j]][,5])),]
    full.weekly.data[[i]][[j]]<-na.omit(full.weekly.data[[i]][[j]])
    full.weekly.data[[i]][[j]]<-full.weekly.data[[i]][[j]][,-c(5,6,7)]
  }
}


nearlythere<-list()
for(i in 1:11) {
  nearlythere[[i]]<-data.frame(rbind(full.weekly.data[[i]][[1]],full.weekly.data[[i]][[2]],
                                full.weekly.data[[i]][[3]],full.weekly.data[[i]][[4]],
                                full.weekly.data[[i]][[5]],full.weekly.data[[i]][[6]],
                                full.weekly.data[[i]][[7]],full.weekly.data[[i]][[8]],
                                full.weekly.data[[i]][[9]],full.weekly.data[[i]][[10]],
                                full.weekly.data[[i]][[11]],full.weekly.data[[i]][[12]],
                                full.weekly.data[[i]][[13]],full.weekly.data[[i]][[14]],
                                full.weekly.data[[i]][[15]],full.weekly.data[[i]][[16]],
                                full.weekly.data[[i]][[17]]))
}

final.weekly<-data.frame(rbind(nearlythere[[1]],nearlythere[[2]],nearlythere[[3]],
                               nearlythere[[4]],nearlythere[[5]],nearlythere[[6]],
                               nearlythere[[7]],nearlythere[[8]],nearlythere[[9]],
                               nearlythere[[10]],nearlythere[[11]]))

#finally done!
write.csv(final.weekly,"weeklynflstats.csv")
#leave our first column when reading in








