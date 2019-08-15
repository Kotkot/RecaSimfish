vec.from.merds.to.farm<-function(x,antall.merds,antall.farm) {

  n.merds<-length(x)
  merd.names<-names(x)
  tmp<-x[[1]]
  dn<-dimnames(tmp)
  time.names<-dn[[1]]
  n.times<-length(time.names)
  
  w<-antall.merds/as.vector(antall.farm)
  w[is.na(w)]<-0
  
  res<-vector("list",1)
  names(res)<-"AllMerds"
###  res[[1]]<-x[[1]]
  
  for (merd in merd.names) {
    xx<-x[[merd]]
    xx[is.na(xx)]<-0
    if (merd==merd.names[1]) {
      tmp<-xx*w[,merd]
    } else {
      tmp<-tmp+xx*w[,merd]
    }
  }
  tmp[antall.farm==0]<-NA
  res[[1]]<-tmp

  res

}


