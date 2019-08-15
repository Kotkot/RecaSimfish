sim.from.merds.to.farm<-function(sim,antall.merds,antall.farm) {

  n.merds<-length(sim)
  merd.names<-names(sim)
  tmp<-sim[[1]]
  dn<-dimnames(tmp)
  time.names<-dn[[1]]
  n.times<-length(time.names)
  stage.names<-dn[[2]]
  n.stages<-length(stage.names)
  sim.names<-dn[[3]]
  n.sim<-length(sim.names)
  
  w<-antall.merds/as.vector(antall.farm)
  w[is.na(w)]<-0
  
  res<-vector("list",1)
  names(res)<-"AllMerds"
  res[[1]]<-sim[[1]]

  for (stage in stage.names) {
    for (merd in merd.names) {
      simres<-sim[[merd]][,stage,]
      simres[is.na(simres)]<-0
      if (merd==merd.names[1]) {
        tmp<-simres*w[,merd]
      } else {
        tmp<-tmp+simres*w[,merd]
      }
    }
    tmp[antall.farm==0,]<-NA
    res[[1]][,stage,]<-tmp
  }

  res
}

