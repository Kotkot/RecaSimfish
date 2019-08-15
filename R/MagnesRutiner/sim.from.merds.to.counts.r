sim.from.merds.to.counts<-function(sim,Rsim,sim.per.sim=5,no.fish.counted=20) {

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

  tmp<-array(NA,dim=c(n.times,n.stages,n.sim*sim.per.sim),dimnames=list(time.names,stage.names,1:(n.sim*sim.per.sim)))
  res<-vector("list",n.merds)
  names(res)<-merd.names
  for (merd in merd.names) {
    res[[merd]]<-tmp
  }

  
  for (merd in merd.names) {
    for (stage in stage.names) {
      simres<-sim[[merd]][,stage,]
      tell.simres<-matrix(NA,nrow=n.times,ncol=n.sim*sim.per.sim)
      Rsimvec<-exp(Rsim[stage,])
      n.sim2<-length(Rsimvec)
      ind.obs<-(1:n.times)[!is.na(simres[,1])]
      n.obs<-length(ind.obs)
      if (n.sim !=n.sim2) {stop("n.sim !=n.sim2")}
      for (ss in 1:n.sim) {
        tmpsim<-rnbinom(n.obs*sim.per.sim,size=no.fish.counted*Rsimvec[ss],mu=rep((no.fish.counted*simres[ind.obs,ss]),sim.per.sim))/no.fish.counted
        tell.simres[ind.obs,((ss-1)*sim.per.sim+1):(ss*sim.per.sim)]<-matrix(tmpsim,ncol=sim.per.sim,byrow=F)
      }
      res[[merd]][,stage,]<-tell.simres
    }
  }

  res
}

