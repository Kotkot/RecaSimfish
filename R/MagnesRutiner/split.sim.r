split.sim<-function(sim,split.date,first=T) {

  time.names<-dimnames(sim[[1]])[[1]]
  n.times<-length(time.names)
  if (!(split.date>=time.names[1] & split.date<=tail(time.names,1))) {
    print("Illegal split.date in function split.sim, returns NULL!")
    return(NULL)
  }
          
  if (first) {
    ind<-(1:n.times)[time.names<=split.date]
  } else {
    ind<-(1:n.times)[time.names>=split.date]
  }

  res<-sim
  for (merdnumb in 1:length(sim)) {
    res[[merdnumb]]<-sim[[merdnumb]][ind,,,drop=F]
  }
  
  res
}
