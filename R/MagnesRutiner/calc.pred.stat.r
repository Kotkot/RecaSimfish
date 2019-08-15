calc.pred.stat<-function(simres,intervals=NULL, limits=NULL, adjust.factor=1) {

  n.times<-nrow(simres)
  n.int<-length(intervals)
  n.lim<-length(limits)
  n.col<-1+2*n.int+2*n.lim

  
  res<-matrix(NA,nrow=n.times,ncol=n.col)
  rownames(res)<-rownames(simres)
  cnames<-rep("NA",n.col)
  
  cnames[1]<-"mean"
  res[,1]<-apply(simres,1,mean,na.rm=T)

  k<-1
  if (n.int>0) {
    for (i in 1:n.int) {
      int<-intervals[i]
      p.nominal<-(1-int)/2
      p.actual<-pnorm(q=qnorm(p.nominal,mean=0,sd=adjust.factor))
      k<-k+1
      cnames[k]<-paste("low",intervals[i],sep="")
      res[,k]<-apply(simres,1,quantile,probs=p.actual,na.rm=T)
      ###res[,k]<-apply(simres,1,quantile,probs=(1-int)/2,na.rm=T)
      k<-k+1
      cnames[k]<-paste("upp",intervals[i],sep="")
      res[,k]<-apply(simres,1,quantile,probs=1-p.actual,na.rm=T)
      ###res[,k]<-apply(simres,1,quantile,probs=1-(1-int)/2,na.rm=T)
      ##browser()
    }
  }
       
  if (n.lim>0) {
    for (i in 1:n.lim) {
      lim<-limits[i]
      k<-k+1
      cnames[k]<-paste("Pgt",lim,sep="")
      tmp<-simres
      tmp[tmp<=lim]<-0
      tmp[tmp>lim]<-1
      p.actual<-apply(tmp,1,mean,na.rm=T)
      p.nominal<-pnorm(q=qnorm(p.actual),mean=0,sd=adjust.factor)
      res[,k]<-p.nominal
      ###res[,k]<-apply(tmp,1,mean,na.rm=T)
      k<-k+1
      cnames[k]<-paste("cumPgt",lim,sep="")
      tmp[is.na(tmp)]<-0
      tmp<-apply(tmp,2,cumsum)
      tmp<-pmin(tmp,1)
      p.actual<-apply(tmp,1,mean,na.rm=T)
      p.nominal<-pnorm(q=qnorm(p.actual),mean=0,sd=adjust.factor)
      res[,k]<-p.nominal
      ###res[,k]<-apply(tmp,1,mean,na.rm=T)
      ##browser()
    }
  }
  colnames(res)<-cnames

  res[is.nan(res[,"mean"]),]<-NA
  
  res
}
