##' Summarises output from lice model.
##'
##' @title Summarises output from lice model.
##' @inheritParams dummy.for.documentation
##' @param nRuns - Integer, number of parallel, independent runs
##' @param thin - Integer, thinning, save each thin'th sample, value 1 is just for test
##' @param nIterToUse - Integer, number of iterations per independent run to use for calculating posterior
##' @return A list with an AllData object, se documentation
##' in function plot.4panels.treat.obs.exp
##' @author Magne Aldrin
##' @export
summarise.output<-function(FullData.historical,FullData.future,est.res,pred.res,sim.per.sim,nIterToUse,nRuns,thin,intervals,limits,
                           no.fish.counted=20,adjust.factor=1) {
  
  Rsim.historical<-est.res$est$paramSamples[c("P.CH","P.OM","P.Af"),]
  rownames(Rsim.historical)<-c("CH","OM","Af")
  
  sim.lice.historical<-est.res$fit$simLice
  sim.lice.future<-pred.res$simLice
  
  
  merd.names.historical<-names(sim.lice.historical)
  n.merds.historical<-length(merd.names.historical)
  dn<-dimnames(sim.lice.historical[[1]])
  time.names.historical<-dn[[1]]
  n.times.historical<-length(time.names.historical)
  stage.names.historical<-dn[[2]]
  n.stages.historical<-length(stage.names.historical)
  sim.names.historical<-dn[[3]]
  n.sim.historical<-length(sim.names.historical)
  
  merd.names.future<-names(sim.lice.future)
  n.merds.future<-length(merd.names.future)
  dn<-dimnames(sim.lice.future[[1]])
  time.names.future<-dn[[1]]
  n.times.future<-length(time.names.future)
  stage.names.future<-dn[[2]]
  n.stages.future<-length(stage.names.future)
  sim.names.future<-dn[[3]]
  n.sim.future<-length(sim.names.future)
  
  
  if (any(merd.names.future != merd.names.historical)) {
    stop("(any(merd.names.future != merd.names.historical)")
  } else
    if (any(stage.names.future != stage.names.historical)) {
      stop("(any(stage.names.future != stage.names.historical)")
    }
  if (!any(sim.names.future %in% sim.names.historical)) {
    stop("(any(sim.names.future !(%in%) sim.names.historical)")
  }
  
  tmp.cleaner.fish.historical<-est.res$fit$cleanerfish
  tmp.cleaner.fish.future<-pred.res$cleanerfish
  cleaner.fish.types.historical<-names(tmp.cleaner.fish.historical)
  n.cleaner.fish.types.historical<-length(cleaner.fish.types.historical)
  cleaner.fish.types.future<-names(tmp.cleaner.fish.future)
  n.cleaner.fish.types.future<-length(cleaner.fish.types.future)
  if (any(cleaner.fish.types.future != cleaner.fish.types.historical)) {
    stop("any(cleaner.fish.types.future != cleaner.fish.types.historical)")
  }
  
  cleaner.fish.historical<-vector("list",n.merds.historical)
  names(cleaner.fish.historical)<-merd.names.historical
  tmp<-array(0,dim=c(n.times.historical,(n.cleaner.fish.types.historical+1),n.sim.historical),
             dimnames=list(time.names.historical,c(cleaner.fish.types.historical,"RensefiskTot"),sim.names.historical))
  for (merd in merd.names.historical) {
    cleaner.fish.historical[[merd]]<-tmp
    for (clf.type in cleaner.fish.types.historical) {
      cleaner.fish.historical[[merd]][,clf.type,]<-tmp.cleaner.fish.historical[[clf.type]][[merd]]
      cleaner.fish.historical[[merd]][,n.cleaner.fish.types.historical+1,]<-
        cleaner.fish.historical[[merd]][,n.cleaner.fish.types.historical+1,]+
        tmp.cleaner.fish.historical[[clf.type]][[merd]]
    }
  }
  
  cleaner.fish.future<-vector("list",n.merds.future)
  names(cleaner.fish.future)<-merd.names.future
  tmp<-array(0,dim=c(n.times.future,(n.cleaner.fish.types.future+1),n.sim.future),
             dimnames=list(time.names.future,c(cleaner.fish.types.future,"RensefiskTot"),sim.names.future))
  for (merd in merd.names.future) {
    cleaner.fish.future[[merd]]<-tmp
    for (clf.type in cleaner.fish.types.future) {
      cleaner.fish.future[[merd]][,clf.type,]<-tmp.cleaner.fish.future[[clf.type]][[merd]]
      cleaner.fish.future[[merd]][,n.cleaner.fish.types.future+1,]<-
        cleaner.fish.future[[merd]][,n.cleaner.fish.types.future+1,]+
        tmp.cleaner.fish.future[[clf.type]][[merd]]
    }
  }
  
  tmp<-matrix(unlist(strsplit(sim.names.future,"_")),ncol=2,byrow=T)
  ord<-order(as.numeric(tmp[,2]),as.numeric(tmp[,1]))
  sim.names.future<-sim.names.future[ord]
  
  Rsim<-Rsim.historical[,sim.names.future,drop=F]
  for (merd in merd.names.historical) {
    sim.lice.historical[[merd]]<-sim.lice.historical[[merd]][,,sim.names.future,drop=F]
    cleaner.fish.historical[[merd]]<-cleaner.fish.historical[[merd]][,,sim.names.future,drop=F]
    sim.lice.future[[merd]]<-sim.lice.future[[merd]][,,sim.names.future,drop=F]
    cleaner.fish.future[[merd]]<-cleaner.fish.future[[merd]][,,sim.names.future,drop=F]
  }


  if (FALSE) {
    ind<-seq(1,n.sim.future,50)
    ind<-c(ind,n.sim.future)
    tmp.sim.lice.historical<-list()
    tmp.sim.lice.future<-list()
    tmp.cleaner.fish.historical<-list()
    tmp.cleaner.fish.future<-list()
    for (merd in merd.names.historical) {
      tmp.sim.lice.historical[[merd]]<-NULL
      tmp.sim.lice.future[[merd]]<-NULL
      tmp.cleaner.fish.historical[[merd]]<-NULL
      tmp.cleaner.fish.future[[merd]]<-NULL
      for (i in 2:length(ind)) {
        tmp.sim.lice.historical[[merd]]<-cbind(tmp.sim.lice.historical[[merd]],apply(sim.lice.historical[[merd]][,3,(ind[i-1]:ind[i])],1,mean))
        tmp.sim.lice.future[[merd]]<-cbind(tmp.sim.lice.future[[merd]],apply(sim.lice.future[[merd]][,3,(ind[i-1]:ind[i])],1,mean))
        tmp.cleaner.fish.historical[[merd]]<-cbind(tmp.cleaner.fish.historical[[merd]],apply(cleaner.fish.historical[[merd]][,3,(ind[i-1]:ind[i])],1,mean))
        tmp.cleaner.fish.future[[merd]]<-cbind(tmp.cleaner.fish.future[[merd]],apply(cleaner.fish.future[[merd]][,3,(ind[i-1]:ind[i])],1,mean))
      }
          

    }
    browser()
  }

  n.samples.to.use<-round(nIterToUse*nRuns/thin)
  
  burn.in<-max(0,n.sim.future-n.samples.to.use)
  ind<-(burn.in+1):n.sim.future
  for (merd in merd.names.historical) {
    sim.lice.historical[[merd]]<-sim.lice.historical[[merd]][,,ind,drop=F]
    sim.lice.future[[merd]]<-sim.lice.future[[merd]][,,ind,drop=F]
    cleaner.fish.historical[[merd]]<-cleaner.fish.historical[[merd]][,,ind,drop=F]
    cleaner.fish.future[[merd]]<-cleaner.fish.future[[merd]][,,ind,drop=F]
  }
  Rsim<-Rsim[,ind]
  
  
  AllData.historical<-make.AllData(FullData=FullData.historical,sim.lice=sim.lice.historical,
                                   cleaner.fish=cleaner.fish.historical,Rsim=Rsim,
                                   sim.per.sim=sim.per.sim,intervals=intervals,limits=limits,
                                   no.fish.counted=no.fish.counted,adjust.factor=adjust.factor)
  
  AllData.future<-make.AllData(FullData=FullData.future,sim.lice=sim.lice.future,
                               cleaner.fish=cleaner.fish.future,Rsim=Rsim,
                               sim.per.sim=sim.per.sim,intervals=intervals,limits=limits,
                               no.fish.counted=no.fish.counted,adjust.factor=adjust.factor)

  AllData<-list(AllData.historical=AllData.historical,AllData.future=AllData.future)

  AllData
}
  






