##' Makes an AllData object with data and lice model results.
##'
##' @title Makes an AllData object.
##' @inheritParams dummy.for.documentation
##' @param sim.lice - A list with samples of lice abundance, one element per cage (merd).
##' @param cleaner.fish - A list with samples of cleaner fish ratio,
##'        one element per cage (merd). All samples are equal per February 2018.
##' @param Rsim - A list with samples of the disperson parameter,
##'        one element per cage (merd). All samples are equal per February 2018.
##' @return A list of type AllData
##' @author Magne Aldrin
make.AllData<-function(FullData,sim.lice,cleaner.fish,Rsim,sim.per.sim=5,intervals=c(0.95,0.90,0.80),limits=c(0.1,0.2,0.5,1.0),
                       no.fish.counted=20,adjust.factor=1) {

  sim.lice.counts<-sim.from.merds.to.counts(sim=sim.lice,Rsim=Rsim,sim.per.sim=sim.per.sim,no.fish.counted=no.fish.counted)
  
  summary.on.merd.level<-summarise.lice.predictions(sim.lice,intervals=intervals,limits=limits)
  summary.on.merd.count.level<-summarise.lice.predictions(sim.lice.counts,intervals=intervals,limits=limits)
  
  FullData.farm<-FullData.from.merds.to.farm(FullData)
  
  sim.lice.farm<-sim.from.merds.to.farm(sim.lice,antall.merds=FullData$antall,antall.farm=FullData.farm$antall)
  sim.lice.farm.counts<-sim.from.merds.to.farm(sim.lice.counts,antall.merds=FullData$antall,antall.farm=FullData.farm$antall)

  summary.on.farm.level<-summarise.lice.predictions(sim.lice.farm,intervals=intervals,limits=limits,adjust.factor=1.6)
  summary.on.farm.count.level<-summarise.lice.predictions(sim.lice.farm.counts,intervals=intervals,limits=limits,adjust.factor=1.6)

  if (!is.null(cleaner.fish)) {
    summary.cleaner.fish.on.merd.level<-summarise.lice.predictions(cleaner.fish,intervals=intervals,limits=limits)
    cleaner.fish.farm<-sim.from.merds.to.farm(cleaner.fish,antall.merds=FullData$antall,antall.farm=FullData.farm$antall)
    summary.cleaner.fish.on.farm.level<-summarise.lice.predictions(cleaner.fish.farm,intervals=intervals,limits=limits)
    ##    CleanerFishMean<-summary.cleaner.fish.on.merd.level[,"RensefiskTot","mean",drop=F]
    ##    CleanerFishMean.farm<-summary.cleaner.fish.on.farm.level[,"RensefiskTot","mean",drop=F]
    ##    CleanerFishMean.farm<-vec.from.merds.to.farm(x=CleanerFishMean,antall.merds=FullData$antall,antall.farm=FullData.farm$antall)
  } else {
    summary.cleaner.fish.on.merd.level<-NULL
    summary.cleaner.fish.on.farm.level<-NULL
    ##CleanerFishMean<-NULL
    ##CleanerFishMean.farm<-NULL
  }
    
  
  AllData.merd.level<-list(FullData=FullData,
                           pred.summary=summary.on.merd.level,
                           count.summary=summary.on.merd.count.level,
                           cleaner.fish.summary=summary.cleaner.fish.on.merd.level)

  AllData.farm.level<-list(FullData=FullData.farm,
                           pred.summary=summary.on.farm.level,
                           count.summary=summary.on.farm.count.level,
                           cleaner.fish.summary=summary.cleaner.fish.on.farm.level)

  AllData<-list(AllData.merd.level=AllData.merd.level,AllData.farm.level=AllData.farm.level)

  AllData
}
