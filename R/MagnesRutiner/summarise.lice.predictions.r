##' Makes a summary of lice predictions.
##' 
##' @title Makes a summary of lice predictions.
##' @param sim 
##' @param intervals 
##' @param limits 
##' @return A list with one element per cage. Each element is a list with one element per stage, with various statistics.
##' @author Magne Aldrin
##' @export 
summarise.lice.predictions <- function(sim,intervals=c(0.95,0.90,0.80),limits=c(0.1,0.2,0.5,1.0),adjust.factor=1) {

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
    
  n.int<-length(intervals)
  n.lim<-length(limits)
  n.col<-1+2*n.int+2*n.lim

  res<-vector("list",n.merds)
  names(res)<-merd.names
  for (merd in merd.names) {
    res[[merd]]<-vector("list",n.stages)
    names(res[[merd]])<-stage.names
  }
  
  for (merd in merd.names) {
    for (stage in stage.names) {
      simres<-sim[[merd]][,stage,]
      res[[merd]][[stage]]<-calc.pred.stat(simres,intervals=intervals,limits=limits,adjust.factor=adjust.factor)
    }
  }

  res
}
