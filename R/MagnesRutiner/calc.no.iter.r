##' Calculating number of iterations per parallell MCMC run
##'
##' @title Calculating number of MCMC iterations
##' @inheritParams dummy.for.documentation
##' @param FullData.historical.new - The FullData object to be used for estimation
##' @param FullData.historical.previous - A FullData object from a previous estimation with less data
##' @param minBurnInIter - Integer, minimum number of iterations for burn in
##' @param nExtraBurnInIterDividedByTotDays - Integer, gives the number of extra burn in
##'        iterations when dividing by the total number of days from stocking to present.
##' @param nExtraBurnInIterPerNewDays - Integer, number of extra burn in
##'        iterations per new days in historical data
##' @param nIterToUse - Integer, number of iterations to use for calculating posterior
##' @return Integer, the total number of iteration per chan/run
##' @author Magne Aldrin
##' @export
calc.no.iter<-function(FullData.historical.new,FullData.historical.previous,
                       no.days.back,
                       minBurnInIter,
                       nExtraBurnInIterDividedByTotDays,
                       nExtraBurnInIterPerNewDays,
                       nIterToUse) {

  nTotDays<-FullData.historical$divinfo$n.times-no.days.back

  if(!is.null(FullData.historical.previous)) {
    nNewDays<-FullData.historical$divinfo$n.times-FullData.historical.previous$divinfo$n.times
  } else {
    nNewDays<-nTotDays
  }
  
  nTotBurnInIter<-minBurnInIter+
    nExtraBurnInIterDividedByTotDays/nTotDays+
    nExtraBurnInIterPerNewDays*nNewDays
  
  nIter<-nTotBurnInIter+nIterToUse
  nIter<-round(nIter)

  nIter
}
