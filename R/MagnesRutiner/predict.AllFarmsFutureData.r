##' Predicts various data for all Norwegian farms, to be used as
##' background data for the lice model.
##'
##' @title Predicts future data for all farms.
##' @inheritParams dummy.for.documentation
##' @return A list with data on all Norwegian farms. The two list elements are
##' \itemize{
##'  \item AllFarmsFutureData - A list of type AllFarmsData, i.e.
##         the same type of object as the input for historical data,
##'        but with predictions ahead in time, i.e. for a future data.
##'        In addition, there are five arrays with predictions with corresponding
##'        prediction intervals.  These are salinity.pred.stat, temp.pred.stat,
##'        AfPerFish.pred.stat, Af.ext.pred.stat and N.Af.ext.pred.stat, all with
##'        dimensions (number of times) x (number of farms) x (number of statistics),
##'        where the statistics are mean (point prediction) and lower and upper limits
##'        of confidence intervals.
##'  \item AllFarmsTotData - A list of type AllFarmsData where
##'         two AllFarmsData object, one for the historical period
##'        (input to the function) and one for the future period are merged
##'        to an AllDataFarm object for the historical and future period together.
##' }
##' @author Magne Aldrin
##' @export predict.AllFarmsFutureData
predict.AllFarmsFutureData<-function(AllFarmsData,forecast.horizon.weeks=12,nsim,intervals) {

  library(mgcv)

  AllFarmsHistoricalData<-AllFarmsData
  
  Dist<-AllFarmsHistoricalData$Dist
  Dist.ind<-AllFarmsHistoricalData$Dist.ind
  coord<-AllFarmsHistoricalData$coord

  # should not be necesarry
  tmp<-rownames(AllFarmsHistoricalData$AfPerFish) ## weekly time series
  last.week.AfPerFish<-tmp[length(tmp)]
  tmp<-rownames(AllFarmsHistoricalData$no) ## weekly time series
  last.week.no<-tmp[length(tmp)]
  tmp<-rownames(AllFarmsHistoricalData$S) ## weekly time series
  last.week.S<-tmp[length(tmp)]
  tmp<-rownames(AllFarmsHistoricalData$temp) ## weekly time series
  last.week.temp<-tmp[length(tmp)]
  if (last.week.no!=last.week.AfPerFish | last.week.no!=last.week.S) {
    stop("last.week.no!=last.week.AfPerFish | last.week.no!=last.week.S")
  }
  last.week.S.in.data<-last.week.S
  last.week.temp.in.data<-last.week.temp
  
  last.week.S<-AllFarmsHistoricalData$dates.info$last.week.S
  last.week.temp<-AllFarmsHistoricalData$dates.info$last.week.temp

  if (last.week.S!=last.week.S.in.data | last.week.temp!=last.week.temp.in.data) {
    stop("last.week.S!=last.week.S.in.data | last.week.temp!=last.week.temp.in.data")
  }
  
  if (as.numeric(last.week.temp)>as.numeric(last.week.S)) {
    last.week<-last.week.temp
  } else {
    last.week<-last.week.S
  }
  
  tmp<-rownames(AllFarmsHistoricalData$salinity) ## daily time series
  last.day.sal.in.data<-tmp[length(tmp)]
  last.day.sal<-AllFarmsHistoricalData$dates.info$last.day.sal
  if (last.day.sal!=last.day.sal.in.data) {
    stop("last.day.sal!=last.day.sal.in.data")
  }



  
  tmp.yy<-as.numeric(substr(last.day.sal,1,4))
  tmp.mm<-as.numeric(substr(last.day.sal,5,6))
  tmp.dd<-as.numeric(substr(last.day.sal,7,8))
  tmp<-GenKalender(tmp.yy,tmp.mm,tmp.dd,tmp.yy,tmp.mm,tmp.dd)
  last.week.sal<-paste(tmp["year.with.week.number"],tmp["weekyear"],sep="")

  if (as.numeric(last.week.sal)>as.numeric(last.week)) {
    last.week<-last.week.sal
  }
  last.year<-as.numeric(substr(last.week,1,4))
  last.weekyear<-as.numeric(substr(last.week,5,6))

  first.year.S<-as.numeric(substr(AllFarmsHistoricalData$dates.info$first.week.S,1,4))
  first.year.temp<-as.numeric(substr(AllFarmsHistoricalData$dates.info$first.week.temp,1,4))
  first.year.sal<-as.numeric(substr(AllFarmsHistoricalData$dates.info$first.day.sal,1,4))
  ## These lines are corrected 27/4-2018 by Magne Aldrin
  ##first.year.temp<-as.numeric(substr(AllFarmsHistoricalData$dates.info$first.week.S,1,4))
  ##first.year.sal<-as.numeric(substr(AllFarmsHistoricalData$dates.info$first.day.S,1,4))
  first.year<-min(c(first.year.S,first.year.temp,first.year.sal))
    
  print("generates calendar")
  tmp.cc<-GenKalender(first.year,1,1,last.year+3,1,1)
  print("finished with generating calendar")

  
  n.tmp.cc<-nrow(tmp.cc)
  ind.2<-(1:n.tmp.cc)[tmp.cc[,"year.with.week.number"]==last.year & tmp.cc[,"weekyear"]==last.weekyear]
  ind.2<-tail(ind.2,1) ### index for last day in week with any observed values of any data type
  ind.2<-ind.2+forecast.horizon.weeks*7 ## index for last day with forecast

  ind.1<-(1:n.tmp.cc)[tmp.cc[,"year"]==tmp.yy & tmp.cc[,"month"]==tmp.mm & tmp.cc[,"day"]==tmp.dd]
  ind.1<-ind.1[1]   ### index for last day with observed salinity
  no.forecast.days<-(ind.2-ind.1)
  tmp.ymd<-ymd.num2char(tmp.cc[(ind.1+1):ind.2,"year"],tmp.cc[(ind.1+1):ind.2,"month"],tmp.cc[(ind.1+1):ind.2,"day"])
  if (length(tmp.ymd)!=no.forecast.days) {stop("length(tmp.ymd)!=no.forecast.days")}
  print("predicts salinity")

  salinity.pred.stat<-predict.Y.AllFarms(Ydat=AllFarmsHistoricalData$salinity,coord=coord,
                                         rn=tmp.ymd,daily=T,nsim=nsim,intervals=intervals,
                                         logtrans=F,small.const=NA)

  last.week.tmp<-last.week.temp
  last.year<-as.numeric(substr(last.week.tmp,1,4))
  last.weekyear<-as.numeric(substr(last.week.tmp,5,6)) 
  ind.1<-(1:n.tmp.cc)[tmp.cc[,"year.with.week.number"]==last.year & tmp.cc[,"weekyear"]==last.weekyear]
  ind.1<-ind.1[7]   ### index for last day in week with any observed temperature
  no.forecast.weeks<-(ind.2-ind.1)/7
  tmp.yw<-yw.num2char(tmp.cc[(ind.1+1):ind.2,"year.with.week.number"],tmp.cc[(ind.1+1):ind.2,"weekyear"])
  tmp.yw<-unique(tmp.yw)
  if (length(tmp.yw)!=no.forecast.weeks) {stop("length(tmp.yw)!=no.forecast.weeks")}
  print("predicts seawater temperature")
  temp.pred.stat<-predict.Y.AllFarms(Ydat=AllFarmsHistoricalData$temp,coord=coord,
                                     rn=tmp.yw,daily=F,nsim=nsim,intervals=intervals,
                                     logtrans=F,small.const=NA)

  ##last.week.tmp<-last.week.AfPerFish
  last.week.tmp<-last.week.S
  last.year<-as.numeric(substr(last.week.tmp,1,4))
  last.weekyear<-as.numeric(substr(last.week.tmp,5,6)) 
  ind.1<-(1:n.tmp.cc)[tmp.cc[,"year.with.week.number"]==last.year & tmp.cc[,"weekyear"]==last.weekyear]
  ind.1<-ind.1[7]   ### index for last day in week with any observed temperature
  no.forecast.weeks<-(ind.2-ind.1)/7
  tmp.yw<-yw.num2char(tmp.cc[(ind.1+1):ind.2,"year.with.week.number"],tmp.cc[(ind.1+1):ind.2,"weekyear"])
  tmp.yw<-unique(tmp.yw)
  if (length(tmp.yw)!=no.forecast.weeks) {stop("length(tmp.yw)!=no.forecast.weeks")}
  print("predicts AfPerFish")
  AfPerFish.pred.stat<-predict.Y.AllFarms(Ydat=AllFarmsHistoricalData$AfPerFish,coord=coord,
                                          rn=tmp.yw,daily=F,nsim=nsim,intervals=intervals,
                                          logtrans=T,small.const=0.01)

  ##last.week.tmp<-last.week.no
  last.week.tmp<-last.week.S
  last.year<-as.numeric(substr(last.week.tmp,1,4))
  last.weekyear<-as.numeric(substr(last.week.tmp,5,6)) 
  ind.1<-(1:n.tmp.cc)[tmp.cc[,"year.with.week.number"]==last.year & tmp.cc[,"weekyear"]==last.weekyear]
  ind.1<-ind.1[7]   ### index for last day in week with any observed temperature
  no.forecast.weeks<-(ind.2-ind.1)/7
  tmp.yw<-yw.num2char(tmp.cc[(ind.1+1):ind.2,"year.with.week.number"],tmp.cc[(ind.1+1):ind.2,"weekyear"])
  tmp.yw<-unique(tmp.yw)
  if (length(tmp.yw)!=no.forecast.weeks) {stop("length(tmp.yw)!=no.forecast.weeks")}
  tmp<-AllFarmsHistoricalData$no
  q<-ncol(tmp)
  no<-matrix(tmp[nrow(tmp),],nrow=no.forecast.weeks,ncol=q,byrow=T,dimnames=list(tmp.yw,colnames(tmp)))
  
  last.week.tmp<-last.week.S
  last.year<-as.numeric(substr(last.week.tmp,1,4))
  last.weekyear<-as.numeric(substr(last.week.tmp,5,6)) 
  ind.1<-(1:n.tmp.cc)[tmp.cc[,"year.with.week.number"]==last.year & tmp.cc[,"weekyear"]==last.weekyear]
  ind.1<-ind.1[7]   ### index for last day in week with any observed temperature
  no.forecast.weeks<-(ind.2-ind.1)/7
  tmp.yw<-yw.num2char(tmp.cc[(ind.1+1):ind.2,"year.with.week.number"],tmp.cc[(ind.1+1):ind.2,"weekyear"])
  tmp.yw<-unique(tmp.yw)
  if (length(tmp.yw)!=no.forecast.weeks) {stop("length(tmp.yw)!=no.forecast.weeks")}
  tmp<-AllFarmsHistoricalData$S
  q<-ncol(tmp)
  S<-matrix(tmp[nrow(tmp),],nrow=no.forecast.weeks,ncol=q,byrow=T,dimnames=list(tmp.yw,colnames(tmp)))

  Af.ext.pred.stat<-AfPerFish.pred.stat
  N.Af.ext.pred.stat<-AfPerFish.pred.stat
  stat.names<-dimnames(AfPerFish.pred.stat)[[3]]
  for (stat in 1:length(stat.names)) {
    Af.ext.pred.stat[,,stat]<-weighted.sum(x=AfPerFish.pred.stat[,,stat],S=S,Dist=Dist,Dist.ind=Dist.ind,sum.w.to.1=T)
    N.Af.ext.pred.stat[,,stat]<-weighted.sum(x=1e6*no*AfPerFish.pred.stat[,,stat],S=S,Dist=Dist,Dist.ind=Dist.ind,sum.w.to.1=F)
  }
##  Af.ext<-weighted.sum(x=AfPerFish.pred.stat[,,"mean"],S=S,Dist=Dist,Dist.ind=Dist.ind,sum.w.to.1=T)
##  N.Af.ext<-weighted.sum(x=1e6*no*AfPerFish.pred.stat[,,"mean"],S=S,Dist=Dist,Dist.ind=Dist.ind,sum.w.to.1=F)

  AllFarmsFutureData<-list()
  
  AllFarmsFutureData$salinity<-salinity.pred.stat[,,"mean"]
  AllFarmsFutureData$temp<-temp.pred.stat[,,"mean"]
  AllFarmsFutureData$AfPerFish<-AfPerFish.pred.stat[,,"mean"]
  AllFarmsFutureData$no<-no
  AllFarmsFutureData$S<-S
  AllFarmsFutureData$Af.ext<-Af.ext.pred.stat[,,"mean"]
  AllFarmsFutureData$N.Af.ext<-N.Af.ext.pred.stat[,,"mean"]

  AllFarmsFutureData$salinity.pred.stat<-salinity.pred.stat
  AllFarmsFutureData$temp.pred.stat<-temp.pred.stat
  AllFarmsFutureData$AfPerFish.pred.stat<-AfPerFish.pred.stat
  AllFarmsFutureData$Af.ext.pred.stat<-Af.ext.pred.stat
  AllFarmsFutureData$N.Af.ext.pred.stat<-N.Af.ext.pred.stat
  

  AllFarmsFutureData$Dist<-Dist
  AllFarmsFutureData$Dist.ind<-Dist.ind
  AllFarmsFutureData$coord<-coord

  AllFarmsTotData<-AllFarmsHistoricalData
  AllFarmsTotData$salinity<-rbind(AllFarmsHistoricalData$salinity,AllFarmsFutureData$salinity)
  AllFarmsTotData$temp<-rbind(AllFarmsHistoricalData$temp,AllFarmsFutureData$temp)
  AllFarmsTotData$AfPerFish<-rbind(AllFarmsHistoricalData$AfPerFish,AllFarmsFutureData$AfPerFish)
  AllFarmsTotData$no<-rbind(AllFarmsHistoricalData$no,AllFarmsFutureData$no)
  AllFarmsTotData$S<-rbind(AllFarmsHistoricalData$S,AllFarmsFutureData$S)
  AllFarmsTotData$Af.ext<-rbind(AllFarmsHistoricalData$Af.ext,AllFarmsFutureData$Af.ext)
  AllFarmsTotData$N.Af.ext<-rbind(AllFarmsHistoricalData$N.Af.ext,AllFarmsFutureData$N.Af.ext)

  rn<-rownames(AllFarmsFutureData$S)
  first.week.S<-rn[1]
  last.week.S<-tail(rn,1)
  rn<-rownames(AllFarmsFutureData$temp)
  first.week.temp<-rn[1]
  last.week.temp<-tail(rn,1)
  rn<-rownames(AllFarmsFutureData$salinity)
  first.day.sal<-rn[1]
  last.day.sal<-tail(rn,1)
  

  AllFarmsFutureData$dates.info<-list(first.week.S=first.week.S,
                                      last.week.S=last.week.S,
                                      first.week.temp=first.week.temp,
                                      last.week.temp=last.week.temp,
                                      first.day.sal=first.day.sal,
                                      last.day.sal=last.day.sal)

  AllFarmsTotData$dates.info<-list(first.week.S=AllFarmsData$dates.info$first.week.S,
                                   last.week.S=last.week.S,
                                   first.week.temp=AllFarmsData$dates.info$first.week.temp,
                                   last.week.temp=last.week.temp,
                                   first.day.sal=AllFarmsData$dates.info$first.day.sal,
                                   last.day.sal=last.day.sal)

  res<-list(AllFarmsFutureData=AllFarmsFutureData,AllFarmsTotData=AllFarmsTotData)
}


