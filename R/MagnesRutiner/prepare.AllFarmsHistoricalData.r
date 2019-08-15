##' Prepares various data for all Norwegian farms, to be used as background data for the lice model. 
##'
##' @title Prepares historical data for all farms.
##' @param AllFarmsRawData - A list with various historical data for all Norwegian farms.
##'        The list elements includes
##' \itemize{
##'  \item MedicineInfo - A data frame with information on different type of treatments,
##'        with one row per treatment. The column names are
##'        medicine, product.name, feed, delay, duration.const, temp.dependent,
##'        CH.effect, PA.effect, A.effect and egg.effect.
##'  \item coord - An (n.farms x 2) matrix with cooordinates for the n.farms farms. One row per farm
##'        with farm id numbers as row names. Column lat with latitude degrees North,
##'        written as decimal number, and column lon with longitudinal degrees East.
##'  \item S - An (n.weeks x n.farms) matrix with with 1 if a farm is active (i.e. have fish that week),
##'        with one row per week an one column per farm.
##'        The rownames are of the form "yyyyww" and the column names are the farm id's.
##'  \item no - An (n.weeks x n.farms) matrix with number of fish at the farm
##'        measured in millions of fish, with one row per week an one column per farm.
##'        The rownames are of the form "yyyyww" and the column names are the farm id's.
##         The number of fish should ideally be the number of fish at a farm
##'        the week in question, but this is probably (per February 2018 unavailable),
##'        and can be replaced by the maximum number of fish allowed.
##'        When the farm is non-active, the number iof fish must be 0. Missing values are not allowed.
##'  \item AfPerFish - An (n.weeks x n.farms) matrix of weekly abundance of adult female lice 
##'        at the farms, with one row per week an one column per farm.
##'        The rownames are of the form "yyyyww" and the column names are the farm id's. 
##         The lice abundance are typically downloaded from Barentswatch,
##'        and will contain missing values (NA).
##'  \item temp - An (n.weeks x n.farms) matrix of weekly seawater temperatures at the farms
##'        measured in degrees Celcius, with one row per week an one column per farm.
##'        The rownames are of the form "yyyyww" and the column names are the farm id's.
##'        The number of weeks may differ from that in other weekly matrices.
##'        The temperatures are typically downloaded from Barentswatch,
##'        and will contain several missing values (NA) and probably also some errors.
##'  \item salinity - An (n.days x n.farms) matrix of daily salinity (in psu) at the farms,
##'        with one row per day an one column per farm.
##'        The rownames are of the form "yyyymmdd" and the column names are the farm id's.
##'        The salinity are typically downloaded from met.no and are
##'        based on their hydrological model.
##'        There may be some missing data (NA).
##'  \item Dist - An (n.farms x n.farms) matrix of pairwise seaway distances (in km) between all farms.
##'        The farm id's are the row and column names. The may be some missing data (NA), which
##'        typical mean that the corresponding distance is more than 100 km.
##'  \item dates.info - list text strings of form "yyyww" or "yyymmdd" indicating
##'        first and last week or day in for the various data. The elements are
##'        first.week.S,last.week.S (also for no fish, Af.ext and AfPerFish),
##'        first.week.temp,last.week.temp,
##'        first.day.sal, last.day.sal.
##' }
##' @return A list of type AllFarmsData, 
##' @author Magne Aldrin
##' @export
prepare.AllFarmsHistoricalData<-function(AllFarmsRawData) {

  AllFarmsData<-AllFarmsRawData
  
  locno<-colnames(AllFarmsData$temp)
  
  Dist<-AllFarmsData$Dist
  Dist[is.na(Dist)]<--1
  Dist<-make.consistent.matrix(Dist,locno,default.val.diag=0,default.val.offdiag=-1,txt="Dist.sea")
  
  Dist.ind<-Dist
  Dist.ind[,]<-1
  diag(Dist.ind)<-0
  Dist.ind[Dist<0]<-0
  Dist[Dist<0]<-10000   ### samme hva som staar her, for det blir nullet ut i beregningene
  Dist[(Dist.ind!=0) & (Dist<0.1)]<-0.1

  AllFarmsData$Dist<-Dist
  AllFarmsData$Dist.ind<-Dist.ind
  rm(Dist)
  rm(Dist.ind)
  AllFarmsData$locno<-locno
  rm(locno)
  
  DivInfo<-list(Dist=AllFarmsData$Dist,Dist.ind=AllFarmsData$Dist.ind,coord=AllFarmsData$coord,locno=AllFarmsData$locno)


  ## interpolating seawater temperature
  print("interpolating seawater temperature")
  interpol.options<-list(NA.val=0,min.equal.val=6,no.na.interpol.w=1,max.outlier.val=3,seaway.to.time.factor=100)
  AllFarmsData$temp<-interpol(Ydat=AllFarmsData$temp,DivInfo=DivInfo,interpol.options=interpol.options)
  ##save(Ydat.interpol,file="SavedRObjects/Ydat.interpol.RData")

  ## Infection pressure, Af at neighbouring farms
  print("interpolating AfPerFish")
  AfPerFish.imp<-interpolate.within.and.between(x=AllFarmsData$AfPerFish,
                                                S=AllFarmsData$S,
                                                Dist=AllFarmsData$Dist,
                                                Dist.ind=AllFarmsData$Dist.ind,
                                                seaway.to.time.factor=100,
                                                interpolate.all=F)
  AllFarmsData$Af.ext<-weighted.sum(x=AfPerFish.imp,S=AllFarmsData$S,Dist=AllFarmsData$Dist,Dist.ind=AllFarmsData$Dist.ind,sum.w.to.1=T)
  AllFarmsData$N.Af.ext<-weighted.sum(x=1e6*AllFarmsData$no*AfPerFish.imp,S=AllFarmsData$S,Dist=AllFarmsData$Dist,Dist.ind=AllFarmsData$Dist.ind,sum.w.to.1=F)
  AllFarmsData$AfPerFish[AllFarmsData$AfPerFish<0]<-NA
  rm(AfPerFish.imp)

  ## interpolating salinity
  print("interpolating salinity - obs daily data - may take time")
  interpol.options<-list(NA.val=0,min.equal.val=6,no.na.interpol.w=7,max.outlier.val=3,seaway.to.time.factor=15)
  AllFarmsData$salinity<-interpol(Ydat=AllFarmsData$salinity,DivInfo=DivInfo,interpol.options=interpol.options)


  rn<-rownames(AllFarmsData$S)
  first.week.S<-rn[1]
  last.week.S<-tail(rn,1)

  rn<-rownames(AllFarmsData$temp)
  first.week.temp<-rn[1]
  last.week.temp<-tail(rn,1)

  rn<-rownames(AllFarmsData$salinity)
  first.day.sal<-rn[1]
  last.day.sal<-tail(rn,1)

  AllFarmsData$dates.info<-list(first.week.S=first.week.S,
                                last.week.S=last.week.S,
                                first.week.temp=first.week.temp,
                                last.week.temp=last.week.temp,
                                first.day.sal=first.day.sal,
                                last.day.sal=last.day.sal)
                                

  AllFarmsData
}


