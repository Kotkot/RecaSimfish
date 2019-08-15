
##' Makes a FullData object with future data for a single farm.
##'
##' @title Makes FullData for future data.
##' @inheritParams dummy.for.documentation
##' @return A list of type FullData
##' @author Magne Aldrin
##' @export
make.FullData.future<-function(FullData,AllFarmsData,Treatment.future=NULL,
                               forecast.horizon.days) {

  last.day.with.obs<-tail(FullData$calendar,1)
  last.ymd<-rownames(tail(FullData$calendar,1))

  calendar<-GenKalender(last.day.with.obs[1,"year"],last.day.with.obs[1,"month"],last.day.with.obs[1,"day"],
                        last.day.with.obs[1,"year"]+3,1,1)
  calendar<-calendar[1:(forecast.horizon.days+1),]
  ymd<-ymd.num2char(calendar[,"year"],calendar[,"month"],calendar[,"day"])
  rownames(calendar)<-ymd
  first.date.in.pred.per<-ymd[1]
  last.date.in.pred.per<-tail(ymd,1)

  if (as.numeric(last.date.in.pred.per)>as.numeric(AllFarmsData$dates.info$last.day.sal)) {
    last.date.in.pred.per<-AllFarmsData$dates.info$last.day.sal
  }


  last.week<-AllFarmsData$dates.info$last.week.S
  last.year<-as.numeric(substr(last.week,1,4))
  last.weekyear<-as.numeric(substr(last.week,5,6))
  tmp.cc<-GenKalender(last.year,1,1,last.year,12,31)
  n.tmp.cc<-nrow(tmp.cc)
  ind.1<-(1:n.tmp.cc)[tmp.cc[,"year"]==last.year & tmp.cc[,"weekyear"]==last.weekyear]
  ind.1<-ind.1[7]   ### index for last day in week 
  tmp.ymd<-ymd.num2char(tmp.cc[ind.1,"year"],tmp.cc[ind.1,"month"],tmp.cc[ind.1,"day"])
  if (as.numeric(last.date.in.pred.per)>as.numeric(tmp.ymd)) {
    last.date.in.pred.per<-tmp.ymd
  }

  last.week<-AllFarmsData$dates.info$last.week.temp
  last.year<-as.numeric(substr(last.week,1,4))
  last.weekyear<-as.numeric(substr(last.week,5,6))
  tmp.cc<-GenKalender(last.year,1,1,last.year,12,31)
  ind.1<-(1:n.tmp.cc)[tmp.cc[,"year"]==last.year & tmp.cc[,"weekyear"]==last.weekyear]
  ind.1<-ind.1[7]   ### index for last day in week 
  tmp.ymd<-ymd.num2char(tmp.cc[ind.1,"year"],tmp.cc[ind.1,"month"],tmp.cc[ind.1,"day"])
  if (as.numeric(last.date.in.pred.per)>as.numeric(tmp.ymd)) {
    last.date.in.pred.per<-tmp.ymd
  }

  ## last.date.in.pred.per is now the last day with all necessary data
  calendar<-calendar[as.numeric(ymd)<=as.numeric(last.date.in.pred.per),]
  ymd<-ymd[as.numeric(ymd)<=as.numeric(last.date.in.pred.per)]
  forecast.horizon.days<-length(ymd)-1
  
  lice<-NULL
  
  tmp<-FullData$vekt
  q<-ncol(tmp)
  vekt<-matrix(tmp[nrow(tmp),],nrow=forecast.horizon.days+1,ncol=q,byrow=T,dimnames=list(ymd,colnames(tmp)))
 
  tmp<-FullData$antall
  q<-ncol(tmp)
  antall<-matrix(tmp[nrow(tmp),],nrow=forecast.horizon.days+1,ncol=q,byrow=T,dimnames=list(ymd,colnames(tmp)))
 
  tmp<-FullData$biom
  q<-ncol(tmp)
  biom<-matrix(tmp[nrow(tmp),],nrow=forecast.horizon.days+1,ncol=q,byrow=T,
               dimnames=list(ymd,colnames(tmp)))

  
##################################################
### Informasjon om medikamenter
##################################################
  MedicineInfo<-AllFarmsData$MedicineInfo


##################################################
### Behandling
##################################################
  if (!is.null(Treatment.future)) {
    StartEndDate<-data.frame(Generation=NA,
                             FromDate=as.Date(first.date.in.pred.per,format="%Y%m%d"),
                             ToDate=as.Date(last.date.in.pred.per,format="%Y%m%d"))
    
    TreatmentData<-MakeTreatmentData(Treatment.future,StartEndDate)
    tmp<-make.treat.and.treat.date(TreatmentData=TreatmentData,
                                   MedicineInfo=MedicineInfo,
                                   ymd.tot=ymd,
                                   merd.names=FullData$divinfo$merd.names)

    behand<-tmp$behand
    med.names<-tmp$med.names
    n.med<-tmp$n.med
    med.names.not.in.MedicineInfo<-tmp$med.names.not.in.MedicineInfo
  } else {
    behand<-NULL
    med.names<-NULL
    n.med<-0
    med.names.not.in.MedicineInfo<-NULL
  }
    


##  tmp<-FullData$behand
##  if (is.null(tmp)) {
##    behand<-NULL
##  } else {
##    behand<-array(0,dim=c(forecast.horizon.days+1,dim(tmp)[2],dim(tmp)[3]),
##                  dimnames=list(ymd,dimnames(tmp)[[2]],dimnames(tmp)[[3]]))
##  }
## behand[1,,]<-tmp[dim(tmp)[1],,]
  
  tmp<-FullData$cleaner.fish.stocked
  for (i in 1:length(tmp)) {
    tmp2<-tmp[[i]][nrow(tmp[[i]]),]
    tmp[[i]]<-matrix(0,nrow=forecast.horizon.days+1,ncol=ncol(tmp[[i]]),byrow=T,
                     dimnames=list(ymd,colnames(tmp[[i]])))
    tmp[[i]][1,]<-tmp2
  }
  cleaner.fish.stocked<-tmp

  tmp<-FullData$w
  tmp2<-matrix(0,nrow=dim(tmp)[2],ncol=dim(tmp)[3])
  nr<-nrow(tmp2)
    for (i in 1:nr) {
    tmp2[i,i]<-1
  }
  w<-array(0,dim=c(forecast.horizon.days+1,dim(tmp)[2],dim(tmp)[3]),
           dimnames=list(ymd,dimnames(tmp)[[2]],dimnames(tmp)[[3]]))
  w[1,,]<-tmp[dim(tmp)[1],,]
  for (i in 2:(forecast.horizon.days+1)) {
    w[i,,]<-tmp2
  }

  tmp<-FullData$S
  q<-ncol(tmp)
  S<-matrix(tmp[nrow(tmp),],nrow=forecast.horizon.days+1,ncol=q,byrow=T,
            dimnames=list(ymd,colnames(tmp)))

  loc.no<-FullData$divinfo$loc.no
  start.year<-calendar[1,"year"]
  TEMPobj<-extract.and.expand(AllFarmsData$temp,loc.no=loc.no,start.year=start.year)
  temp<-TEMPobj$daily.series
  calendar.temp<-TEMPobj$calendar
  ymd.tot<-TEMPobj$ymd
  first.date.with.temp<-ymd.tot[1]
  last.date.with.temp<-tail(ymd.tot,1)
  
  
  smittepressObj<-extract.and.expand(AllFarmsData$N.Af.ext,loc.no=loc.no,start.year=start.year)
  lusetetthetNaboObj<-extract.and.expand(AllFarmsData$Af.ext,loc.no=loc.no,start.year=start.year)
  smittepress<-smittepressObj$daily.series
  lusetetthetNabo<-lusetetthetNaboObj$daily.series
  
  ind<-which(colnames(AllFarmsData$salinity)==loc.no)
  if (length(ind)==0) {
    print("loc.no not found in salinity! Returns NULL")
    return(NULL)
  }
  ymd.sal<-rownames(AllFarmsData$salinity)
  salinity<-as.vector(AllFarmsData$salinity[,ind])
  names(salinity)<-ymd.sal
  first.date.with.salinity<-ymd.sal[1]
  last.date.with.salinity<-tail(ymd.sal,1)


  temp<-temp[as.numeric(names(temp))>=as.numeric(first.date.in.pred.per)]
  smittepress<-smittepress[as.numeric(names(smittepress))>=as.numeric(first.date.in.pred.per)]
  lusetetthetNabo<-lusetetthetNabo[as.numeric(names(lusetetthetNabo))>=as.numeric(first.date.in.pred.per)]
  salinity<-salinity[as.numeric(names(salinity))>=as.numeric(first.date.in.pred.per)]

  if (as.numeric(last.date.with.temp)>=as.numeric(last.date.in.pred.per)) {
    temp<-temp[as.numeric(names(temp))<=as.numeric(last.date.in.pred.per)]
    smittepress<-smittepress[as.numeric(names(smittepress))<=as.numeric(last.date.in.pred.per)]
    lusetetthetNabo<-lusetetthetNabo[as.numeric(names(lusetetthetNabo))<=as.numeric(last.date.in.pred.per)]
  } else {
    n.diff<-forecast.horizon.days-length(temp)
    temp<-c(temp,rep(NA,n.diff+1))
    smittepress<-c(smittepress,rep(NA,n.diff+1))
    lusetetthetNabo<-c(lusetetthetNabo,rep(NA,n.diff+1))
    names(temp)<-ymd
    names(smittepress)<-ymd
    names(lusetetthetNabo)<-ymd
  }
  
  if (as.numeric(last.date.with.salinity)>=as.numeric(last.date.in.pred.per)) {
    salinity<-salinity[as.numeric(names(salinity))<=as.numeric(last.date.in.pred.per)]
  } else {
    n.diff<-forecast.horizon.days-length(salinity)
    salinity<-c(salinity,rep(NA,n.diff+1))
    names(salinity)<-ymd
  }


  divinfo<-FullData$divinfo
  divinfo$time.names<-ymd
  divinfo$n.times<-length(ymd)
  divinfo$first.date.with.fish<-ymd[1]
  divinfo$last.date.with.fish<-tail(ymd,1)
  divinfo$movement.from<-unlist(NULL)
  divinfo$movement.to<-unlist(NULL)
  divinfo$med.names<-unlist(med.names)
  divinfo$n.med<-n.med
  divinfo$med.names.not.in.MedicineInfo<-unlist(med.names.not.in.MedicineInfo)
  divinfo$MedicineInfo<-MedicineInfo
  FutureFullData<-list(calendar=calendar,
                       lice=lice,
                       vekt=vekt,
                       antall=antall,
                       biom=biom,
                       behand=behand,
                       cleaner.fish.stocked=cleaner.fish.stocked,
                       temp=temp,
                       salinity=salinity,
                       w=w,
                       S=S,
                       smittepress=smittepress,
                       lusetetthetNabo=lusetetthetNabo,
                       divinfo=divinfo)

  covariates.future<-make.covariates(FutureFullData)
  treatment.future<-covariates.future$treatment 
  treatment.tot<-make.treatment(FullData=FullData,time.names.future=ymd)

  merd.names.future<-FutureFullData$divinfo$merd.names
  med.names.future<-FutureFullData$divinfo$med.names

  if (is.null(treatment.tot) & is.null(treatment.future)) {
    covariates.future$treatment<-unlist(NULL)
  } else if (!is.null(treatment.tot) & is.null(treatment.future)) {
    covariates.future$treatment<-treatment.tot
  } else if (is.null(treatment.tot) & !is.null(treatment.future)) {
    covariates.future$treatment<-treatment.future
  } else {
    for(merd in merd.names.future) {
      if (length(med.names.future)>0) {
        for(med in med.names.future) {
          if(treatment.future[[merd]][[med]][1]!="NR") {
            if (is.null(treatment.tot[[merd]][[med]])) {
              treatment.tot[[merd]][[med]]<-"NR"
            }
            if (treatment.tot[[merd]][[med]][1]=="NR") {
              treatment.tot[[merd]][[med]]<-list()
            }
            
            treatno.names.future<-names(treatment.future[[merd]][[med]])
            n.treat.future<-length(treatno.names.future)
            treatno.names.new<-paste(treatno.names.future,".future",sep="")
            for (l in 1:n.treat.future) {
              treatment.tot[[merd]][[med]][[treatno.names.new[l]]]<-treatment.future[[merd]][[med]][[treatno.names.future[l]]]
            }
          }
        }
      }
    }
    covariates.future$treatment<-treatment.tot
  }

  FutureFullData$covariates<-covariates.future

  FutureFullData
}
