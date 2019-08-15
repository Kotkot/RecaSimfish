extract.and.expand<-function(tmp,loc.no,start.year) {
### Extract one weekly time series from matrix or data frame (time x no. of farms) of time series,
### expand to one daily time series by linear interpolation
### Returns the daily time series + a corresponding calendar matrix
### Input:
### tmp: A matrix or data frame with dimension (time x no. of farms),
###      with weeks as row names (yyyyww) and farm id number as column names  
### loc.no: Id number of farm to be extracted
### start.year: First year 

  tmp<-as.matrix(tmp)
  tmp.names<-rownames(tmp)
  tmp.yy<-as.numeric(substr(tmp.names,1,4))
  tmp<-tmp[tmp.yy>=start.year,]
  tmp.names<-rownames(tmp)
  n.tmp<-nrow(tmp)
  
  tmp.tt<-as.vector(tmp[,as.character(loc.no)])
  tmp.yy<-as.numeric(substr(tmp.names,1,4))
  tmp.ww<-as.numeric(substr(tmp.names,5,6))

  tmp.cc<-GenKalender(tmp.yy[1],1,1,tmp.yy[n.tmp],12,31)
  n.tmp.cc<-nrow(tmp.cc)

  ind.1<-(1:n.tmp.cc)[tmp.cc[,"year"]==tmp.yy[1] & tmp.cc[,"weekyear"]==tmp.ww[1]]
  ind.1<-ind.1[1]   ### første dag i startuke i startåret
  ind.2<-(1:n.tmp.cc)[tmp.cc[,"year"]==tmp.yy[n.tmp] & tmp.cc[,"weekyear"]==tmp.ww[n.tmp]]
  ind.2<-tail(ind.2,1) ### siste dag i sluttuke i sluttåret

  calendar<-GenKalender(tmp.cc[ind.1,"year"],tmp.cc[ind.1,"month"],tmp.cc[ind.1,"day"],
                        tmp.cc[ind.2,"year"],tmp.cc[ind.2,"month"],tmp.cc[ind.2,"day"])
  n.days<-nrow(calendar)
### antar registrert temp er for ukedag 3, bortsett fra for første og siste uke
  tmp.dd<-rep(3,n.tmp)   
  
  ##tmp.dd[1]<-1##Korrigert 14/1-2019. Var feil.
  tmp.dd[1]<-calendar[1,"weekday"]
  tmp.dd[n.tmp]<-7
  
  tmp.ywwd<-ymd.num2char(tmp.yy,tmp.ww,tmp.dd)
### Må korrigere når dag 3 (evt. dag 1 hvis det er start.year) ikke er i riktig år,
### så da blir temperaturen antatt å være første dag i neste år
### Hardkodet fram til 2054
  tmp.ywwd[tmp.yy==2015 & tmp.ww==1 & tmp.dd<=3]<-"20150104"
  ##tmp.ywwd[tmp.yy==2021 & tmp.ww==1 & tmp.dd<=3]<-"20210105"## Fjernet 14/1-2019. Var feil.
  tmp.ywwd[tmp.yy==2026 & tmp.ww==1 & tmp.dd<=3]<-"20260104"
  tmp.ywwd[tmp.yy==2032 & tmp.ww==1 & tmp.dd<=3]<-"20320104"
  tmp.ywwd[tmp.yy==2037 & tmp.ww==1 & tmp.dd<=3]<-"20370104"
  tmp.ywwd[tmp.yy==2043 & tmp.ww==1 & tmp.dd<=3]<-"20430104"
  tmp.ywwd[tmp.yy==2054 & tmp.ww==1 & tmp.dd<=3]<-"20540104"


  calendar.ywwd<-ymd.num2char(calendar[,"year"],calendar[,"weekyear"],calendar[,"weekday"])
  ind<-match(tmp.ywwd,calendar.ywwd)
  temp<-rep(NA,n.days)
  temp[ind]<-tmp.tt
  x <- 1:n.days
  temp[-ind]<-approx(x[ind], temp[ind], x[-ind], method="linear")$y
  ymd.tot<-ymd.num2char(calendar[,"year"],calendar[,"month"],calendar[,"day"])
  names(temp)<-ymd.tot
  
  rownames(calendar)<-ymd.tot

  res<-list(daily.series=temp,calendar=calendar,ymd=ymd.tot)

  res
}
