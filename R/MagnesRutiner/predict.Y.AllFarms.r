predict.Y.AllFarms<-function(Ydat,coord,rn,daily,nsim,intervals,logtrans,small.const) {

  if (logtrans) {
    Ydat<-log(Ydat+small.const)
  }
  ym<-rownames(Ydat)
  if (daily) {
    startyear<-as.numeric(substr(ym[1],1,4))
    startmonth<-as.numeric(substr(ym[1],5,6))
    startday<-as.numeric(substr(ym[1],7,8))
    endyear<-as.numeric(substr(tail(ym,1),1,4))
    endmonth<-as.numeric(substr(tail(ym,1),5,6))
    endday<-as.numeric(substr(tail(ym,1),7,8))
    time.in.year<-myyeardays(startyear,startmonth,startday,endyear,endmonth,endday)
    time.in.year[time.in.year==366]<-365
  } else {
    time.in.year<-as.numeric(substr(ym,5,6))
    time.in.year[time.in.year==53]<-52
  }
  q<-ncol(Ydat)
  T<-nrow(Ydat)
  ##q.orig<-q
  ##ind.all.miss<-(1:q)[is.na(apply(Ydat,2,sum,na.rm=T))]
  ##Ydat<-Ydat[,-ind.all.miss]
  ##q<-ncol(Ydat)
  
  time.in.year<-matrix(time.in.year,nrow=T,ncol=q,byrow=F)
  lat<-coord[,"lat"]
  lon<-coord[,"lon"]
  lat<-matrix(lat,nrow=T,ncol=q,byrow=T)
  lon<-matrix(lon,nrow=T,ncol=q,byrow=T)
  y<-as.vector(Ydat)
  time.in.year<-as.vector(time.in.year)
  lat<-as.vector(lat)
  lon<-as.vector(lon)

  df.hist<-data.frame(y=y,time.in.year=time.in.year,lat=lat,lon=lon)

  if (daily) {
    ind8<-seq(1,T,by=8)
    ind8<-ind8+(T-last(ind8))
    time.in.year<-matrix(time.in.year,nrow=T,ncol=q,byrow=F)
    lat<-coord[,"lat"]
    lon<-coord[,"lon"]
    lat<-matrix(lat,nrow=T,ncol=q,byrow=T)
    lon<-matrix(lon,nrow=T,ncol=q,byrow=T)
    y<-as.vector(Ydat[ind8,])
    time.in.year<-as.vector(time.in.year[ind8,])
    lat<-as.vector(lat[ind8,])
    lon<-as.vector(lon[ind8,])

    df.hist8<-data.frame(y=y,time.in.year=time.in.year,lat=lat,lon=lon)

    gamobj<-gam(y~te(time.in.year,lat,lon),data=df.hist8)
  } else {
    gamobj<-gam(y~te(time.in.year,lat,lon),data=df.hist)
  }    


  
  ### Estimating model for residuals
  pred<-predict(gamobj,newdata=df.hist)
  
  Res<-matrix(df.hist$y-pred,nrow=T)
  res.mean<-apply(Res,2,mean,na.rm=T)
  Res.mean<-matrix(res.mean,byrow=T,ncol=q,nrow=T)
  Res.cent<-Res-Res.mean
  res.cent.last<-Res.cent[T,]

  
  X1<-rbind(rep(NA,q),Res.cent[-T,])
  X2<-rbind(rep(NA,q),rep(NA,q),Res.cent[-((T-1):T),])
  X3<-rbind(rep(NA,q),rep(NA,q),rep(NA,q),Res.cent[-((T-2):T),])

  df.res.hist<-data.frame(df.hist,z=as.vector(Res.cent),x1=as.vector(X1),x2=as.vector(X2),x3=as.vector(X3))

  ##gamobj.res<-gam(z~te(x1,time.in.year),data=df.res.hist)
  lmobj.res<-lm(z~x1-1,data=df.res.hist) # AR(1) model for residuals
  phi1<-lmobj.res$coef[1]
  pred.res<-predict(lmobj.res,newdata=df.res.hist)
  ## Eps are residuals from the AR model and will later be used for
  ## bootstrapping to keep variance for each farm and correlation between farms
  Eps<-matrix(df.res.hist$z-pred.res,nrow=T)
  ind.time.all.miss<-(1:T)[is.na(apply(Eps,1,sum,na.rm=T))]
  if (length(ind.time.all.miss)>0) {
    Eps<-Eps[-ind.time.all.miss,]
  }
  T.obs<-nrow(Eps)
  eps.orig<-na.omit(as.vector(Eps))
  nobs<-apply(!is.na(Eps),2,sum)
  for (i in 1:q) {
    if (nobs[i]==0) {
      Eps[,i]<-sample(eps.orig,T.obs,replace=T)
    } else if (nobs[i]<T.obs) {
      ind.mis<-(1:T.obs)[is.na(Eps[,i])]
      Eps[ind.mis,i]<-sample(Eps[-ind.mis,i],(T.obs-nobs[i]),replace=T)
    }
  }

  ## Prediction ahead in time
  ym<-rn
  if (daily) {
    startyear<-as.numeric(substr(ym[1],1,4))
    startmonth<-as.numeric(substr(ym[1],5,6))
    startday<-as.numeric(substr(ym[1],7,8))
    endyear<-as.numeric(substr(tail(ym,1),1,4))
    endmonth<-as.numeric(substr(tail(ym,1),5,6))
    endday<-as.numeric(substr(tail(ym,1),7,8))
    time.in.year<-myyeardays(startyear,startmonth,startday,endyear,endmonth,endday)
    time.in.year[time.in.year==366]<-365
  } else {
    time.in.year<-as.numeric(substr(ym,5,6))
    time.in.year[time.in.year==53]<-52
  }
  T<-length(time.in.year)
  time.in.year<-matrix(time.in.year,nrow=T,ncol=q,byrow=F)
  time.in.year<-as.vector(time.in.year)
  lat<-coord[,"lat"]
  lon<-coord[,"lon"]
  lat<-matrix(lat,nrow=T,ncol=q,byrow=T)
  lon<-matrix(lon,nrow=T,ncol=q,byrow=T)
  lat<-as.vector(lat)
  lon<-as.vector(lon)

  df.new<-data.frame(time.in.year=time.in.year,lat=lat,lon=lon)

  Ypred<-predict(gamobj,newdata=df.new)

  Ypred<-matrix(Ypred,nrow=T,ncol=q,byrow=F,dimnames=list(rn,colnames(Ydat)))
  Ypred.orig<-Ypred
  Res.mean<-matrix(res.mean,byrow=T,ncol=q,nrow=T)
  Ypred<-Ypred+Res.mean ## Add bias term for each farm

  Ysim<-array(NA,dim=c(T,q,nsim),dimnames=list(rn,colnames(Ydat),1:nsim))
  
  res0<-res.cent.last
  res0.obs<-na.omit(res0)
  nmiss<-sum(is.na(res.cent.last))
  ind.last.miss<-(1:q)[is.na(res.cent.last)]
  for (isim in 1:nsim) {
    ind.boot<-sample(1:T.obs,T,replace=T)
    Eps.boot<-Eps[ind.boot,]
    tt<-1
    if (nmiss>0) {
      res0[ind.last.miss]<-sample(res0.obs,nmiss,replace=T)
    }
    Ysim[tt,,isim]<-phi1*res0+Eps.boot[tt,]
    if (T>1) {
      for (tt in 2:T) {
        Ysim[tt,,isim]<-phi1*Ysim[tt-1,,isim]+Eps.boot[tt,]
      }
    }
    Ysim[,,isim]<-Ypred+Ysim[,,isim]  ## Add expectations and simulated residuals
  }

  if (logtrans) {
    Ysim<-exp(Ysim)
  }

  tmp<-calc.pred.stat(Ysim[,1,],intervals=intervals)
  stat.names<-colnames(tmp)
  nstat<-length(stat.names)
  pred.stat<-array(NA,dim=c(T,q,nstat),dimnames=list(rn,colnames(Ydat),stat.names))
  for (i in 1:q) {
    pred.stat[,i,]<-calc.pred.stat(Ysim[,i,],intervals=intervals)
  }

  rm(Ysim)
  
  pred.stat
}
