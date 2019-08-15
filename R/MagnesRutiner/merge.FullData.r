merge.FullData<-function(FullData.historical,FullData.future) {
  
  last.date.in.historical<-tail(FullData.historical$divinfo$time.names,1)
  first.date.in.future<-FullData.future$divinfo$time.names[1]
  if (last.date.in.historical!=first.date.in.future) {
    print("Incorrect time match - last.date.in.historical!=first.date.in.future, returns NULL!")
    return(NULL)
  }
  FD<-FullData.historical
  FD$divinfo$time.names<-c(FullData.historical$divinfo$time.names,FullData.future$divinfo$time.names[-1])
  FD$divinfo$n.times<-length(FD$divinfo$time.names)
  FD$divinfo$first.date.with.fish<-FullData.historical$divinfo$first.date.with.fish
  FD$divinfo$last.date.with.fish<-sort(c(FullData.historical$divinfo$last.date.with.fish,FullData.future$divinfo$last.date.with.fish))[2]

  
  for (merdnumb in 1:FullData.historical$divinfo$n.merds) {
    tmp<-rbind(FullData.historical$lice[[merdnumb]],FullData.future$lice[[merdnumb]])
    ind<-duplicated(rownames(tmp))
    tmp<-tmp[!ind,]  ## for removing possible duplicates if there is lice counts in last.date.in.historical=first.date.in.future
    FD$lice[[merdnumb]]<-tmp
  }

  FD$calendar<-rbind(FullData.historical$calendar,FullData.future$calendar[-1,,drop=F])
  FD$vekt<-rbind(FullData.historical$vekt,FullData.future$vekt[-1,,drop=F])
  FD$antall<-rbind(FullData.historical$antall,FullData.future$antall[-1,,drop=F])
  FD$biom<-rbind(FullData.historical$biom,FullData.future$biom[-1,,drop=F])
  FD$S<-rbind(FullData.historical$S,FullData.future$S[-1,,drop=F])

  FD$temp<-c(FullData.historical$temp,FullData.future$temp[-1])
  FD$salinity<-c(FullData.historical$salinity,FullData.future$salinity[-1])
  FD$smittepress<-c(FullData.historical$smittepress,FullData.future$smittepress[-1])
  FD$lusetetthetNabo<-c(FullData.historical$lusetetthetNabo,FullData.future$lusetetthetNabo[-1])

  FD$w<-abind::abind(FullData.historical$w,FullData.future$w[-1,,],along=1)
  
  if (is.list(FD$cleaner.fish.stocked)) {
    for (i in 1:length(FD$cleaner.fish.stocked)) {
      FD$cleaner.fish.stocked[[i]]<-rbind(FullData.historical$cleaner.fish.stocked[[i]],FullData.future$cleaner.fish.stocked[[i]][-1,,drop=F])
    }
  } else {
    FD$cleaner.fish.stocked<-rbind(FullData.historical$cleaner.fish.stocked,FullData.future$cleaner.fish.stocked[-1,,drop=F])
  }



  
  dim.historical<-dim(FullData.historical$behand)
  dimnames.historical<-dimnames(FullData.historical$behand)
  dim.future<-dim(FullData.future$behand)
  dimnames.future<-dimnames(FullData.future$behand)

  med.names<-unique(c(dimnames.historical[[3]],dimnames.future[[3]]))
  n.med<-length(med.names)
  FD$divinfo$med.names<-med.names
  FD$divinfo$n.med<-n.med

  if (n.med==0) {
    FD$behand<-NULL
  } else {
    FD$behand<-array(0,dim=c(FD$divinfo$n.times,FD$divinfo$n.merds,FD$divinfo$n.med),
                  dimnames=list(FD$divinfo$time.names,FD$divinfo$merd.names,FD$divinfo$med.names))
    if (FullData.historical$divinfo$n.med>0) {
      for (mn in FullData.historical$divinfo$med.names) {
        FD$behand[FullData.historical$divinfo$time.names,
               FullData.historical$divinfo$merd.names,
               FullData.historical$divinfo$med.names]<-
          FullData.historical$behand[FullData.historical$divinfo$time.names,
                                     FullData.historical$divinfo$merd.names,
                                     FullData.historical$divinfo$med.names]
      }
    }
    if (FullData.future$divinfo$n.med>0) {
      for (mn in FullData.future$divinfo$med.names) {
        FD$behand[FullData.future$divinfo$time.names[-1],
               FullData.future$divinfo$merd.names,
               FullData.future$divinfo$med.names]<-
          FullData.future$behand[FullData.future$divinfo$time.names[-1],
                                 FullData.future$divinfo$merd.names,
                                 FullData.future$divinfo$med.names]
      }
    }
  }

  FD$covariates<-make.covariates(FD)
  
  FD
}
