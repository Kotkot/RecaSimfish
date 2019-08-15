split.FullData<-function(FullData,split.date,first=T) {

  time.names<-FullData$divinfo$time.names
  n.times<-FullData$divinfo$n.times
  if (!(split.date>=time.names[1] & split.date<=tail(time.names,1))) {
    print("Illegal split.date in function split.FullData, returns NULL!")
    return(NULL)
  }
          

  if (first) {
    ind<-(1:n.times)[time.names<=split.date]
  } else {
    ind<-(1:n.times)[time.names>=split.date]
  }
  
  FD<-FullData

  FD$divinfo$time.names<-FullData$divinfo$time.names[ind]
  FD$divinfo$n.times<-length(FD$divinfo$time.names)

  FD$calendar<-FullData$calendar[ind,,drop=F]

  for (merdnumb in 1:FullData$divinfo$n.merds) {
    if (first) {
      FD$lice[[merdnumb]]<-FullData$lice[[merdnumb]][rownames(FullData$lice[[merdnumb]])<=split.date,,drop=F]
    } else {
      FD$lice[[merdnumb]]<-FullData$lice[[merdnumb]][rownames(FullData$lice[[merdnumb]])>=split.date,,drop=F]
    }      
  }
  FD$vekt<-FullData$vekt[ind,,drop=F]
  FD$antall<-FullData$antall[ind,,drop=F]
  FD$biom<-FullData$biom[ind,,drop=F]
  
  FD$behand<-FullData$behand[ind,,,drop=F]
  for ( i in 1:length(FullData$cleaner.fish.stocked)) {
    FD$cleaner.fish.stocked[[i]]<-FullData$cleaner.fish.stocked[[i]][ind,,drop=F]
  }
  FD$w<-FullData$w[ind,,,drop=F]
  FD$S<-FullData$S[ind,,drop=F]
  FD$temp<-FullData$temp[ind]
  FD$salinity<-FullData$salinity[ind]
  FD$smittepress<-FullData$smittepress[ind]
  FD$lusetetthetNabo<-FullData$lusetetthetNabo[ind]

  if (!is.null(FullData$covariates)) {
    FD$covariates$Tmean<-FullData$covariates$Tmean[ind,,drop=F]
    
    for (i in 1:length(FullData$covariates$treatment)) { ## cages
      if (is.list(FullData$covariates$treatment[[i]])) {
        for (j in 1:length(FullData$covariates$treatment[[i]])) { ## treatment types
          if (is.list(FullData$covariates$treatment[[i]][[j]])) {
            for (k in 1:length(FullData$covariates$treatment[[i]][[j]])) { ## treatment episodes of the given type
              for (l in 1:length(FullData$covariates$treatment[[i]][[j]][[k]])) { ## stages CH, PA, A
                if (is.matrix(FullData$covariates$treatment[[i]][[j]][[k]][[l]])) {
                  FD$covariates$treatment[[i]][[j]][[k]][[l]]<-FullData$covariates$treatment[[i]][[j]][[k]][[l]][ind,,drop=F]
                }
              }
            }
          }
        }
      }
    }

    FD$covariates$reprod<-FullData$covariates$reprod[ind,,drop=F]
  }
  
  FD
}
