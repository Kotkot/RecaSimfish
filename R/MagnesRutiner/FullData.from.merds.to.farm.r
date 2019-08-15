##' Aggregates a FullData object on cage level to a FullData object on farm level.
##'
##' @title Aggregates FullData from cage level to farm level
##' @inheritParams dummy.for.documentation
##' @return A list of type FullData
##' @author Magne Aldrin
##' @export
FullData.from.merds.to.farm<-function(FullData) {

  TotData<-FullData
  TotData$divinfo$n.merds<-1
  TotData$divinfo$merd.names<-"AllMerds"
  
  n.merds<-FullData$divinfo$n.merds
  TotData$antall<-as.matrix(apply(FullData$antall,1,sum))
  colnames(TotData$antall)<-"AllMerds"
  if (!is.null(FullData$behand)) {
    TotData$behand<-FullData$behand[,1,,drop=F]
    colnames(TotData$behand)<-"AllMerds"
  }
  if (is.list(TotData$cleaner.fish.stocked)) {
    for (i in 1:length(TotData$cleaner.fish.stocked)) {
      TotData$cleaner.fish.stocked[[i]]<-FullData$cleaner.fish.stocked[[i]][,1,drop=F]
      colnames(TotData$cleaner.fish.stocked[[i]])<-"AllMerds"
    }
  } else {
    TotData$cleaner.fish.stocked<-FullData$cleaner.fish.stocked[,1,drop=F]
    colnames(TotData$cleaner.fish.stocked)<-"AllMerds"
  }
  
  ww<-FullData$antall/as.vector(TotData$antall)
  ##  ww[is.na(ww)]<-0
  FullData$vekt[FullData$antall==0]<-0
  TotData$vekt<-FullData$vekt[,1,drop=F]*ww[,1]
  colnames(TotData$vekt)<-"AllMerds"
  if (n.merds>1) {
    for (merdnumb in 2:n.merds) {
      TotData$vekt=TotData$vekt+FullData$vekt[,merdnumb]*ww[,merdnumb]
      if (!is.null(FullData$behand)) {
        TotData$behand[,1,]<-TotData$behand[,1,]+FullData$behand[,merdnumb,]
      }
      if (is.list(TotData$cleaner.fish.stocked)) {
        for (i in 1:length(TotData$cleaner.fish.stocked)) {
          TotData$cleaner.fish.stocked[[i]][,1]<-TotData$cleaner.fish.stocked[[i]][,1]+FullData$cleaner.fish.stocked[[i]][,merdnumb]
        }
      } else {
        TotData$cleaner.fish.stocked[,1]<-TotData$cleaner.fish.stocked[,1]+FullData$cleaner.fish.stocked[,merdnumb]
      }
    }
  }

  if (is.null(FullData$lice)) {
    TotData$lice<-NULL
  } else {
    TotData$lice<-vector("list",1)
    names(TotData$lice)<-"AllMerds"
    
    dates<-rownames(FullData$lice[[1]])
    for (merdnumb in 2:n.merds) {
      dates<-c(dates,rownames(FullData$lice[[merdnumb]]))
    }
    if (length(dates)>0) {
      dates.un<-sort(unique(dates))
      n.d<-length(dates.un)
      TotData$lice[[1]]<-matrix(0,nrow=n.d,ncol=4)
      rownames(TotData$lice[[1]])<-dates.un
      colnames(TotData$lice[[1]])<-colnames(FullData$lice[[1]])
      for (d in dates.un) {
        sumw<-0
        for (merdnumb in 1:n.merds) {
          if (d %in% rownames(FullData$lice[[merdnumb]]) & d %in% rownames(FullData$antall)) {
            x<-FullData$lice[[merdnumb]][d,-1]/FullData$lice[[merdnumb]][d,"n.fish"]
            w<-FullData$antall[d,merdnumb]/TotData$antall[d,1]
            sumw<-sumw+w
            TotData$lice[[1]][d,-1]<-TotData$lice[[1]][d,-1]+x*w
            TotData$lice[[1]][d,"n.fish"]<-TotData$lice[[1]][d,"n.fish"]+FullData$lice[[merdnumb]][d,"n.fish"]
          }
        }
        if (sumw>0) {
        TotData$lice[[1]][d,-1]<-TotData$lice[[1]][d,-1]/sumw
        }
      }
      TotData$lice[[1]][,-1]<-TotData$lice[[1]][,-1]*TotData$lice[[1]][,"n.fish"]
    } else {
      TotData$lice[[1]]<-NULL
    }
  }

  TotData
}
