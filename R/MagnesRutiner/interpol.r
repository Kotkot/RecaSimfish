interpol<-function(Ydat,DivInfo,interpol.options) {

  ym<-rownames(Ydat)
  week.in.year<-substr(ym,5,6)
  week.in.year[week.in.year==53]<-52

  n.times<-nrow(Ydat)
  n.loc<-ncol(Ydat)
  
  NA.val<-interpol.options$NA.val ## value set to NA
  min.equal.val<-interpol.options$min.equal.val ## minimum number of subsequent equal values to be set to NA
  no.na.interpol.w<-interpol.options$no.na.interpol.w ## maximum number of subsequent NA values to be interpolated within farm
  max.outlier.val<-interpol.options$max.outlier.val
  seaway.to.time.factor<-interpol.options$seaway.to.time.factor

  ##
  ## Values equal NA.val are set to NA
  ## 
  if (!is.null(NA.val)) {
    Ydat[Ydat==NA.val]<-NA
  }
  
  ##
  ## If min.equal.val or more values are exactly equal, they are set to NA
  ##
  if (!is.null(min.equal.val) & min.equal.val>1) {
    for (i in 1:n.loc) {
      
      ## First time point
      last.val<-Ydat[1,i]
      if (!is.na(last.val)) {
        no.equal.val<-1
      } else {
        no.equal.val<-0
      }
      reached.max<-F
      
      ## From second time point
      for (tt in 2:n.times) {
        this.val<-Ydat[tt,i]
        if (!is.na(this.val)) {
          if (!is.na(last.val)) {
            if (this.val==last.val) {
              no.equal.val<-no.equal.val+1
              if (no.equal.val==min.equal.val) {
                reached.max<-T
              }
            } else {##this.val!=last.val
              if (reached.max) {
                Ydat[(tt-no.equal.val):(tt-1),i]<-NA
              }
              last.val<-this.val
              no.equal.val<-1
              reached.max<-F
            }  
          } else {##last.val==NA
            last.val<-this.val
            no.equal.val<-1
            reached.max<-F ## not necessary?
          }
        } else {##this.val==NA
          if (reached.max) {
            Ydat[(tt-no.equal.val):(tt-1),i]<-NA
          }
          last.val<-NA
          no.equal.val<-0
          reached.max<-F
        }
      }

      ## At the end
      if (!is.na(this.val)) {
        if (reached.max) {
          Ydat[(tt-no.equal.val):(tt-1),i]<-NA
        }
      }
    }
  }


  ##
  ## Up to no.na.interpol.w subsequent missing values are linearly interpolated within farm
  ##
  if (no.na.interpol.w>0) {
    no.na.interpol.w.at.end<-ceiling(no.na.interpol.w/2)
    for (i in 1:n.loc) {
      
      ## First time point
      this.val<-Ydat[1,i]
      if (!is.na(this.val)) {
        no.miss.val<-0
        last.known.val<-last.val
      } else {
        no.miss.val<-1
        last.known.val<-NA
      }
      last.val<-this.val
      
      ## From second time point
      for (tt in 2:n.times) {
        this.val<-Ydat[tt,i]
        if (!is.na(this.val)) {##this.val observed
          if (is.na(last.val)) {##last.val==NA
            if (no.miss.val<=no.na.interpol.w & !is.na(last.known.val)) {
              delta<-(this.val-last.known.val)/(no.miss.val+1)
              for (j in 1:no.miss.val) {
                Ydat[tt-j,i]<-this.val-j*delta
              }
            }
          }
          no.miss.val<-0
          last.known.val<-this.val
        } else {##this.val==NA
          no.miss.val<-no.miss.val+1
        }
        last.val<-this.val
      }
      
      ## Last no.na.interpol.w.at.end values carried forward if missing at end
      if (is.na(Ydat[n.times,i])) {
        if (no.miss.val<=no.na.interpol.w.at.end) {
          for (j in 1:no.miss.val) {
            Ydat[n.times+1-j,i]<-last.known.val
          }
        }
      }
    }
  }
  

  ##
  ## Interpolate between farms at same time point, using seaway distances
  ##
  S<-Ydat
  S[,]<-1

  ##
  ## Outliers, where observed or interpolated within are more than max.outlier.val from interpolate between,
  ## are set to NA
  ##
  if (!is.null(max.outlier.val) & max.outlier.val>0) {
###    Ydat.interpol<-interpolate.between(x=Ydat,S=S,Dist=DivInfo$Dist,Dist.ind=DivInfo$Dist.ind,interpolate.all=T)
    Ydat.interpol<-interpolate.within.and.between(x=Ydat,S=S,
                                                  Dist=DivInfo$Dist,
                                                  Dist.ind=DivInfo$Dist.ind,
                                                  seaway.to.time.factor=seaway.to.time.factor,
                                                  interpolate.all=T)
    Ydat.interpol<-interpolate.within.and.between(x=Ydat.interpol,S=S,
                                                  Dist=DivInfo$Dist,
                                                  Dist.ind=DivInfo$Dist.ind,
                                                  seaway.to.time.factor=seaway.to.time.factor,
                                                  interpolate.all=F)
    diff<-sqrt(mean((Ydat-Ydat.interpol)^2,na.rm=T))
    print(diff)
    Ydat[abs(Ydat-Ydat.interpol)>max.outlier.val]<-NA
  }

  ##
  ## interpolate missing values from observed values at neighbours at the same time point
  ##
###  Ydat<-interpolate.between(x=Ydat,S=S,Dist=DivInfo$Dist,Dist.ind=DivInfo$Dist.ind,interpolate.all=F)
  Ydat.interpol<-interpolate.within.and.between(x=Ydat,S=S,Dist=DivInfo$Dist,Dist.ind=DivInfo$Dist.ind,
                                       seaway.to.time.factor=seaway.to.time.factor,interpolate.all=F)
  Ydat.interpol<-interpolate.within.and.between(x=Ydat.interpol,S=S,Dist=DivInfo$Dist,Dist.ind=DivInfo$Dist.ind,
                                       seaway.to.time.factor=seaway.to.time.factor,interpolate.all=F)

###  koordinater trengs?

###  Mulig uten koordinater:
###  sortere alle 1) fra laves til høyest gjennomsnitt, 2) fra lavest til høyest uke for maksimum

  Ydat.interpol
}
