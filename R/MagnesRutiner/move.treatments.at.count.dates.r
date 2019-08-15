move.treatments.at.count.dates<-function(FullData) {

  max.timediff<-15
  min.reldiff<-0.6
  min.reldiff.2<-0.4
  min.sum<-0.2
  min.val<-0.2

  treat<-FullData$behand

  divinfo<-FullData$divinfo
  feed.med<-divinfo$MedicineInfo$medicine[divinfo$MedicineInfo$feed=="Y"]
  
  
  if (divinfo$n.med>0) {
    lice<-FullData$lice
    ##antall<-FullData$antall
    treat.dates<-dimnames(treat)[[1]]
    n.treat<-length(treat.dates)
    for (med in divinfo$med.names) {
      treat.one.med<-treat[,,med]
      if (!(med%in%feed.med)) {
        CH.effect<-divinfo$MedicineInfo[divinfo$MedicineInfo$medicine==med,]$CH.effect
        PA.effect<-divinfo$MedicineInfo[divinfo$MedicineInfo$medicine==med,]$PA.effect
        A.effect<-divinfo$MedicineInfo[divinfo$MedicineInfo$medicine==med,]$A.effect
        for (merd in divinfo$merd.names) {
          lice.counts<-lice[[merd]]
          n.counts<-nrow(lice.counts)
          treat1.dates<-treat.dates[treat.one.med[,merd]==1]
          n.treat1<-length(treat1.dates)
          if (n.treat1>0 & n.counts>0) {
            count.dates<-rownames(lice.counts)
            count1.dates<-count.dates[count.dates%in%treat1.dates]
            if (length(count1.dates)>0) {  ## at least one count date equal to a treatment date
              CHOMAF<-0
              if (CH.effect) {
                CHOMAF<-CHOMAF+lice.counts[,"CH"]
              }
              if (PA.effect) {
                CHOMAF<-CHOMAF+lice.counts[,"OM"]
              }
              if (A.effect) {
                CHOMAF<-CHOMAF+lice.counts[,"Af"]
              }
              for (d in 1:length(count1.dates)) {
                date1<-count1.dates[d]
                print(paste("Treatment ",med," and lice count on same day ",date1," in cage ",merd,sep=""))
                count.ind<-(1:n.counts)[count.dates==date1]
                if (count.ind>1 & count.ind<(n.counts-1)) { ## lice counts before and after count date
                  CHOMAF.tmin1<-CHOMAF[count.ind-1]/lice.counts[count.ind-1,"n.fish"]
                  CHOMAF.t<-CHOMAF[count.ind]/lice.counts[count.ind,"n.fish"]
                  CHOMAF.tplus1<-CHOMAF[count.ind+1]/lice.counts[count.ind+1,"n.fish"]

                  treat.t.ind<-(1:n.treat)[treat.dates==date1]
                  treat.tmin1.ind<-(1:n.treat)[treat.dates==count.dates[count.ind-1]]
                  treat.tplus1.ind<-(1:n.treat)[treat.dates==count.dates[count.ind+1]]

                  timediff.back<-treat.t.ind-treat.tmin1.ind
                  timediff.ahead<-treat.tplus1.ind-treat.t.ind

                  if (timediff.back<=max.timediff & timediff.ahead<=max.timediff) {
                    CHOMAF.sum<-CHOMAF.tmin1+CHOMAF.t+CHOMAF.tplus1
                    if (CHOMAF.sum>min.sum & CHOMAF.t<=min.reldiff*CHOMAF.tmin1 &
                        (CHOMAF.t<min.val | (CHOMAF.t>=min.val & CHOMAF.tplus1>=min.reldiff*CHOMAF.t))) {
                      print("Treatment are moved one day back")
                      treat[treat.t.ind-1,merd,med]<-1
                      treat[treat.t.ind,merd,med]<-0
                     }
                  }
                } else if (count.ind>1 & count.ind==n.counts) { ## lice counts before, but not after count date
                  CHOMAF.tmin1<-CHOMAF[count.ind-1]/lice.counts[count.ind-1,"n.fish"]
                  CHOMAF.t<-CHOMAF[count.ind]/lice.counts[count.ind,"n.fish"]

                  treat.t.ind<-(1:n.treat)[treat.dates==date1]
                  treat.tmin1.ind<-(1:n.treat)[treat.dates==count.dates[count.ind-1]]

                  timediff.back<-treat.t.ind-treat.tmin1.ind

                  if (timediff.back<=max.timediff) {
                    CHOMAF.sum<-CHOMAF.tmin1+CHOMAF.t
                    if (CHOMAF.sum>min.sum & CHOMAF.t<=min.reldiff.2*CHOMAF.tmin1) {
                      print("Treatment are moved one day back")
                      treat[treat.t.ind-1,merd,med]<-1
                      treat[treat.t.ind,merd,med]<-0
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
          
  treat       
}
