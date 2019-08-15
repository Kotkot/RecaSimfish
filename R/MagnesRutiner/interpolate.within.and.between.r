interpolate.within.and.between<-function(x,S,Dist,Dist.ind,seaway.to.time.factor=1,interpolate.all=FALSE) {
### if x==NA and S==1, it should be interpolated between farms for the same time point
### if (interpolate.all=TRUE, all vales are interpolated, also the non-missing ones.

  q<-ncol(x)			       
  T<-nrow(x)
  
  Wb<-(1/Dist)*Dist.ind  ## Weight matrix based on seaway distances between farms
  diag(Wb)<-0
  Wb<-seaway.to.time.factor*Wb  ## Observations at seaway distance 1 have seaway.to.time.factor more weight
                                ## than observations at time distance 1
  tmp<-1:T
  Dist.times<-abs(outer(tmp,tmp,"-")) ## Time distances within farms
  Ww<-(1/Dist.times)  ## Weight matrix based on time distances within farms
  diag(Ww)<-0
  

  x.imp<-x
  for (i in 1:q) {
    wbi<-Wb[i,]
    for (j in 1:T) {
      if (interpolate.all) {
        interpol.j.i<-TRUE
      } else {
        interpol.j.i<-is.na(x[j,i])
      }
      if (interpol.j.i) {
        ind.j.mis<-is.na(x[j,])
        ind.j.Seq0<-(S[j,]==0)
        wb<-wbi
        wb[ind.j.mis]<-0
        wb[ind.j.Seq0]<-0
        sum.wb<-sum(wb)
        if (is.na(sum.wb)) {
          browser()
        }
        if (sum.wb >0) {
          imp.b.x<-sum(na.omit(as.vector(wb*x[j,])))/sum.wb
        } else {
###          browser()
          imp.b.x<-NA
        }

        wwj<-Ww[j,]
        ind.i.mis<-is.na(x[,i])
        ind.i.Seq0<-(S[,i]==0)
        ww<-wwj
        ww[ind.i.mis]<-0
        ww[ind.i.Seq0]<-0
        sum.ww<-sum(ww)
        if (is.na(sum.ww)) {
          browser()
        }
        if (sum.ww >0) {
          imp.w.x<-sum(na.omit(as.vector(ww*x[,i])))/sum.ww
        } else {
###          browser()
          imp.w.x<-NA
        }
        if (!is.na(imp.w.x) & !is.na(imp.b.x)) {
          a<-sum.ww/(sum.ww+sum.wb)
          x.imp[j,i]<-a*imp.w.x+(1-a)*imp.b.x
        } else if (!is.na(imp.w.x)) {
          x.imp[j,i]<-imp.w.x
        } else {
          x.imp[j,i]<-imp.b.x
        }
      }
    }
  }

  x.imp
}
   
