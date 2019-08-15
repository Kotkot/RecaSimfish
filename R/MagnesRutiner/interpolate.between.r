interpolate.between<-function(x,S,Dist,Dist.ind,interpolate.all=FALSE) {
### if x==NA and S==1, it should be interpolated between farms for the same time point
### if (interpolate.all=TRUE, all vales are interpolated, also the non-missing ones.

  W<-(1/Dist)*Dist.ind
  diag(W)<-0
  q<-ncol(x)			       
  T<-nrow(x)
  x.imp<-x
  for (i in 1:q) {
    wi<-W[i,]
    for (j in 1:T) {
      if (interpolate.all) {
        interpol.j.i<-TRUE
      } else {
        interpol.j.i<-is.na(x[j,i])
      }
      if (interpol.j.i) {
        ind.j.mis<-is.na(x[j,])
        ind.j.Seq0<-(S[j,]==0)
        w<-wi
        w[ind.j.mis]<-0
        w[ind.j.Seq0]<-0
        sum.w<-sum(w)
        if (is.na(sum.w)) {
          browser()
        }
        if (sum.w >0) {
          imp.x<-sum(na.omit(as.vector(w*x[j,])))/sum.w
        } else {
          imp.x<-NA
        }
        
        x.imp[j,i]<-imp.x
      }
    }
  }

  x.imp
}
   
