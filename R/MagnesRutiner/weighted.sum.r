weighted.sum<-function(x,S,Dist,Dist.ind,sum.w.to.1) {

  ## From PlosOne-paper 2013
  phi1<--0.618  ## = -0.351/0.568, cfr. PlosOne paper
  phi2<-0.568

  x[S==0]<-0
  x[is.na(x)]<-0
    
  D<-exp(phi1*Dist^phi2)
  D<-D*Dist.ind

  if (sum.w.to.1) {
    sum.w<-apply(D,1,sum)
    sum.w[sum.w==0]<-1e100
    
    D<-D/sum.w
  }

  ws<-x%*%D

  ws
}
