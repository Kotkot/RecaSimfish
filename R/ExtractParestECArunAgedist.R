##' Extract parameters from a fitted ECA model (fit object); to be used for computing the age distribution
##' @title ExtractParestECArunAgedist
##' @param fit Output (list object) from a fitted ECA model with elements: "age"       "lga"       "hsz"       "nonlin"    "lga.cc"    "nonlin.cc" "wgl"       "wgl.cc"    "split"     "k"
##' @return  A list (parameters for computing the age distribution)
##' @author Ingunn Fride Tvete
##' @export
ExtractParestECArunAgedist<-function(fit) {
  A<-dim(fit$age$Int$Const)[1]
  S<-dim(fit$age$Int$seas)[2]
  G<-dim(fit$age$Int$gear)[2]
  
  alpha_const<-rep(0,A)
  for (i in 1:A)
  {
    alpha_const[i]<-mean(fit$age$Int$Const[i,1,])
  }
  alpha_season<-matrix(0,nrow=A,ncol=S)
  for (i in 1:A)
  {
    for (j in 1:S)
    {
      alpha_season[i,j]<-mean(fit$age$Int$seas[i,j,])
    }
  }
  alpha_gear<-matrix(0,nrow=A,ncol=G)
  for (i in 1:A)
  {
    for (j in 1:G)
    {
      alpha_gear[i,j]<-mean(fit$age$Int$gear[i,j,])
    }
  }
  
  tau_region<-mean(fit$age$Int$tau.area)
  tau_boat<-mean(fit$age$Int$tau.boat)
  tau_cell<-mean(fit$age$Int$tau.cell)
  tau_unit<-mean(fit$age$Int$tau.haul)
  
  dataout<-list()
  dataout$alpha_const<-alpha_const
  dataout$alpha_season<-alpha_season
  dataout$alpha_gear<-alpha_gear
  dataout$tau_region<-tau_region
  dataout$tau_boat<-tau_boat
  dataout$tau_cell<-tau_cell
  dataout$tau_unit<-tau_unit
  
  phi_age<-mean(fit$age$Int$ar)
  dataout$phi_age<-phi_age
  
  return(dataout)
  
}
