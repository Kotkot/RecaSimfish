##' ExtractParestECArunWGL: parameters for weigth given length
##'
##' @title ExtractParestECArunWGL
##' @param fit Output (list object) from a fitted ECA model with elements: "age"       "lga"       "hsz"       "nonlin"    "lga.cc"    "nonlin.cc" "wgl"       "wgl.cc"    "split"     "k"
##' @return A list (parameters for computing weigth given length)
##' @author Ingunn Fride Tvete
##' @export

ExtractParestECArunWGL<-function(fit) {
  S<-dim(fit$wgl$Int$seas)[2]
  G<-dim(fit$wgl$Int$gear)[2]
  
  delta_const<-mean(fit$wgl$Int$Const[1,1,])
  
  delta_season<-rep(0,S)
  for (i in 1:S)
  {
    delta_season[i]<-mean(fit$wgl$Int$seas[1,i,])
  }
  delta_gear<-rep(0,G)
  for (j in 1:G)
  {
    delta_gear[j]<-mean(fit$wgl$Int$gear[1,j,])
  }
  
  delta_1<-mean(fit$wgl$Slp$Const[1,1,])
  
  tau_nu_region<-mean(fit$wgl$Int$tau.area)
  tau_nu_boat<-mean(fit$wgl$Int$tau.boat)
  tau_nu_cell<-mean(fit$wgl$Int$tau.cell)
  tau_nu_unit<-mean(fit$wgl$Int$tau.haul)
  tau_nu_fish<-mean(fit$wgl$tau.obs)
  
  phi_WGL<-mean(fit$wgl$Int$ar)
  
  dataout<-list()
  dataout$delta_const<-delta_const
  dataout$delta_season<-delta_season
  dataout$delta_gear<-delta_gear
  dataout$delta_1<-delta_1
  dataout$tau_nu_region<-tau_nu_region
  dataout$tau_nu_boat<-tau_nu_boat
  dataout$tau_nu_cell<-tau_nu_cell
  dataout$tau_nu_unit<-tau_nu_unit
  dataout$tau_nu_fish<-tau_nu_fish
  dataout$phi_WGL<-phi_WGL
  
  return(dataout)
}
