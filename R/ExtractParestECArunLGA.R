##' ExtractParestECArunLGA: parameters for length given age 
##'
##' @title ExtractParestECArunLGA       
##' @param fit Output (list object) from a fitted ECA model with elements: "age"       "lga"       "hsz"       "nonlin"    "lga.cc"    "nonlin.cc" "wgl"       "wgl.cc"    "split"     "k"
##' @return  A list (parameters for computing length given age)
##' @author Ingunn Fride Tvete
##' @export
ExtractParestECArunLGA<-function(fit) {
  S<-dim(fit$lga$Int$seas)[2]
  G<-dim(fit$lga$Int$gear)[2]
  
  beta_const<-mean(fit$lga$Int$Const[1,1,])
  
  beta_season<-rep(0,S)
  for (i in 1:S)
  {
    beta_season[i]<-mean(fit$lga$Int$seas[1,i,])
  }
  beta_gear<-rep(0,G)
  for (j in 1:G)
  {
    beta_gear[j]<-mean(fit$lga$Int$gear[1,j,])
  }
  
  beta_1<-mean(fit$lga$Slp$Const)
  
  tau_epsilon_region<-mean(fit$lga$Int$tau.area)
  tau_epsilon_boat<-mean(fit$lga$Int$tau.boat)
  tau_epsilon_cell<-mean(fit$lga$Int$tau.cell)
  tau_epsilon_unit<-mean(fit$lga$Int$tau.haul)
  tau_epsilon_fish<-mean(fit$lga$tau.obs)
  
  phi_LGA<-mean(fit$lga$Int$ar)
  
  dataout<-list()
  
  dataout$beta_const<-beta_const
  dataout$beta_season<-beta_season
  dataout$beta_gear<-beta_gear
  dataout$beta_1<-beta_1
  dataout$tau_epsilon_region<-tau_epsilon_region
  dataout$tau_epsilon_boat<-tau_epsilon_boat
  dataout$tau_epsilon_cell<-tau_epsilon_cell
  dataout$tau_epsilon_unit<-tau_epsilon_unit
  dataout$tau_epsilon_fish<-tau_epsilon_fish
  dataout$phi_LGA<-phi_LGA
  
  return(dataout)
}
