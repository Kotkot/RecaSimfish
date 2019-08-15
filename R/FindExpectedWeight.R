##' Expected weigth in a given landing in sale notes data
##'
##' @title FindExpectedWeight
##' @param data Sale notes data object, with one line per landing and columns: "Boat"   "S"      "G"      "R"      "Weight" "L"      "Boat2"  "LwiB" (Boat = orig. boat numer, S = season, G = gear, R = region (ECA-region 1:9), Weight = total weight (converted to gram), L = landing, Boat2 = boat number within {S,G,R}, LwiB = landing number within boat
##' @param fit Output (list object) from a fitted ECA model with elements: "age"       "lga"       "hsz"       "nonlin"    "lga.cc"    "nonlin.cc" "wgl"       "wgl.cc"    "split"     "k"
##' @param Par_ECA Sampled parameters,  the fitted ECA model (ceta_boat_matrix, ceta_unit_matrix, ceta_cell_matrix)
##' @param pA Age distibution matrix
##' @return A vector of expected weights (one for each landing, in gram)
##' @author Ingunn Fride Tvete
##' @export

FindExpectedWeight<-function(data,fit,Par_ECA,pA) {
  data<-data[order(data$S,data$G,data$R,data$Boat2,data$LwiB),]#makes sure we have the right order of data
  
  agepar<-ExtractParestECArunAgedist(fit)
  LGApar<-ExtractParestECArunLGA(fit)
  WGLpar<-ExtractParestECArunWGL(fit)
 
  alpha_const<-agepar$alpha_const
  alpha_season<-agepar$alpha_season
  alpha_gear<-agepar$alpha_gear
  tau_region<-agepar$tau_region
  tau_boat<-agepar$tau_boat
  tau_cell<-agepar$tau_cell
  tau_unit<-agepar$tau_unit
  phi_age<-agepar$phi_age
   
  beta_const<-LGApar$beta_const
  beta_season<-LGApar$beta_season
  beta_gear<-LGApar$beta_gear
  beta_1<-LGApar$beta_1
  tau_epsilon_region<-LGApar$tau_epsilon_region
  tau_epsilon_boat<-LGApar$tau_epsilon_boat
  tau_epsilon_cell<-LGApar$tau_epsilon_cell
  tau_epsilon_unit<-LGApar$tau_epsilon_unit
  tau_epsilon_fish<-LGApar$tau_epsilon_fish
  phi_LGA<-LGApar$phi_LGA
  
  epsilon_region<-Par_ECA$epsilon_region
  epsilon_boat<-Par_ECA$epsilon_boat
  epsilon_landing<-Par_ECA$epsilon_unit
  epsilon_cell<-Par_ECA$epsilon_cell
  
  delta_const<-WGLpar$delta_const
  delta_season<-WGLpar$delta_season
  delta_gear<-WGLpar$delta_gear
  delta_1<-WGLpar$delta_1
  tau_nu_region<-WGLpar$tau_nu_region
  tau_nu_boat<-WGLpar$tau_nu_boat
  tau_nu_cell<-WGLpar$tau_nu_cell
  tau_nu_unit<-WGLpar$tau_nu_unit
  tau_nu_fish<-WGLpar$tau_nu_fish
  phi_WGL<-WGLpar$phi_WGL
  
  nu_region<-Par_ECA$nu_region
  nu_boat<-Par_ECA$nu_boat
  nu_landing<-Par_ECA$nu_unit
  nu_cell<-Par_ECA$nu_cell

  SGR<-as.numeric(as.factor(paste(data$S,data$G,data$R,sep="")))
  
  S<-length(unique(data$S))
  G<-length(unique(data$G))
  R<-length(unique(data$R))
  A<-length(alpha_const)
  
  EWa<-matrix(0,nrow=nrow(data),ncol=A)
  for (i in 1:A)
  {
    EWa[,i]<-exp(delta_const + delta_season[data$S] + delta_gear[data$G] + nu_region[data$R] + nu_boat[data$Boat2] + nu_landing[data$LwiB] + nu_cell[SGR] + delta_1*(beta_const+beta_season[data$S] + beta_gear[data$G] + epsilon_region[data$R] + epsilon_boat[data$Boat2] + epsilon_landing[data$LwiB]  + epsilon_cell[SGR] + beta_1*log(i)) -0.5*(1/tau_nu_fish))
  }
  EW<-rowSums(EWa*pA)
  return(EW)
}
