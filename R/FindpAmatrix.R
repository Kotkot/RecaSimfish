##' FindpAmatrix: find age probability distribution matrix
##'
##' @title FindpAmatrix
##' @param dataSale notes data object, with one line per landing and columns: "Boat"   "S"      "G"      "R"      "Weight" "L"      "Boat2"  "LwiB" (Boat = orig. boat numer, S = season, G = gear, R = region (ECA-region 1:9), Weight = total weight (converted to gram), L = landing, Boat2 = boat number within {S,G,R}, LwiB = landing number within boat
##' @param fit Output (list object) from a fitted ECA model with elements: "age"       "lga"       "hsz"       "nonlin"    "lga.cc"    "nonlin.cc" "wgl"       "wgl.cc"    "split"     "k"
##' @param Par_ECA Sampled parameters,  the fitted ECA model (ceta_boat_matrix, ceta_unit_matrix, ceta_cell_matrix)
##' @return A matrix (number of rows are equal to the number of landings in the sale notes data), each row is the age distribution for the given landing
##' @author Ingunn Fride Tvete
FindpAmatrix<-function(data,fit,Par_ECA){
  data<-data[order(data$S,data$G,data$R,data$Boat2,data$ LwiB),]#makes sure we have the right order of data
  
  agepar<-ExtractParestECArunAgedist(fit)
  LGApar<-ExtractParestECArunLGA(fit)
  WGLpar<-ExtractParestECArunWGL(fit)
  
  #sample age (alpha_const,alpha_season,alpha_gear,tau_region,tau_boat,tau_cell,tau_unit,phi_age)
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
  
  epsilon_region<-rowMeans(fit$lga$Int$area[1,,])#not used
  
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
  
  nu_region<-rowMeans(fit$wgl$Int$area[1,,])#not used
  
  #these are the parameters that we have sampled from their ECA-fitted distributions
  ceta_region_matrix<-Par_ECA$ceta_region_matrix
  ceta_boat_matrix<-Par_ECA$ceta_boat_matrix
  ceta_unit_matrix<-Par_ECA$ceta_unit_matrix
  ceta_cell_matrix<-Par_ECA$ceta_cell_matrix
  
  boat_identity<-Par_ECA$boat_identity#to check boat number 
  unit_identity<-Par_ECA$unit_identity#to check landing number 
  cell_identity<-Par_ECA$cell_identity#to check cell {S,G,R} number 
  
  Boat<-as.numeric(as.factor(data$Boat2))#must be same as as.numeric(as.factor(boat_identity))
  SGR<-as.numeric(as.factor(paste(data$S,data$G,data$R,sep="")))#must be same as cell_identity
  
  A<-length(alpha_const)
  alpha<-matrix(0,ncol=A,nrow=nrow(data))
  for (i in 1:A)
  {   
    alpha[,i]<-c(rep(alpha_const[i],nrow(data)))+unlist(alpha_season[i,data$S])+unlist(alpha_gear[i,data$G])+ceta_region_matrix[i,data$R]+ceta_boat_matrix[i,Boat]+ceta_unit_matrix[i,]+ceta_cell_matrix[i,SGR]
  }
  p<-exp(alpha)/rowSums(exp(alpha))
  
  return(p)
}
