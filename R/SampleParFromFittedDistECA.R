##' Sample random parameters from fitted ECA-model 
##'
##' @title SampleParFromFittedDistECA
##' @param data Sale notes data object, with one line per landing and columns: "Boat"   "S"      "G"      "R"      "Weight" "L"      "Boat2"  "LwiB" (Boat = orig. boat numer, S = season, G = gear, R = region (ECA-region 1:9), Weight = total weight (converted to gram), L = landing, Boat2 = boat number within {S,G,R}, LwiB = landing number within boat
##' @param fit  Output (list object) from a fitted ECA model with elements: "age"       "lga"       "hsz"       "nonlin"    "lga.cc"    "nonlin.cc" "wgl"       "wgl.cc"    "split"     "k"
##' @param n_neighbours Number of neighbours
##' @param cmat Spatial matrix
##' @return A list of sampled parameters (form the fitted ECA model)
##' @author Ingunn Fride Tvete
##' @export

SampleParFromFittedDistECA<-function(data,fit,n_neighbours,cmat){
  data<-data[order(data$S,data$G,data$R,data$Boat2,data$LwiB),]
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
  
  
  #age:
  A<-length(alpha_const)
  R<-length(unique(data$R))
  B<-length(unique(data$Boat2))
  
  #for each age: one region-vector
  ceta_region_matrix<-matrix(0,ncol=R,nrow=A)
  for (a in 1:A)
  {
    M<-diag((tau_region*(phi_age*n_neighbours+1-phi_age))^(-1))
    CovMat<-(solve(diag(1,nrow(M))-cmat*phi_age))%*%M
    mu_region<-c(rep(0,nrow(M)))
    tmp<-mvrnorm(1,mu_region,CovMat)
    tmp<-tmp-mean(tmp)#Make sure sum to zero!!! one for each region
    ceta_region_matrix[a,]<-tmp
  }
  
  ceta_boat_matrix<-matrix(0,ncol=B,nrow=A)
  for (a in 1:A)
  {
    ceta_boat_matrix[a,]<-rnorm(length(unique(data$Boat2)),mean=0,sd=sqrt(1/tau_boat))#one for each boat
  }
  boat_identity<-unique(data$Boat2)
  
  ceta_unit_matrix<-matrix(0,ncol=nrow(data),nrow=A)#landings
  for (A in 1:A)
  {
    ceta_unit_matrix[a,]<-rnorm(nrow(data),mean=0,sd=sqrt(1/tau_unit))#each row in data is a unique landing
  }
  unit_identity<-data$L
  
  SGR<-as.numeric(as.factor(paste(data$S,data$G,data$R,sep="")))
  ceta_cell_matrix<-matrix(0,ncol=length(unique(SGR)),nrow=A)
  for (a in 1:A)
  {
    ceta_cell_matrix[a,]<-rnorm(length(unique(SGR)),mean=0,sd=sqrt(1/tau_cell))#each S,G,R-comination in data 
  }
  cell_identity<-unique(SGR)
  
  
  #LGA:
  M<-diag((tau_nu_region*(phi_LGA*n_neighbours+1-phi_WGL))^(-1))
  CovMat<-(solve(diag(1,nrow(M))-cmat*phi_WGL))%*%M
  mu_region<-c(rep(0,nrow(M)))
  nu_region<-mvrnorm(1,mu_region,CovMat)
  nu_region<-nu_region-mean(nu_region)#Make sure sum to zero!!! one for each region
  
  Boat<-as.numeric(as.factor(data$Boat2))
  nu_boat<-rnorm(length(unique(data$Boat2)),mean=0,sd=sqrt(1/tau_nu_boat))#one for each boat
  
  nu_unit<-rnorm(length(data$L),mean=0,sd=sqrt(1/tau_nu_unit))# each row in data is a unique landing
  
  SGR<-as.numeric(as.factor(paste(data$S,data$G,data$R,sep="")))
  nu_cell<-rnorm(length(unique(SGR)),mean=0,sd=sqrt(1/tau_nu_cell))#each S,G,R-comination in data 
  
  #WGL  
  M<-diag((tau_epsilon_region*(phi_LGA*n_neighbours+1-phi_LGA))^(-1))
  CovMat<-(solve(diag(1,nrow(M))-cmat*phi_LGA))%*%M
  mu_region<-c(rep(0,nrow(M)))
  epsilon_region<-mvrnorm(1,mu_region,CovMat)
  epsilon_region<-epsilon_region-mean(epsilon_region)#Make sure sum to zero!!! one for each region
  
  Boat<-as.numeric(as.factor(data$Boat))
  epsilon_boat<-rnorm(length(unique(Boat)),mean=0,sd=sqrt(1/tau_epsilon_boat))#one for each boat
  
  epsilon_unit<-rnorm(length(data$L),mean=0,sd=sqrt(1/tau_epsilon_unit))# each row in data is a unique landing
  
  SGR<-as.numeric(as.factor(paste(data$S,data$G,data$R,sep="")))
  epsilon_cell<-rnorm(length(unique(SGR)),mean=0,sd=sqrt(1/tau_epsilon_cell))#each S,G,R-comination in data 
  
  dataout<-list()
  dataout$ceta_region_matrix<-ceta_region_matrix
  dataout$ceta_boat_matrix<-ceta_boat_matrix
  dataout$boat_identity<-boat_identity
  dataout$ceta_unit_matrix<-ceta_unit_matrix
  dataout$unit_identity<-unit_identity
  dataout$ceta_cell_matrix<-ceta_cell_matrix
  dataout$cell_identity<-cell_identity
  dataout$nu_region<-nu_region
  dataout$nu_boat<-nu_boat
  dataout$nu_unit<-nu_unit
  dataout$nu_cell<-nu_cell
  dataout$epsilon_region<-epsilon_region
  dataout$epsilon_boat<-epsilon_boat
  dataout$epsilon_unit<-epsilon_unit
  dataout$epsilon_cell<-epsilon_cell
  
  return(dataout)
  
}
