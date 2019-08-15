##' Simulating fishery  
##'
##' @title SimulateFishery
##' @param salenotesdata Sale notes data object, with one line per landing and columns: "Boat"   "S"      "G"      "R"      "Weight" "L"      "Boat2"  "LwiB" (Boat = orig. boat numer, S = season, G = gear, R = region (ECA-region 1:9), Weight = total weight (converted to gram), L = landing, Boat2 = boat number within {S,G,R}, LwiB = landing number within boat
##' @param A The Number of age groups
##' @param ages The Ages (range, e.g. 1:19)
##' @param fit Output (list object) from a fitted ECA model with elements: "age"  "lga"       "hsz"       "nonlin"    "lga.cc"    "nonlin.cc" "wgl"       "wgl.cc"    "split"     "k"
##' @return A list (fishery data object, with elements "Boat"  "S"   "G" "R"  "Weight" "L"  "Boat2" "LwiB"  "ExpectedWeight" "Nofish" "ages"  "LGA"   "WGL"), LGA = length given age and WGL = weight given length
##' @author Ingunn Fride Tvete
##' @export
SimulateFishery<-function(salenotesdata,A,ages,fit) {
  library(MASS)
  
  data<-salenotesdata
  data<-data[order(data$S,data$G,data$R,data$Boat2,data$LwiB),]
  
  #Spatial pattern:function Spatial()
  spat<-Spatial()
  c<-spat[[1]]
  cmat<-c 
  n_neighbours<-spat[[2]]
  
  Par_ECA<-SampleParFromFittedDistECA(data,fit,n_neighbours,cmat)
  pA<-FindpAmatrix(data,fit,Par_ECA)
  
  EW<-FindExpectedWeight(data,fit,Par_ECA,pA)
  data<-data.frame(data,EW,ceiling(data$Weight/EW))
  names(data)[ncol(data)-1]<-"ExpectedWeight"
  
  names(data)[ncol(data)]<-"Nofish"
  
  LGApar<-ExtractParestECArunLGA(fit)
  WGLpar<-ExtractParestECArunWGL(fit)
  
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
   
  #for each year
  ceta_region_matrix<-Par_ECA$ceta_region_matrix
  ceta_boat_matrix<-Par_ECA$ceta_boat_matrix
  ceta_unit_matrix<-Par_ECA$ceta_unit_matrix
  ceta_cell_matrix<-Par_ECA$ceta_cell_matrix
  
  boat_identity<-Par_ECA$boat_identity#to check boat number (not used)
  unit_identity<-Par_ECA$unit_identity#to check landing number 
  cell_identity<-Par_ECA$cell_identity#to check cell {S,G,R} number 
  
  Fishery<-data
  pp<-unlist(pA)
  pp<-matrix(pp,ncol=A)
  
  #######
  #ages
  #######
  f1<-list()
  for (i in 1:nrow(pA))
  {
    tmp<-pp[i,]
    p2<-matrix(rep(tmp,Fishery$Nofish[i]),ncol=length(ages),byrow=T)
    #p2<-matrix(unlist(p2),ncol=A)
    cump<-p2%*%upper.tri(diag(ncol(p2)),diag=TRUE)/rowSums(p2)
    x<-runif(nrow(p2))
    j<-rowSums(x > cump) + 1L
    f1[[i]]<- ages[j]
  }
  
  data<-as.list(Fishery)
  length(data)
  names(data)
  data$ages<-f1
  
  ##################
  #LGA
  #################
  beta_const<-LGApar$beta_const
  beta_season<-LGApar$beta_season
  beta_gear<-LGApar$beta_gear
  beta_1<-LGApar$beta_1
  epsilon_region<-Par_ECA$epsilon_region
  epsilon_boat<-Par_ECA$epsilon_boat
  epsilon_landing<-Par_ECA$epsilon_unit
  
  epsilon_cell<-Par_ECA$epsilon_cell
  tau_epsilon_fish<-LGApar$tau_epsilon_fish
  
  LGA<-LengthGivenAge(data,beta_const,beta_season,beta_gear,beta_1,epsilon_region,epsilon_boat,epsilon_cell,epsilon_landing,tau_epsilon_fish)
  data$LGA<-LGA    
  
  ###############
  #WGL
  ##############
  delta_const<-WGLpar$delta_const
  delta_season<-WGLpar$delta_season
  delta_gear<-WGLpar$delta_gear
  delta_1<-WGLpar$delta_1
  
  nu_region<-Par_ECA$nu_region
  nu_boat<-Par_ECA$nu_boat
  nu_landing<-Par_ECA$nu_unit
  
  nu_cell<-Par_ECA$nu_cell
  tau_nu_fish<-WGLpar$tau_nu_fish
  
  WGL<-WeightGivenLength(data,LGA,delta_const,delta_season,delta_gear,delta_1,nu_region,nu_boat,nu_cell,nu_landing,tau_nu_fish)
  data$WGL<-WGL
  
  return(data)
  
}



