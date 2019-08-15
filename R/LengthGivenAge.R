##' Sample the length of fish for a given age     
##'
##' @title LengthGivenAge
##' @param data Sale notes data object, with one line per landing and columns: "Boat"   "S"      "G"      "R"      "Weight" "L"      "Boat2"  "LwiB" (Boat = orig. boat numer, S = season, G = gear, R = region (ECA-region 1:9), Weight = total weight (converted to gram), L = landing, Boat2 = boat number within {S,G,R}, LwiB = landing number within boat
##' @param beta_const ECA-parameter
##' @param beta_season ECA-parameter
##' @param beta_gear ECA-parameter
##' @param beta_1 ECA-parameter
##' @param epsilon_region ECA-parameter
##' @param epsilon_boat ECA-parameter
##' @param epsilon_cell ECA-parameter
##' @param epsilon_landing ECA-parameter
##' @param tau_epsilon_fish ECA-parameter
##' @return A vector of fish lengths (cm) 
##' @author Ingunn Fride Tvete
##' @export
LengthGivenAge<-function(data,beta_const,beta_season,beta_gear,beta_1,epsilon_region,epsilon_boat,epsilon_cell,epsilon_landing,tau_epsilon_fish) {
  
  SGR<-as.numeric(as.factor(paste(data$S,data$G,data$R,sep="")))#must be same as cell_identity
  beta0<-c(rep(beta_const,length(data$S)))+unlist(beta_season[data$S])+unlist(beta_gear[data$G])+epsilon_region[data$R]+epsilon_boat[data$Boat2]+epsilon_cell[SGR]+epsilon_landing[data$L]
  
  f1<-list()
  for (i in 1:length(data$S))
  {
    epsilon_fish<-rnorm(length(data$age[[i]]),mean=0,sd=sqrt(1/tau_epsilon_fish))
    f1[[i]]<- exp(beta0[i]+beta_1*log(data$age[[i]])+epsilon_fish)
  }
  
  return(f1)
}
