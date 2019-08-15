##' Sample the weight of fish for a givenlength 
##'
##' @title WeightGivenLength
##' @param data Sale notes data object, with one line per landing and columns: "Boat"   "S"      "G"      "R"      "Weight" "L"      "Boat2"  "LwiB" (Boat = orig. boat numer, S = season, G 0 gear, R = region (ECA-region 1:9), Weight = total weight (converted to gram), L = landing, Boat2 = boat number within {S,G,R}, LwiB = landing number within boat
##' @param length_g_age  Length of each fish (given the age)
##' @param delta_const ECA parameter
##' @param delta_season ECA parameter
##' @param delta_gear ECA parameter
##' @param delta_1 ECA parameter
##' @param nu_region ECA parameter
##' @param nu_boat ECA parameter
##' @param nu_cell ECA parameter
##' @param nu_landing ECA parameter
##' @param tau_nu_fish ECA parameter
##' @return  A vector of fish weights (in gram) 
##' @author Ingunn Fride Tvete
##' @export

WeightGivenLength<-function(data,length_g_age,delta_const,delta_season,delta_gear,delta_1,nu_region,nu_boat,nu_cell,nu_landing,tau_nu_fish){
  SGR<-as.numeric(as.factor(paste(data$S,data$G,data$R,sep="")))#must be same as cell_identity
  delta0<-c(rep(delta_const,length(data$S)))+unlist(delta_season[data$S])+unlist(delta_gear[data$G])+nu_region[data$R]+nu_boat[data$Boat2]+nu_cell[SGR]+nu_landing[data$L]
  f1<-list()
  for (i in 1:length(data$S))
  {
    nu_fish<-rnorm(length(data$age[[i]]),mean=0,sd=sqrt(1/tau_nu_fish))
    f1[[i]]<-exp(delta0[i]+delta_1*log(length_g_age[[i]]) + nu_fish)
  }
  return(f1)
}
