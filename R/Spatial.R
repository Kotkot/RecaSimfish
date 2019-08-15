##' Spatial pattern (ECA regions 1:9)
##'
##' @title Spatial
##' @return A list (input information for spatial relationship)
##' @author Ingunn Fride Tvete
##' @export

Spatial<-function(){
######################################
#Spatial pattern:               
#region, no neighbors (neighbors)
#1,3 (2,3,4)
#2,3 (1,3,7)
#3,7 (1,2,4,5,7,8,9)
#4,3 (1,3,5)
#5,4 (3,4,6,8)
#6,2 (5,8)
#7,2 (2,3)
#8,4 (3,5,6,9)
#9,2 (3,8)
########################################

c<-matrix(c(
  0,1,1,1,0,0,0,0,0,
  1,0,1,0,0,0,1,0,0,
  1,1,0,1,1,0,1,1,1,
  1,0,1,0,1,0,0,0,0,
  0,0,1,1,0,1,0,1,0,
  0,0,0,0,1,0,0,1,0,
  0,1,1,0,0,0,0,0,0,
  0,0,1,0,1,1,0,0,1,
  0,0,1,0,0,0,0,1,0
),nrow=9)
n<-rowSums(c)
c<-c/n
cmat<-c
  n_neighbours<-n
  
return(list(c,n_neighbours))
}
