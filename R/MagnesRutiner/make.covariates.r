make.covariates <- function(FullData) {

##################################################################################
### Lager ulike kovariater returnert som en liste
###  
###
### Forklarer her innholdet av listeelementet treatment:
### -treatment er en liste med ett element per merd.
###  Hvert merd-element inneholder
###   - "NR" (not relevant) hvis det ikke er noen behandlinger
###   - en liste med behandlingstyper hvis det er behandlinger.
###     Hvert behandlingstype-element inneholder
###      - "NR" hvis det ikke er noen behandlinger av denne type (for denne merden)
###      - en liste med enkeltbehandlinger av denne type hvis det er minst en slik
###        Hvert enkelbehandlings-element inneholder
###         - en liste med tre stadier, CH, PA og A.
###         Hvert stadie-element inneholder
###          - "NR" hvis behandlingstypen ikke har effekt for dette stadiet
###          - en (tid x alder)-matrise med 0-ere, 1-ere
###            og evt. desimaltall mellom 0 og 1 hvis behandlingstypen har effekt
#################################################################################

  if (is.null(FullData)) {
    return(NULL)
  }

  M<-FullData$divinfo$max.stage.life
  
  treatment<-make.treatment(FullData=FullData)


  
  temp<-FullData$temp
  nam <- names(temp)

  n <- length(temp)
  
  Tmean  <- matrix(NA,ncol=M+1,nrow=n,dimnames=list(names(temp),paste(0:M,sep="")))
  temp.new <- temp
  temp1 <- rep(temp[1],M)
  temp.new <- c(temp1,temp)
  for(j in nam){
    i <- (1:n)[j==nam]+M
    for(a in 0:M){
      Tmean.tmp <- mean(temp.new[(i-a):i])
      Tmean[i-M,a+1]<-Tmean.tmp
    } 
  }
  
###reproduction
  beta1.r <- 0.20
  beta2.r <- 1
  alpha1  <- 1762
  alpha2  <- 4.2
  
  a.vec<-0:M
  z <- matrix(rep(1/(temp+alpha2)^2,M+1),ncol=M+1,dimnames=list(names(temp),paste(0:M,sep="")))
  z0 <- t(matrix(rep(a.vec,dim(z)[1]),ncol=dim(z)[1],dimnames=list(paste(0:M,sep=""),(names(temp)))))
  reprod <- (z0+beta2.r)^beta1.r * 1/(alpha1*z+1)
  
  
  return(list(Tmean=Tmean,treatment=treatment,reprod=reprod))
}
