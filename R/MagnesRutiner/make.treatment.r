make.treatment <- function(FullData,time.names.future=NULL) {

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

  n.stage.treat<-3
  stage.treat.list<-c("CH","PA","A")
  max.gap.within.treatment<-3
  
  MedicineInfo<-FullData$divinfo$MedicineInfo
  temp<-FullData$temp
  behand<-FullData$behand
  
  time.names<-FullData$divinfo$time.names
  n.times<-FullData$divinfo$n.times
  merd.names<-FullData$divinfo$merd.names
  n.merds<-FullData$divinfo$n.merds
  med.names<-FullData$divinfo$med.names
  n.med<-FullData$divinfo$n.med

  if (!is.null(time.names.future)) {
    last.date.in.historical<-tail(time.names,1)
    first.date.in.future<-time.names.future[1]
    if (last.date.in.historical!=first.date.in.future) {
      print("Incorrect time match - last.date.in.historical!=first.date.in.future, returns NULL!")
      return(NULL)
    }
    time.names.tot<-c(time.names,time.names.future[-1])
    start.time<-n.times
    time.names.actual<-time.names.future
  } else {
    time.names.tot<-time.names
    start.time<-1
    time.names.actual<-time.names
  }
  n.times.tot<-length(time.names.tot)
  n.times.actual<-length(time.names.actual)
  
#####################################################################
### Treatment
#####################################################################
    
  if (n.med==0) {
    treatment<-NULL
    ##treatment<-vector("list",n.merds)
    ##names(treatment)<-merd.names
    ##for(merd in merd.names) {
    ##  treatment[[merd]]<-"NR"
    ##}
  } else {
    mat0<-matrix(0,nrow=n.times.actual,ncol=M+1,dimnames=list(time.names.actual,as.character(0:M)))
##    mat0<-matrix(0,nrow=n.times,ncol=M+1,dimnames=list(time.names,as.character(1:(M+1))))
    treatment<-vector("list",n.merds)
    names(treatment)<-merd.names
    ##treatment <- array(0,dim=c(n.times,n.merds,n.med,n.stage.treat,M+1),
    ##dimnames=list(time.names,merd.names,med.names,stage.treat.list,as.character(1:(M+1))))
    
    
    
    ## Azametifos brukt sammen med Deltametrin eller Cypermetrin settes lik
    ## Deltametrin eller Cypermetrin brukt aleine, dvs. Azametifos ignoreres,
    ## så lenge Deltametrin/Cypermetrin
    ## er brukt innen max.gap.within.treatment dager før eller etter
    ## at Azametifos er brukt.
    Aname<-"Azametifos"
    if (Aname %in% med.names) {
      for(j in 1:n.merds) {
        A<-behand[,j,Aname]
        for (Bname in c("Deltametrin","Cypermetrin")) {
          if (Bname %in% med.names) {
            B<-behand[,j,Bname]
            for (tt in 1:n.times) {
              if (A[tt]==1) {
                for (t2 in max(tt-max.gap.within.treatment,1):
                     min(tt+max.gap.within.treatment,n.times)) {
                  if (B[t2]==1) {
                    A[tt]<-0
                  }
                }
              }
            }
          }
        }
        behand[,j,Aname]<-A
      }
    }

    for(j in 1:n.merds) {
      treatment[[j]]<-vector("list",n.med)
      names(treatment[[j]])<-med.names
      for(k in 1:n.med) {
        id.full <- (1:n.times)[behand[,j,k]==1]
        if (length(id.full)==0) {
          treatment[[j]][[k]]<-"NR"
        } else {
          if (substr(med.names[k],1,5)=="Ideal") {
            medicine.info<-MedicineInfo[MedicineInfo$medicine=="Ideal",]
          } else {
            medicine.info<-MedicineInfo[MedicineInfo$medicine==med.names[k],]
          }
          Deltadel<-medicine.info$delay
          feed<-medicine.info$feed
          if (medicine.info$temp.dependent) {
            Deltadur<-medicine.info$duration.const/temp
          } else {
            Deltadur<-rep(medicine.info$duration.const,n.times)
          }


          CH.effect<-medicine.info$CH.effect
          PA.effect<-medicine.info$PA.effect
          A.effect<-medicine.info$A.effect
          
          
          if (length(id.full)==1) {
            n.treat<-1
            id.list<-list(id1=id.full)
          } else {
            gap<-diff(id.full)
            treat.start<-c(1,which(gap>max.gap.within.treatment)+1)
            treat.end<-c(which(gap>max.gap.within.treatment),length(id.full))
            n.treat<-length(treat.start)
            id.list<-vector("list",n.treat)
            names(id.list)<-paste(rep("id",n.treat),1:n.treat,sep="")
            for (l in 1:n.treat) {
              id.list[[l]]<-id.full[treat.start[l]:treat.end[l]]
            }
          }
          
          ##    n.treat<-1
          treatment[[j]][[k]]<-vector("list",n.treat)
          names(treatment[[j]][[k]])<-paste(rep("Treatno",n.treat),1:n.treat,sep="")
          
          for (l in 1:n.treat) {
            id<-id.list[[l]]
            treatment[[j]][[k]][[l]]<-vector("list",n.stage.treat)
            names(treatment[[j]][[k]][[l]])<-stage.treat.list
            
            if (!CH.effect) {
              treatment[[j]][[k]][[l]]$CH<-"NR"
            } else {
              treatment[[j]][[k]][[l]]$CH<-mat0
              if (feed=="Y") {
                for(i in id){
                  ##                  if ((i+Deltadel)<=n.times.tot & (i+Deltadel+round(Deltadur[i]))>=start.time) {
                  ##                    for (tt in (i+Deltadel):min(n.times.tot,(i+Deltadel+round(Deltadur[i])))) {
                  end.time.effect<-Deltadel+max(0,round(Deltadur[i])-1)
                  ##print("here")
                  ##browser()
                  if ((i+Deltadel)<=n.times.tot & (i+end.time.effect)>=start.time) {
                    for (tt in (i+Deltadel):min(n.times.tot,(i+end.time.effect))) {
                      if (tt>=start.time) {
                        treatment[[j]][[k]][[l]]$CH[tt-start.time+1,(1:(M+1))]<-1
                      }
                    }
                  }
                }
              } else {
                for(i in id){
                  aa<-Deltadel
                  ##                  if ((i+Deltadel)<=n.times.tot & (i+Deltadel+round(Deltadur[i]))>=start.time) {
                  ##                    for (tt in (i+Deltadel):min(n.times.tot,(i+Deltadel+round(Deltadur[i])))) {
                  end.time.effect<-Deltadel+max(0,round(Deltadur[i])-1)
                  if ((i+Deltadel)<=n.times.tot & (i+end.time.effect)>=start.time) {
                    for (tt in (i+Deltadel):min(n.times.tot,(i+end.time.effect))) {
                      aa<-aa+1
                      if (tt>=start.time) {
                        treatment[[j]][[k]][[l]]$CH[tt-start.time+1,(aa:(M+1))]<-1
                      }
                    }
                  }
                }
              }
            }
            
            if (!PA.effect) {
              treatment[[j]][[k]][[l]]$PA<-"NR"
            } else {
              treatment[[j]][[k]][[l]]$PA<-mat0
              if (feed=="Y") {
                for(i in id){
                  ##                  if ((i+Deltadel)<=n.times.tot & (i+Deltadel+round(Deltadur[i]))>=start.time) {
                  ##                    for (tt in (i+Deltadel):min(n.times.tot,(i+Deltadel+round(Deltadur[i])))) {
                  end.time.effect<-Deltadel+max(0,round(Deltadur[i])-1)
                  if ((i+Deltadel)<=n.times.tot & (i+end.time.effect)>=start.time) {
                    for (tt in (i+Deltadel):min(n.times.tot,(i+end.time.effect))) {
                      if (tt>=start.time) {
                        treatment[[j]][[k]][[l]]$PA[tt-start.time+1,(1:(M+1))]<-1
                      }
                    }
                  }
                }
              } else {
                for(i in id){
                  aa<-Deltadel
                  ##                  if ((i+Deltadel)<=n.times.tot & (i+Deltadel+round(Deltadur[i]))>=start.time) {
                  ##                    for (tt in (i+Deltadel):min(n.times.tot,(i+Deltadel+round(Deltadur[i])))) {
                  end.time.effect<-Deltadel+max(0,round(Deltadur[i])-1)
                  if ((i+Deltadel)<=n.times.tot & (i+end.time.effect)>=start.time) {
                    for (tt in (i+Deltadel):min(n.times.tot,(i+end.time.effect))) {
                      aa<-aa+1
                      if (tt>=start.time) {
                        treatment[[j]][[k]][[l]]$PA[tt-start.time+1,(aa:(M+1))]<-1
                      }
                    }
                  }
                }
              }
            }
            
            if (!A.effect) {
              treatment[[j]][[k]][[l]]$A<-"NR"
            } else {
              treatment[[j]][[k]][[l]]$A<-mat0
              if (feed=="Y") {
                for(i in id){
                  ##                  if ((i+Deltadel)<=n.times.tot & (i+Deltadel+round(Deltadur[i]))>=start.time) {
                  ##                    for (tt in (i+Deltadel):min(n.times.tot,(i+Deltadel+round(Deltadur[i])))) {
                  end.time.effect<-Deltadel+max(0,round(Deltadur[i])-1)
                  if ((i+Deltadel)<=n.times.tot & (i+end.time.effect)>=start.time) {
                    for (tt in (i+Deltadel):min(n.times.tot,(i+end.time.effect))) {
                      if (tt>=start.time) {
                        treatment[[j]][[k]][[l]]$A[tt-start.time+1,(1:(M+1))]<-1
                      }
                    }
                  }
                }
              } else {
                for(i in id){
                  aa<-Deltadel
                  ##                  if ((i+Deltadel)<=n.times.tot & (i+Deltadel+round(Deltadur[i]))>=start.time) {
                  ##                    for (tt in (i+Deltadel):min(n.times.tot,(i+Deltadel+round(Deltadur[i])))) {
                  end.time.effect<-Deltadel+max(0,round(Deltadur[i])-1)
                  if ((i+Deltadel)<=n.times.tot & (i+end.time.effect)>=start.time) {
                    for (tt in (i+Deltadel):min(n.times.tot,(i+end.time.effect))) {
                      aa<-aa+1
                      if (tt>=start.time) {
                        treatment[[j]][[k]][[l]]$A[tt-start.time+1,(aa:(M+1))]<-1
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  ## Hva med flytting?
  ## Skal treatment modifiseres med w?
  
  treatment
}
