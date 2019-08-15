##' Plot lice data and predictions.
##' 
##' @title Plot lice data and predictions.
##' @inheritParams dummy.for.documentation
##' @param merdnumb 
##' @param AllCages 
##' @param onlydata 
##' @param plot.treat.in.all.panels 
##' @param includestage 
##' @param yheight 
##' @param to.file 
##' @param file 
##' @param mainTxt1 
##' @param forecast 
##' @return NULL
##' @author Magne Aldrin
##' @export plot.4panels.treat.obs.expected
plot.4panels.treat.obs.expected <- function(AllData,merdnumb,AllCages=FALSE,
                                            onlydata=TRUE,
                                            plot.treat.in.all.panels=FALSE,
                                            includestage=c("CH","OM","Af"),
                                            yheight=3,
                                            to.file=TRUE,file=NULL,
                                            mainTxt1="",
                                            forecast=NULL) {

  FullData<-AllData$FullData
  pred.summary<-AllData$pred.summary
  count.summary<-AllData$count.summary
  cleaner.fish.summary<-AllData$cleaner.fish.summary
  CleanerFishMean<-AllData$CleanerFishMean
  
  first.day.col<-6
  movement.col<-8
  treat.col<-4
  
  hor.lwd<-2
  plt.lwd<-2.5
  
  forecast.col<-1
  forecast.lty<-3
  
  forecast.lwd<-3
  clf.sto.col<-1
  clf.sto.lty<-1
  clf.sto.lwd<-2
  clf.ratio.col<-1
  clf.ratio.lty<-9
  clf.ratio.lwd<-0.8
  smp.col<-2
  smp.lty<-9
  smp.lwd<-4
  w.col<-5
  w.lty<-1
  w.lwd<-2
  
  fit.col<-2
  stage.col<-3
  
####################################################
### Ordne kalender
####################################################
  normal.year.days.in.month<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  leap.year.days.in.month<-c(31,29,31,30,31,30,31,31,30,31,30,31)
  four.years.days.in.month<-c(rep(normal.year.days.in.month,3),leap.year.days.in.month)
  no.4years.cycles<-20
  days.in.month<-rep(four.years.days.in.month,no.4years.cycles,)
  
  no.days<-sum(days.in.month)
  start.month<-rep(NA,length(days.in.month))
  start.month[1]<-1
  for (i in 2:length(days.in.month)) {
    start.month[i]<-start.month[i-1]+days.in.month[i-1]
  }
  mid.month<-start.month+15
  
  month.names<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  month.names<-c(month.names,month.names,month.names,month.names,month.names,month.names,month.names,month.names)
  year<-rep(NA,length(days.in.month))
  i<-0
  for (yy in 2011:(2011+4*no.4years.cycles-1)) {
    for (j in 1:12) {
      year
      i<-i+1
      year[i]<-yy
    }
  }

  first.year<-as.numeric(substr(FullData$divinfo$first.date.with.fish,1,4))
  first.month<-as.numeric(substr(FullData$divinfo$first.date.with.fish,5,6))
  
  if (first.month==1) {
    first.month<-12
    first.year<-first.year-1
  } else {
    first.month<-first.month-1
  }
  tmp<-(first.year-2011)*12+first.month-1
  
  if (tmp>0) {
    start.time<-sum(days.in.month[1:tmp])+1
  } else {
    start.time<-1 
  }
  
  last.year<-as.numeric(substr(FullData$divinfo$last.date.with.fish,1,4))
  last.month<-as.numeric(substr(FullData$divinfo$last.date.with.fish,5,6))
  tmp<-(last.year-2011)*12+last.month
  if (tmp>0) {
    end.time<-sum(days.in.month[1:tmp])+1
  } else {
    end.time<-1
  }

  ind<-1:length(start.month)
  ind<-ind[(start.month>=start.time) & (start.month<=end.time)]
  start.month<-start.month[ind]

  ind<-1:length(mid.month)
  ind<-ind[(mid.month>=start.time) & (mid.month<=end.time)]
  mid.month<-mid.month[ind]
  month.names<-month.names[ind]
  year<-year[ind]
  
  tmp<-start.month[1]
  start.month<-start.month-tmp+1
  mid.month<-mid.month-tmp+1
  
  
  
  if (last.month==12) {
    ly<-last.year+1
    lm<-1
  } else {
    ly<-last.year
    lm<-last.month+1
  }
  calendar<-GenKalender(first.year,first.month,1,ly,lm,1)
  calendar.ymd<-ymd.num2char(calendar[,"year"],calendar[,"month"],calendar[,"day"])
  n.days<-nrow(calendar)
  time.axis<-1:n.days
  names(time.axis)<-calendar.ymd
#############################################################



  if(to.file){
    cat("filename=",file,"\n")
    pdf(file=file,width=14,height=14) 
###  pdf(file=file,width=14,height=9) 
  }
  
###par(mfrow=c(3,1),mar=c(5, 6, 2.5, 6)-1.0)
  par(mfrow=c(4,1),mar=c(5, 6, 2.5, 6)-1.0)





  
  loc.name<-FullData$divinfo$loc.name
  
  merd.name<-FullData$divinfo$merd.names[merdnumb]

  cat(paste("In Merd",merdnumb,"\n"))
  date1 <- row.names(FullData$antall)[FullData$antall[,merdnumb]>0][1]
  if (is.na(date1)) {
    print("No fish in this cage")
    return()
  }
  data.lus <- FullData$lice[[merdnumb]][,c("CH","OM","Af"),drop=F]/FullData$lice[[merdnumb]][,"n.fish"]
 
  
  if (!is.null(cleaner.fish.summary[[merdnumb]])) {
    E.clf <- 100*cleaner.fish.summary[[merdnumb]][["RensefiskTot"]][,"mean"]
    if (!is.null(pred.summary[[merdnumb]][["OM"]])) {
      E.clf[is.na(pred.summary[[merdnumb]][["OM"]][,"mean"])]<-NA
    }
  } else if (!is.null(CleanerFishMean[[merdnumb]])) {
    E.clf <- 100*CleanerFishMean[[merdnumb]]
    if (!is.null(pred.summary[[merdnumb]][["OM"]])) {
      E.clf[is.na(pred.summary[[merdnumb]][["OM"]][,"mean"])]<-NA
    }
  }
  else {
    E.clf<-NULL
  }




#########################
### plot temperature, cleaner fish, treatments, infection pressure
#########################

  temp<-FullData$temp[names(time.axis)]
  
  if (!AllCages) {
    txt<-" - Temperature, infection pressure index, cleaner fish ratio"
    if (mainTxt1 !="") {
      maintxt<-paste(mainTxt1,"  Cage no. ",merdnumb,txt,sep="")
    } else {
      maintxt<-paste(mainTxt1,loc.name," ",merd.name,txt,sep="")
    }
  } else {
    ##this is mean of all cages
    txt<-" - Temperature, infection pressure index, cleaner fish ratio"
    if (mainTxt1 !="") {
      maintxt<-paste(mainTxt1,"  Average over all cages ",txt,sep="")
    } else {
      maintxt<-paste(mainTxt1,loc.name,txt,sep="")
    }
  }
  
####ylim<-c(0,yheight)
  temp.max<-20
  ylim<-c(0,temp.max)
  xlim<-c(1,n.days)
  plot(time.axis,temp,type="n",
       lty=1,col=stage.col,lwd=plt.lwd,
       xlim=xlim,ylim=ylim,axes=FALSE,
       xlab="",ylab="",
       main=maintxt)
  
  box()
  axis(side=1,line=0,at=start.month,label=FALSE)
  mtext(text=month.names,side=1,line=0.7,at=mid.month,adj=0.5)
  mtext(text=year,side=1,line=2,at=mid.month,adj=0.5)
  mtext(text="Temperature (degree C)",side=2,line=2,at=sum(ylim)/2,adj=0.5)
  axis(side=2,line=0)
  
  lines(time.axis,temp,type="l",
        lty=1,col=stage.col,lwd=plt.lwd)
  
####################################
### Første dag med fisk
####################################
  xcoordinat <- time.axis[names(time.axis)==date1]
  lines(x=c(xcoordinat,xcoordinat),y=c(0,0.5*ylim[2]),col=first.day.col,lty=1,lwd=hor.lwd)
  
  pl.treatments(FullData,merdnumb,time.axis,temp.max,treat.col,hor.lwd)
  pl.movement(FullData,merdnumb,time.axis,temp.max,movement.col,hor.lwd)
  pl.inf.pressure(FullData,time.axis,xlim,ylim,smp.col,smp.lty,smp.lwd)
  pl.weight(FullData,merdnumb,time.axis,xlim,ylim,w.col,w.lty,w.lwd)
  pl.cleaner.fish(FullData,merdnumb,E.clf,time.axis,xlim,ylim,clf.sto.col,clf.sto.lty,clf.sto.lwd,
                  clf.ratio.col,clf.ratio.lty,clf.ratio.lwd)
  if (!is.null(forecast)) {
    x<-time.axis[names(time.axis)==forecast]
    abline(v=x,lwd=forecast.lwd,lty=forecast.lty)
  }
  
  

  
##########################################
### Legend
##########################################  
  if (is.null(CleanerFishMean[[merdnumb]])) {
    ltxt<-c("sea temperature",
            "weight",
            "infection pressure index",
            "cleaner fish stocked",
            "treatment",
            "first day with fish in this cage")
    lcol<-c(stage.col,w.col,smp.col,clf.sto.col,treat.col,first.day.col)
    llty<-c(1,w.lty,smp.lty,clf.sto.lty,1,1)
    llwd<-c(plt.lwd,w.lwd,smp.lwd,clf.sto.lwd,hor.lwd,hor.lwd)
  } else {
    ltxt<-c("sea temperature",
            "weight",
            "infection pressure index",
            "cleaner fish stocked",
            "estimated cleaner fish ratio",
            "treatment",
            "first day with fish in this cage")
    lcol<-c(stage.col,w.col,smp.col,clf.sto.col,clf.ratio.col,treat.col,first.day.col)
    llty<-c(1,w.lty,smp.lty,clf.sto.lty,clf.ratio.lty,1,1)
    llwd<-c(plt.lwd,w.lwd,smp.lwd,clf.sto.lwd,clf.ratio.lwd,hor.lwd,hor.lwd)
}

  if (!is.null(FullData$divinfo$movement.to[[merdnumb]])) {
    ltxt<-c(ltxt,"fish moved from another cage")
    lcol<-c(lcol,movement.col)
    llty<-c(llty,1)
    llwd<-c(llwd,hor.lwd)
  }

  legend(x="topleft", ltxt,col=lcol,lty=llty,lwd=llwd,seg.len=3,cex=0.85,bg="white")



#########################
### plot lice counts etc.
#########################
  for(include in c("CH","OM","Af")){
    
    if (include=="CH") {
      stage.txt<-" - Chalimus"
      stage.name<-"Chalimus"
    }
    if (include=="OM") {
      stage.txt<-" - Other mobiles"
      stage.name<-"Other mobiles"
    }
    if (include=="Af") {
      stage.txt<-" - Adult females"
      stage.name<-"Adult females"
      yheight<-yheight/2
    }


    if (!AllCages) {
      if (mainTxt1 !="") {
        maintxt<-paste(mainTxt1,"  Cage no. ",merdnumb,stage.txt,sep="")
      } else {
        maintxt<-paste(mainTxt1,loc.name,"  ",merd.name,stage.txt,sep="")
      }
    } else {
      ##this is mean of all cages
      if (mainTxt1 !="") {
        maintxt<-paste(mainTxt1,"  Average over all cages",stage.txt,sep="")
      } else {
        maintxt<-paste(mainTxt1,loc.name,"  Average over all cages",stage.txt,sep="")
      }
    }


    ylim<-c(0,yheight)
    xlim<-c(1,n.days)
    plot(NA,NA,type="n",
         lty=1,col=stage.col,lwd=plt.lwd,
         xlim=xlim,ylim=ylim,axes=FALSE,
         xlab="",ylab="",
         main=maintxt)
  
    box()
    axis(side=1,line=0,at=start.month,label=FALSE)
    mtext(text=month.names,side=1,line=0.7,at=mid.month,adj=0.5)
    mtext(text=year,side=1,line=2,at=mid.month,adj=0.5)
    mtext(text="Number of lice per fish",side=2,line=2,at=sum(ylim)/2,adj=0.5)
    axis(side=2,line=0)
    
    if(!onlydata){
      pred.sum<-pred.summary[[merdnumb]][[include]]
      count.sum<-count.summary[[merdnumb]][[include]]
      time.names.est <- rownames(pred.sum)
      if (!is.null(count.sum)) {
        xx<-time.axis[time.names.est][!is.na(count.sum[,"upp0.95"])]
        polygon(c(xx,rev(xx)),c(na.omit(count.sum[,"upp0.95"]),rev(na.omit(count.sum[,"low0.95"]))),col="lightgrey",border=NA)
      }
      
      if (!is.null(pred.sum)) {
        xx<-time.axis[time.names.est][!is.na(pred.sum[,"upp0.95"])]
        polygon(c(xx,rev(xx)),c(na.omit(pred.sum[,"upp0.95"]),rev(na.omit(pred.sum[,"low0.95"]))),col="pink",border=NULL)
        lines(time.axis[time.names.est],pred.sum[,"mean"],lty=1,col=fit.col,lwd=plt.lwd)
      }
    }
    if (!is.null(forecast)) {
      x<-time.axis[names(time.axis)==forecast]
      abline(v=x,lwd=forecast.lwd,lty=forecast.lty)
    }
    
    if (nrow(data.lus)>0) {
      time.names.obs <- rownames(data.lus)
      lines(time.axis[time.names.obs],data.lus[,include],type="b",
            lty=1,col=stage.col,lwd=plt.lwd)
    }
    
    
    if (plot.treat.in.all.panels) {
      pl.treatments(FullData,merdnumb,time.axis,yheight,treat.col,hor.lwd)
    }



##########################################
### Legend
##########################################  

    ltxt<-c(paste(stage.name," data",sep=""))
    llwd<-plt.lwd
    lcol<-c(stage.col)
    if(!onlydata) {
      ltxt<-c(ltxt,paste("estimated " ,stage.name,sep=""))
      lcol<-c(lcol,fit.col)
      llty<-c(1,1)
      llwd<-c(llwd,plt.lwd)
    }
    if(!onlydata){
      if (!is.null(forecast)) {
        ltxt<-c(ltxt,"end of estimation period")
        lcol<-c(lcol,forecast.col)
        llty<-c(llty,forecast.lty)
        llwd<-c(llwd,forecast.lwd)
      }
    }
    
    legend(x="topleft", ltxt,col=lcol,lty=llty,lwd=llwd,seg.len=3,cex=0.85)
    
    if (!onlydata) {
      ltxt<-"95 % C.I. for estimated abundance"
      fcol<-"pink"
      if (!is.null(count.sum)) {
        ltxt<-c(ltxt,"95 % C.I. for lice counts")
        fcol<-c(fcol,"lightgrey")
      }
      legend(x="left", col=fcol,ltxt,cex=0.85,fill=fcol)
    }

##########################################
  }








  if(to.file){
    dev.off()
  }
  
}





pl.cleaner.fish<-function(FullData,merdnumb,E.clf,time.axis,xlim,ylim,clf.sto.col,clf.sto.lty,clf.sto.lwd,
                          clf.ratio.col,clf.ratio.lty,clf.ratio.lwd) {
#######################################
### Evt utsett av rensefisk
#######################################
  
  par(new=T)
  plot(1,0,xlim=xlim,ylim=c(0,10),xlab="",ylab="",axes=FALSE,bty="n",type="n")
  if (is.list(FullData$cleaner.fish.stocked)) {
    cfs<-FullData$cleaner.fish.stocked$RensefiskTot[,merdnumb]/FullData$antall[,merdnumb]
    tmp<-FullData$cleaner.fish.stocked$RensefiskTot[,merdnumb]
  } else {
    cfs<-FullData$cleaner.fish.stocked[,merdnumb]/FullData$antall[,merdnumb]
    tmp<-FullData$cleaner.fish.stocked[,merdnumb]
  }
  cfs<-100*cfs
  cfs[FullData$antall[,merdnumb]==0]<-0
  cfs[tmp>0 & FullData$antall[,merdnumb]==0]<-100 ## added 3/1-2019
  if (!is.null(cfs)) {
    n <- length(cfs)
    ind<-(1:n)[cfs>0]
    nb<-length(ind)
    
    if (nb>0) {
      mn<-"cleaner fish"
      behand.dates<-names(cfs)
      for(i in 1:nb){
        bhd<-behand.dates[ind[i]]
        x<-time.axis[names(time.axis)==bhd]
        yh<-cfs[ind[i]]
        lines(x=c(x,x),y=c(0,yh),col=clf.sto.col,lty=clf.sto.lty,lwd=clf.sto.lwd)
      }
      
      axis(side=4,at=c(0,2.5,5,7.5,10))
      mtext(text="Cleaner fish ratio (%)",side=4,line=3,at=5,adj=0.5)
      
      if(!is.null(E.clf)){
        time.names.clf <- names(E.clf)
        matplot(time.axis[time.names.clf],E.clf,type="l",lty=clf.ratio.lty,col=clf.ratio.col,lwd=clf.ratio.lwd,xlim=xlim,ylim=ylim,axes=FALSE,add=TRUE)
      }
    }
    
  }
}



pl.inf.pressure<-function(FullData,time.axis,xlim,ylim,smp.col,smp.lty,smp.lwd) {
#######################################
### Smittepress
#######################################

  time.names.smp <- names(FullData$smittepress)
  par(new=T)
  plot(1,0,xlim=xlim,ylim=c(0,600),axes=FALSE,xlab="",ylab="",bty="n",type="n")
  matplot(time.axis[time.names.smp],(1/1000)*FullData$smittepress,type="l",lty=smp.lty,col=smp.col,lwd=smp.lwd,xlim=xlim,ylim=ylim,axes=FALSE,add=TRUE)
###axis(side=4,at=c(0,200,400,600))
###mtext(text="Infection pressure (in 1000 Af)",side=4,line=3,at=300,adj=0.5)

}

pl.weight<-function(FullData,merdnumb,time.axis,xlim,ylim,w.col,w.lty,w.lwd) {
#######################################
### Smittepress
#######################################

  time.names.w <- rownames(FullData$vekt)
  par(new=T)
  plot(1,0,xlim=xlim,ylim=c(0,10),axes=FALSE,xlab="",ylab="",bty="n",type="n")
  matplot(time.axis[time.names.w],(1/1)*FullData$vekt[,merdnumb],type="l",lty=w.lty,col=w.col,lwd=w.lwd,xlim=xlim,ylim=ylim,axes=FALSE,add=TRUE)
###axis(side=4,at=c(0,200,400,600))
###mtext(text="Infection pressure (in 1000 Af)",side=4,line=3,at=300,adj=0.5)

}

pl.movement<-function(FullData,merdnumb,time.axis,yheight,movement.col,hor.lwd) {
####################################
### Evt flytting av fisk
####################################
  movement.to<-FullData$divinfo$movement.to[[merdnumb]]
  movement.from<-FullData$divinfo$movement.from[[merdnumb]]
  
  if (length(movement.to)>0) {
  for (i in 1:length(movement.to)) {
    xcoordinat <- time.axis[names(time.axis)==movement.to[i]]
    lines(x=c(xcoordinat,xcoordinat),y=c(0,0.5*yheight),col=movement.col,lty=1,lwd=hor.lwd) #movement to this}
  }
  }
}
                    


pl.treatments<-function(FullData,merdnumb,time.axis,yheight,treat.col,hor.lwd) {
#######################################
### Evt behandlinger
#######################################
  yheight<-yheight*4/5
  
  behand <- FullData$behand[,merdnumb,]
  
  n.med<-FullData$divinfo$n.med
  if (n.med>0) {
    if (!is.matrix(behand)) {
      behand<-as.matrix(behand)
    }
    behand.dates      <- rownames(behand)
   
    for (j in 1:n.med) {
      n <- dim(behand)[1]
      ind<-(1:n)[behand[,j]>0]
      nb<-length(ind)
      if (nb>0) {
        mn<-FullData$divinfo$med.names[j]
        if (FALSE) {
          if (mn=="Azametifos") {
            mn<-"azamethiphos"
          }
          if (mn=="Azametiphos") {
            mn<-"azamethiphos"
          }
          if (mn=="Emamektin") {
            mn<-"emamectin"
          }
          if (mn=="Diflubenzuron") {
            mn<-"diflubenzuron"
          }
          if (mn=="Deltametrin") {
            mn<-"deltamethrin"
          }
          if (mn=="Cypermetrin") {
            mn<-"cypermethrin"
          }
        }
        if (mn=="Hydrogenperoksid") {
          mn<-"H2O2"
        }
        if (mn=="Emamektinbenzoat") {
          mn<-"Eb"
        }   
        
        ii<-0.5
        x<--9999
        for(i in 1:nb){
          ii<-ii+1
          if (ii>4) {ii<-1}
          bhd<-behand.dates[ind[i]]
          xprev<-x
          x<-time.axis[names(time.axis)==bhd]
          if (x<120) {
            ff<-0.5
          } else {
            ff<-0.80
###          ff<-0.85
          }
          lines(x=c(x,x),y=c(0,ff*yheight),col=treat.col,lty=1,lwd=hor.lwd)
          
          if ((x-xprev)>1) {
            text(x=c(x),y=c(ff*yheight+(j/4)*0.15*yheight),mn,col=treat.col,lty=1,lwd=1,cex=0.9)
          }
        }
      }
    }
  }

}

