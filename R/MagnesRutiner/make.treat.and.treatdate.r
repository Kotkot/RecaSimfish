##' Make intermediate treatment object from treatment information.
##' @title Make intermediate treatment object from treatment information
##' @param TreatmentData - A data frame with information on treatments,
##'        see documentation of the element Treatment in the
##'        object ProductionDataFromCsv documented in \link{dummy.for.documentation}
##' @param MedicineInfo - A data frame with information on different type of treatments,
##'        with one row per treatment. The column names are
##'        medicine, product.name, feed, delay, duration.const, temp.dependent,
##'        CH.effect, PA.effect, A.effect and egg.effect.
##' @param ymd.tot Vector of character strings with dates of the form "yyyymmdd"
##' @param merd.names Vector of character strings with cage names
##' @return A list with elements behand, med.names and n.med.
##'         Here, behand is an n.times x n.merds x n.med array with 1
##'         at times/cages with treatment of a given type, and 0 elsewhere,
##'         med.names contains the names of the treatment types,
##'         and n.med is the number of different treatment types.
##' @author Magne Aldrin
make.treat.and.treat.date<-function(TreatmentData,MedicineInfo,ymd.tot,merd.names) {

  n.days<-length(ymd.tot)
  n.merds<-length(merd.names)
  
  tmp<-TreatmentData
  
  if (nrow(tmp)>=1) {
    tmp$Produkt<-treatname.to.standardtreat(as.character(tmp$Produkt))
    tmp.med<-tmp$Produkt
    ind<-tmp.med%in%MedicineInfo$medicine
    tmp2<-tmp[!ind,]
    tmp<-tmp[ind,]
    tmp.med<-tmp$Produkt
    if (any(tmp.med=="NA")) {
      cat("Medisintype NA")
    }
  } else {
    tmp2<-tmp
  }
  
  if (nrow(tmp2) >=1) {
    tmp.med<-tmp2$Produkt
    med.names.not.in.MedicineInfo<-unique(tmp.med)
    txt<-"Minst en medisin som ikke er med i MedisinInfo for\n"
    cat(txt)
    txt<-paste("denne lokaliteten. Det er\n",sep="")
    cat(txt)
    print(med.names.not.in.MedicineInfo)
  } else {
    med.names.not.in.MedicineInfo<-NULL
  }
  if (nrow(tmp) >=1) {
    tmp.med<-tmp$Produkt

    if (any(tmp.med=="Ideal")) {
      ind<-(1:length(tmp.med))[tmp.med=="Ideal"]
      tmp.med[ind]<-paste(tmp.med[ind],
                          ".CH",sprintf('%03d',tmp$CHmortality[ind]),
                          ".PA",sprintf('%03d',tmp$PAmortality[ind]),
                          ".A",sprintf('%03d',tmp$Amortality[ind]),sep="")
    }
        
    med.names<-unique(tmp.med)
    n.med<-length(med.names)
    
    tmp.merd.no<-as.character(tmp$Merd)
    tmp.merd.no<-extract.numbers.from.stringvec(tmp.merd.no)
    n.tmp<-nrow(tmp)
    
    tmp.date<-as.character(tmp$Dato)
     
    ## Eksempel på justering av dato, hvos nødvendig
    ##    if (loc.name=="Tendalsvik") {
    ##      tmp.date[tmp.merd.no==11 & tmp$Aktiv.stoff=="Hydrogenperoksid" &
    ##               tmp.date=="2014-02-10"]<-"2014-02-11"
    ##    }
    
    tmp.yy<-as.numeric(substr(tmp.date,1,4))
    tmp.mm<-as.numeric(substr(tmp.date,6,7))
    tmp.dd<-as.numeric(substr(tmp.date,9,10))
    tmp.ymd<-ymd.num2char(tmp.yy,tmp.mm,tmp.dd)
    
    behand<-array(0,dim=c(n.days,n.merds,n.med),dimnames=list(ymd.tot,merd.names,med.names))
    ind<-match(tmp.ymd,ymd.tot)
    tmp.merd.name <- paste("Merd",tmp.merd.no,sep="")
    for (i in 1:length(ind)) {
      if (tmp.merd.name[i]%in%merd.names) {
        behand[max(ind[i]-1,1),tmp.merd.name[i],tmp.med[i]] <- 1
      } else {
        txt1<-paste(tmp.merd.name[i]," in treatment data are not included\n",sep="")
        txt2<-"in other production data and is therefore ignored\n"
        cat(txt1)
        cat(txt2)
      }        
    }

    ## Fjerner påfølgende registreringer av Termisk behandling på samme merd når
    ## disse bare har én dags mellomrom. Beholder da bare den første i hver serie.
    if ("Termisk" %in% med.names) {
      for (merd in 1:n.merds) {
        tmp<-behand[,merd,"Termisk"]
        for (tt in 2:n.days) {
          if ((tmp[tt]==1) & (tmp[tt-1]==1)) {
            behand[tt,merd,"Termisk"]<-0
          }
        }
      }
    }
          

        
  } else {
    
    behand<-NULL
    med.names<-NULL
    n.med<-0
  }

  res<-list(behand=behand,med.names=med.names,n.med=n.med,
            med.names.not.in.MedicineInfo=med.names.not.in.MedicineInfo)

  res
}
