##' Makes a FullData object with historical data for a single farm.
##'
##' @title Makes FullData for historical data.
##' @inheritParams dummy.for.documentation
##' @return A list of type FullData, see function \link{dummy.for.documentation}
##' @author Magne Aldrin
##' @export
make.FullData.historical <-  function(ProductionDataFromCsv,StartEndDate,AllFarmsData,no.days.back=90,max.stage.life=100) {

  ProductionData<-production.data.from.csv.to.intermediate(ProductionDataFromCsv,
                                                           StartEndDate)
  
  start.year<-2012

  loc.no<-ProductionData$loc.no
  loc.name<-ProductionData$loc.name
  generation.year<-ProductionData$generation.year
  
##################################################
### Informasjon om medikamenter
##################################################
  MedicineInfo<-AllFarmsData$MedicineInfo

##################################################
### Temperatur,
### på ukesnivå,
### en rad per lokalitet i fila, en kolonne per uke 
### + generererer kalender for dataperioden for temperatur
##################################################
  TEMPobj<-extract.and.expand(AllFarmsData$temp,loc.no=loc.no,start.year=start.year)

  temp<-TEMPobj$daily.series
  calendar<-TEMPobj$calendar
  ymd.tot<-TEMPobj$ymd
  first.date.with.temp<-ymd.tot[1]
  last.date.with.temp<-tail(ymd.tot,1)
  
  
  smittepress<-extract.and.expand(AllFarmsData$N.Af.ext,loc.no=loc.no,start.year=start.year)$daily.series
  ymd.smittepress<-names(smittepress)
  first.date.with.smittepress<-ymd.smittepress[1]
  last.date.with.smittepress<-tail(ymd.smittepress,1)
  
  lusetetthetNabo<-extract.and.expand(AllFarmsData$Af.ext,loc.no=loc.no,start.year=start.year)$daily.series
  ymd.lusetetthetNabo<-names(lusetetthetNabo)
  first.date.with.lusetetthetNabo<-ymd.lusetetthetNabo[1]
  last.date.with.lusetetthetNabo<-tail(ymd.lusetetthetNabo,1)

  ind<-which(colnames(AllFarmsData$salinity)==loc.no)
  if (length(ind)==0) {
    print("loc.no not found in salinity! Returns NULL")
    return(NULL)
  }
  ymd.sal<-rownames(AllFarmsData$salinity)
  salinity<-as.vector(AllFarmsData$salinity[,ind])
  names(salinity)<-ymd.sal
  first.date.with.salinity<-ymd.sal[1]
  last.date.with.salinity<-tail(ymd.sal,1)

##################################################
### Antall
##################################################
  tmp<-ProductionData$CloseCountData
  tmp.date<-as.character(tmp$Dato)
  ind<-c(which(colnames(tmp)=="Dato"),which(colnames(tmp)=="Lokalitet"))
  tmp<-as.matrix(tmp[,-ind])
  
  tmp2<-tmp
  tmp2[is.na(tmp2)]<-0
  rsum<-rowSums(tmp2)
  ind<-which(rsum>0)
  if (length(ind)>0) {
    ind1<-ind[1]
    ind2<-tail(ind,1)
  } else {
    print("make.FullData.historical: No fish! Returns NULL")
    return(NULL)
  }
  tmp<-tmp[ind1:ind2,]
  tmp.date<-tmp.date[ind1:ind2]
  tmp.yy<-as.numeric(substr(tmp.date,1,4))
  tmp.mm<-as.numeric(substr(tmp.date,6,7))
  tmp.dd<-as.numeric(substr(tmp.date,9,10))
  
  ymd.fishNo<-ymd.num2char(tmp.yy,tmp.mm,tmp.dd)
  first.date.with.fish.no<-ymd.fishNo[1]
  last.date.with.fish.no<-tail(ymd.fishNo,1)
  if (first.date.with.fish.no<first.date.with.salinity | first.date.with.fish.no<first.date.with.temp) {
    print("first.date.with.fish.no<first.date.with.salinity | first.date.with.fish.no<first.date.with.temp! Returns(NULL)!")
    return(NULL)
  }
  
  last.date<-min(c(last.date.with.temp,last.date.with.smittepress,
                   last.date.with.lusetetthetNabo,
                   last.date.with.salinity,last.date.with.fish.no))
  if (last.date<first.date.with.salinity | last.date<first.date.with.temp | last.date<first.date.with.fish.no) {
    print("Not enough overlap in time span for seawater temperature, salinity and number of fish! Returns(NULL)!")
    return(NULL)
  }


  ind<-match(first.date.with.fish.no,ymd.tot)
  if (is.na(ind)) {
    print("first.date.with.fish.no not within time span for seawater temperature! Returns(NULL)!")
    return(NULL)
  }

  ind.first<-max(1,ind-no.days.back)
  if (ind.first<1) {
    print("first.date.with.fish.no-no.days.back not within time span for seawater temperature! Returns(NULL)!")
    return(NULL)
  }

  if (ind.first>1) {
    ymd.tot<-ymd.tot[-(1:(ind.first-1))]
    temp<-temp[-(1:(ind.first-1))]
    calendar<-calendar[-(1:(ind.first-1)),]
  }
  first.date.in.time.span<-ymd.tot[1]

  if (first.date.with.smittepress>first.date.in.time.span) {
   print("first.date.with.smittepress>first.date.in.time.span! Returns(NULL)!")
   return(NULL)
  } else if (first.date.with.smittepress<first.date.in.time.span) {
    smittepress<-smittepress[ymd.smittepress>=first.date.in.time.span]
    ymd.smittepress<-ymd.smittepress[ymd.smittepress>=first.date.in.time.span]
  }
  
  if (first.date.with.lusetetthetNabo>first.date.in.time.span) {
   print("first.date.with.lusetetthetNabo>first.date.in.time.span! Returns(NULL)!")
   return(NULL)
  } else if (first.date.with.lusetetthetNabo<first.date.in.time.span) {
    lusetetthetNabo<-lusetetthetNabo[ymd.lusetetthetNabo>=first.date.in.time.span]
    ymd.lusetetthetNabo<-ymd.lusetetthetNabo[ymd.lusetetthetNabo>=first.date.in.time.span]
  }
  
  if (first.date.with.salinity>first.date.in.time.span) {
   print("first.date.with.salinity>first.date.in.time.span! Returns(NULL)!")
   return(NULL)
  } else if (first.date.with.salinity<first.date.in.time.span) {
    salinity<-salinity[ymd.sal>=first.date.in.time.span]
    ymd.sal<-ymd.sal[ymd.sal>=first.date.in.time.span]
  }

  n.days<-match(last.date,ymd.tot)
  ymd.tot<-ymd.tot[1:n.days]
  temp<-temp[1:n.days]
  calendar<-calendar[1:n.days,]
 
  smittepress<-smittepress[1:n.days]
  lusetetthetNabo<-lusetetthetNabo[1:n.days]
  salinity<-salinity[1:n.days]


  ###n.days<-match(last.date,ymd.sal)
  ###ymd.sal<-ymd.sal[1:n.days]
  ###salinity<-salinity[1:n.days]
  ###ind<-match(ymd.sal,ymd.tot)
  ###n.days<-length(ymd.tot)
  ###sal<-rep(NA,n.days)
  ###sal<-salinity[ind]
  ###names(sal)<-ymd.tot
  ###salinity<-sal
  ###rm(sal)

  
  n.days<-match(last.date,ymd.fishNo)
  ymd.fishNo<-ymd.fishNo[1:n.days]
  tmp<-tmp[1:n.days,]
  ind<-match(ymd.fishNo,ymd.tot)
  n.merds<-ncol(tmp)
  tmp.cnames<-colnames(tmp)
  merd.no<-extract.numbers.from.stringvec(tmp.cnames)
  merd.names<-paste("Merd",merd.no,sep="")
  n.days<-length(ymd.tot)
  antall<-matrix(NA,nrow=n.days,ncol=n.merds)
  antall[ind,]<-tmp
  rownames(antall)<-ymd.tot
  colnames(antall)<-merd.names
  
  
  id <- antall<0
  if(sum(id,na.rm=T)>0){
    cat("Det er ",sum(id,na.rm=T)," observasjoner som har antall <0\n")
    cat("antall settes til 0 \n")
    antall[id] <- 0
  }
  antall[is.na(antall)]<-0
  
  
##################################################
### Biomasse (kg)
##################################################
  tmp<-ProductionData$CloseBiomassData
  tmp.date<-tmp$Dato
  ind<-c(which(colnames(tmp)=="Dato"),which(colnames(tmp)=="Lokalitet"))
  tmp<-as.matrix(tmp[,-ind])

  tmp.yy<-as.numeric(substr(tmp.date,1,4))
  tmp.mm<-as.numeric(substr(tmp.date,6,7))
  tmp.dd<-as.numeric(substr(tmp.date,9,10))
  ymd.biom<-ymd.num2char(tmp.yy,tmp.mm,tmp.dd)
  

  tmp.n.merds<-ncol(tmp)
  tmp.cnames<-colnames(tmp)
####  tmp.merd.no<-tmp.cnames
####  tmp.merd.no<-rep(NA,n.merds)
  tmp.merd.no<-extract.numbers.from.stringvec(tmp.cnames)
###  tmp.merd.no<-as.numeric(substr(tmp.cnames,9,10))
  if (tmp.n.merds != n.merds){stop("make.FullData.historical: n.merds i biomasseinfo feil")}
  if (any(tmp.merd.no != merd.no)) {stop("make.FullData.historical: merd.no  i biomasseinfo feil")}
  
  ind<-match(ymd.biom,ymd.tot)
  biom<-matrix(NA,nrow=n.days,ncol=n.merds)
  biom[na.omit(ind),]<-tmp[!is.na(ind),]
  rownames(biom)<-ymd.tot
  colnames(biom)<-merd.names
  
  id <- antall<0
  if(sum(id,na.rm=T)>0){
    cat("Det er ",sum(id,na.rm=T)," observasjoner som har biomasse <0\n")
    cat("antall settes til NA \n")
    biom[id] <- 0
  }
  biom[biom==0]<-NA
  
  
##################################################
### Gjennomsnittsvekt per fisk (kg)
##################################################
  vekt<-biom/antall
  
##################################################
### Aktiv/ikke-aktiv-indikator
##################################################
  S<-antall
  S[S>=1]<-1
  
  
##################################################
### Behandling
##################################################
  tmp<-make.treat.and.treat.date(TreatmentData=ProductionData$TreatmentData,
                                 MedicineInfo=MedicineInfo,
                                 ymd.tot=ymd.tot,merd.names=merd.names)

  behand<-tmp$behand
  med.names<-tmp$med.names
  n.med<-tmp$n.med
  med.names.not.in.MedicineInfo<-tmp$med.names.not.in.MedicineInfo
  
##################################################
### Rensefisk
##################################################
  groupnames.cleaner.fish<-c("Rognkjeks","Leppefisk","RensefiskTot")
  n.cf.groupnames<-length(groupnames.cleaner.fish)
  names.cleaner.fish<-vector("list",n.cf.groupnames-1)
  names(names.cleaner.fish)<-groupnames.cleaner.fish[1:(n.cf.groupnames-1)]
  names.cleaner.fish$Rognkjeks<-"Lumpfish"
  names.cleaner.fish$Leppefisk<-c("Goldsinny","Cuckoo (female)","Cuckoo (male)","Corkwing","Ballan","Rock cook","Labrus Ballan")
  
  cleaner.fish.stocked<-vector("list",n.cf.groupnames)
  names(cleaner.fish.stocked)<-groupnames.cleaner.fish
  for (i in 1:n.cf.groupnames) {
    cleaner.fish.stocked[[i]]<-matrix(0,nrow=n.days,ncol=n.merds)
    rownames(cleaner.fish.stocked[[i]])<-ymd.tot
    colnames(cleaner.fish.stocked[[i]])<-merd.names
  }
  n.clf.types<-length(ProductionData$CleanerFishData)

  if (n.clf.types>0) {
    for (fno in 1:n.clf.types) {
      print(fno)
      tmp<-ProductionData$CleanerFishData[[fno]]
      cf.name<-unique(as.character(tmp$TypeNavn))
      if (length(cf.name)>1) {stop(paste("length(cf.name)>1, cf.name=",cf.name))}
      tmp.merd.no<-as.character(tmp$Merd)
      tmp.merd.no<-extract.numbers.from.stringvec(tmp.merd.no)

      ind<-tmp.merd.no%in%merd.no
      if (any(ind)) {
        tmp<-tmp[ind,,drop=F]
        cf.name<-unique(as.character(tmp$TypeNavn))
        if (length(cf.name)>1) {stop(paste("length(cf.name)>1, cf.name=",cf.name))}
        tmp.merd.no<-as.character(tmp$Merd)
        tmp.merd.no<-extract.numbers.from.stringvec(tmp.merd.no)
        
        n.tmp<-nrow(tmp)
        tmp.cf.stocked<-tmp$Antall
        
        tmp.date<-as.character(tmp$Dato)
        tmp.yy<-as.numeric(substr(tmp.date,1,4))
        tmp.mm<-as.numeric(substr(tmp.date,6,7))
        tmp.dd<-as.numeric(substr(tmp.date,9,10))
        tmp.ymd<-ymd.num2char(tmp.yy,tmp.mm,tmp.dd)


        ## Korreksjon lagt inn 3/1-2019
        ind1<-match(tmp.ymd,ymd.tot)
        print(paste("Number of missing dates in cleaner fish data=",sum(is.na(ind1))))
        ind2<-(1:length(ind1))[!is.na(ind1) & tmp.cf.stocked>0]
        ind<-ind1[ind2]
        tmp.merd.no<-tmp.merd.no[ind2]
        tmp.cf.stocked<-tmp.cf.stocked[ind2]
        
        ##FEIL, SLETTET 3/1 2019 ind<-na.omit(match(tmp.ymd,ymd.tot))
        tmp.merd.name <- paste("Merd",tmp.merd.no,sep="")

        print(cf.name)
        if (cf.name%in%names.cleaner.fish$Rognkjeks) {
          for (i in 1:length(ind)) {
            cleaner.fish.stocked$Rognkjeks[ind[i],tmp.merd.name[i]] <-
              cleaner.fish.stocked$Rognkjeks[ind[i],tmp.merd.name[i]]+tmp.cf.stocked[i]
          }
        } else {
          for (i in 1:length(ind)) {
            cleaner.fish.stocked$Leppefisk[ind[i],tmp.merd.name[i]] <-
              cleaner.fish.stocked$Leppefisk[ind[i],tmp.merd.name[i]]+tmp.cf.stocked[i]
          }
        }
      }
    }
    
    for (i in 1:(n.cf.groupnames-1)) {
      cleaner.fish.stocked$RensefiskTot<-cleaner.fish.stocked$RensefiskTot+cleaner.fish.stocked[[i]]
    }
  }



##################################################
### Lusetellinger
##################################################
  tmp<-ProductionData$LiceCountData
  tmp<-na.omit(tmp)
  
  if (nrow(tmp)>0) {
    ind<-duplicated(tmp) 
    tmp<-tmp[!ind,] #remove duplicates if any
    tmp.merd.no<-as.character(tmp$Merd)
    tmp.date<-as.character(tmp$Dato)
    
    data.on.single.fish<-TRUE
    if (!(data.on.single.fish)) {
      n.fisk<-tmp$n.fisk
###      tmp<-tmp[,c("Merd","Dato","Bevegelige","Kj.m.holus","Fastsittende")]
    } else {
###      tmp<-tmp[,c("Merd","Dato","Bevegelige","Kj.m.holus","Fastsittende")]
      tmp.id<-paste(tmp.merd.no,tmp.date,sep="")
      tmp.unique.id<-unique(tmp.id)
      n.tmp<-length(tmp.unique.id)
      ind<-match(tmp.unique.id,tmp.id)
      tmp2<-tmp[ind,]

      n.fisk<-rep(NA,n.tmp)
      for (i in 1:n.tmp) {
        tmp.i<-tmp[tmp.id==tmp.unique.id[i],]
        if (any(is.na(tmp.i))) {
          print("Missing observations in lice data. These should have been remved before this step.")
          browser()
        }
        n.fisk[i]<-nrow(tmp.i)

        ## "Bøttelus" is sometimes coded as fish number -1. This is not a real fish, so
        ## the number of fish counted must be corrected
        no.neg.fish.no<-length((1:n.fisk[i])[tmp.i$FiskNr<0])
        n.fisk[i]<-n.fisk[i]-no.neg.fish.no
##        if (no.neg.fish.no>0) {
##          print(c("no.neg.fish.no",no.neg.fish.no,as.character(tmp.i$Dato)[1],tmp.i$Merd[1]))
##        }
        
        if (n.fisk[i]==0) {
          print(paste("Something wrong in lice data for ",tmp.unique.id[i],sep=""))
          browser()
        }
        if (n.fisk[i]>0) {
          tmp2[i,]$Bevegelige<-sum(tmp.i$Bevegelige)
          tmp2[i,]$Kj.m.holus<-sum(tmp.i$Kj.m.holus)
          tmp2[i,]$Fastsittende<-sum(tmp.i$Fastsittende)
        }
      }
      tmp2<-tmp2[n.fisk>0,]
      tmp<-tmp2
      rm(tmp2)
    }
    
    tmp.merd.no<-as.character(tmp$Merd)
    tmp.merd.no<-extract.numbers.from.stringvec(tmp.merd.no)
    n.tmp<-nrow(tmp)
    
    tmp.date<-as.character(tmp$Dato)
    tmp.yy<-as.numeric(substr(tmp.date,1,4))
    tmp.mm<-as.numeric(substr(tmp.date,6,7))
    tmp.dd<-as.numeric(substr(tmp.date,9,10))
    tmp.ymd<-ymd.num2char(tmp.yy,tmp.mm,tmp.dd)
    lice<-vector("list",n.merds)
    
    for (i in 1:n.merds) {
      m.no<-merd.no[i]
      ind<-(1:n.tmp)[tmp.merd.no==m.no]
      n.m<-length(ind)
      mat<-matrix(NA,nrow=n.m,ncol=4)
      colnames(mat)<-c("n.fish","OM","CH","Af")
      if (n.m>0) {
        tmp.m<-tmp[ind,]
        n.fisk.m<-n.fisk[ind]
        ymd.tmp<-tmp.ymd[ind]
        rownames(mat)<-ymd.tmp
        mat[,"n.fish"]<-n.fisk.m
        mat[,"OM"]<-tmp.m$Bevegelige
        mat[,"CH"]<-tmp.m$Fastsittende
        mat[,"Af"]<-tmp.m$Kj.m.holus
      } 
      lice[[i]]<-mat
    }
    names(lice)<-merd.names
  } else {
    print("make.FullData.historical: No lice data! Returns NULL")
    return(NULL)
    ##    lice<-NULL
 }

  
##################################################
### W-matrise med flytting
##################################################

  tmp<-ProductionData$MovementData
###  tmp<-read.table(file=fn,header=T,colClasses=c("character","character","character","numeric"))
###  ind<-which(colnames(tmp)=="Lokalitet")
###  tmp<-as.matrix(tmp[,-ind])

  if (loc.name=="Kalhag.13220.2015") {
    tmp[tmp$Antall==115444,"Antall"]<-114938
  }
  if (loc.name=="Kjeahola.11913.2014") {
    antall["20160213",6]<-14987
  }
  if (loc.name=="Prestholmen.11972.2015") {
    ### Flytter fisk fra 2 til 3 og videre til 8 samme dag. Takler ikke dette automatisk, og forenkler fra 2 til 8.
    tmp[tmp$Dato=="2016-10-05" & tmp$FraMerd=="Ph02" & tmp$TilMerd=="Ph03" & tmp$Antall==59204,"TilMerd"]<-"Ph08"
    ind<-(1:nrow(tmp))[tmp$Dato=="2016-10-05" & tmp$FraMerd=="Ph03" & tmp$TilMerd=="Ph08" & tmp$Antall==59204]
    tmp<-tmp[-ind,]

    ### Flytter fra 5 til 4 og så tilbake igjen. Forenkler ved å droppe disse to linjene
    ind<-(1:nrow(tmp))[tmp$Dato=="2017-04-20" & tmp$Antall==51599]
    tmp<-tmp[-ind,]
  }
  
  n.tmp<-nrow(tmp)
  CC<-array(0,dim=c(n.days,n.merds+1,n.merds+1),dimnames=list(ymd.tot,c(merd.names,"stocked"),c(merd.names,"removed")))
  movement.from<-vector("list",n.merds)
  movement.to<-vector("list",n.merds)
  names(movement.from)<-merd.names
  names(movement.to)<-merd.names
  if (n.tmp>0) {
    tmp.date<-as.character(tmp$Dato)
    tmp.yy<-as.numeric(substr(tmp.date,1,4))
    tmp.mm<-as.numeric(substr(tmp.date,6,7))
    tmp.dd<-as.numeric(substr(tmp.date,9,10))
    tmp.ymd<-ymd.num2char(tmp.yy,tmp.mm,tmp.dd)
    
    tmp.FraMerd<-paste("Merd",extract.numbers.from.stringvec(tmp$FraMerd),sep="")
    tmp.TilMerd<-paste("Merd",extract.numbers.from.stringvec(tmp$TilMerd),sep="")
    
    for (i in 1:n.tmp) {
###    CC[tmp.ymd[i],tmp$FraMerd[i],tmp$TilMerd[i]]<-tmp$Antall[i]
      ind<-which(ymd.tot==tmp.ymd[i])
      ind<-ind-1 ### Legger i prinsippet flytting til rett før midnatt, men beholder datoene for plott
      CC[ind,tmp.FraMerd[i],tmp.TilMerd[i]]<-tmp$Antall[i]
###    if (tmp.TilMerd[i]!="removed") {  ### OBS Sjekk hvordan vi håndterer removed! 
###    if (tmp$TilMerd[i]!="removed") {
###      movement.from[[tmp$FraMerd[i]]]<-unique(c(movement.from[[tmp$FraMerd[i]]],tmp.ymd[i]))
###      movement.to[[tmp$TilMerd[i]]]<-unique(c(movement.to[[tmp$TilMerd[i]]],tmp.ymd[i]))
      movement.from[[tmp.FraMerd[i]]]<-unique(c(movement.from[[tmp.FraMerd[i]]],tmp.ymd[i]))
      movement.to[[tmp.TilMerd[i]]]<-unique(c(movement.to[[tmp.TilMerd[i]]],tmp.ymd[i]))
###    }
    }
  }

  CCold<-CC
  for (i in 1:(n.days-1)) {
    for (j in 1:n.merds) {
###      QQ<-antall[i+1,j]-antall[i,j]-sum(CC[i,-c(j,n.merds+1),j])+sum(CC[i,j,-c(j,n.merds+1)])
      QQ<-antall[i+1,j]-antall[i,j]-sum(CCold[i,-c(j,n.merds+1),j])+sum(CCold[i,j,-c(j,n.merds+1)])

      if (QQ>0) {
        CC[i,n.merds+1,j]<-QQ
      } else {
        CC[i,j,n.merds+1]<--QQ
      }
      tmp<-antall[i,j]-sum(CC[i,j,-j])
      if (tmp<0) { ### Har ikke helt klar logikk på dette, men det ser ut som det må til
        CC[i,j,n.merds+1]<-CC[i,j,n.merds+1]+tmp
      }
      CC[i,j,j]<-antall[i,j]-sum(CC[i,j,-j])
###      CC[i,j,j]<-antall[i,j]-sum(CCold[i,j,-j])
    }
  }
  i<-n.days
  for (j in 1:n.merds) {
    CC[i,j,j]<-antall[i,j]-sum(CC[i,j,-j])
###    CC[i,j,j]<-antall[i,j]-sum(CCold[i,j,-j])
  }
  
  w<-array(0,dim=c(n.days,n.merds,n.merds+1),dimnames=list(ymd.tot,merd.names,c(merd.names,"removed")))
  for (i in 1:n.days) {
    w[i,,-(n.merds+1)]<-CC[i,-(n.merds+1),-(n.merds+1)]/antall[i,]
  }

## Hvis antall fisk i en merd øker fra en dag til neste i løpet av de
## første 35 dagene etter første utsett, så oppfattes dette
## som innsett av ny fisk, og vi lar lar korresponderende diagonalelement i
## w[i,,] være 1. Det betyr i praksis at innsatt fisk antas å ha samme
## lusetetthet som fisken som allerede er i merda. Dette er ikke helt ideellt,
## for innsatt fisk (smolt) er i prinsippet lusefri, men i starten er
## lusetettheten i en merd veldig nær 0 uansett.
  max.lag<-35
  w[is.na(w)]<-0
  for (i in 1:n.days) {
###  diag(w[i,,-(n.merds+1)])<-pmin(1,diag(w[i,,-(n.merds+1)]))
    if (any(diag(w[i,,])>1)) {
      ind<-which(diag(w[i,,])>1)
      for (j in 1:length(ind)) {
        tt<-max(i-max.lag,1)
        if (antall[tt,ind[j]]==0) {
          print("make.FullData.historical: Diagonal-element i w > 1, antall fisk har økt.")
          print("make.FullData.historical: Vi godtar dette fordi det er maks 35 dager siden utsett.")
          w[i,ind[j],ind[j]]<-1
        } else {
          print("make.FullData.historical: Diagonal-element i w > 1, antall fisk har økt")
          browser()
        }
      }      
    }
    w[i,,(n.merds+1)]<-1-apply(w[i,,-(n.merds+1)],1,sum)
  }
  if (any(w<0)) {
    print("make.FullData.historical: Negative tall i w!")
    browser()
  }
  

     
##################################################
### Diverse informasjon legges i divinfo
##################################################
  
  stage.list<-c("CO","CH","PA","Af","Am","R")
  n.stages<-length(stage.list)
  time.names<-ymd.tot
  n.times<-n.days
  
  tmp<-apply(S,1,sum)
  i<-0
  found.first<-F
  while (!found.first) {
    i<-i+1
    if (tmp[i]>0) {
      first.date.with.fish<-time.names[i]
      found.first<-T
    }
  }
  i<-n.days+1
  found.last<-F
  while (!found.last) {
    i<-i-1
    if (tmp[i]>0) {
      last.date.with.fish<-time.names[i]
      found.last<-T
    }
  }
  
  divinfo<-list(loc.no=loc.no,
                loc.name=loc.name,
                generation.year=generation.year,
                stage.list=stage.list,
                n.stages=n.stages,
                max.stage.life=max.stage.life,
                merd.names=merd.names,
                n.merds=n.merds,
                time.names=time.names,
                n.times=n.times,
                first.date.with.fish=first.date.with.fish,
                last.date.with.fish=last.date.with.fish,
                med.names.not.in.MedicineInfo=med.names.not.in.MedicineInfo,
                med.names=med.names,
                n.med=n.med,
                MedicineInfo=MedicineInfo,
                movement.from=movement.from,
                movement.to=movement.to)




  FullData <- list(calendar=calendar,
                   lice=lice,
                   vekt=vekt,
                   antall=antall,
                   biom=biom,
                   behand=behand,
                   cleaner.fish.stocked=cleaner.fish.stocked,
                   temp=temp,
                   salinity=salinity,
                   w=w,
                   S=S,
                   smittepress=smittepress,
                   lusetetthetNabo=lusetetthetNabo,
                   divinfo=divinfo)

  FullData$behand<-move.treatments.at.count.dates(FullData)

  FullData$covariates<-make.covariates(FullData)

  return(FullData)
}
