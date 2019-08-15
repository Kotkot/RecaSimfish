#############################################################
# From the Excel sheet "Inventory" for all fish farms, make
# a list of the generations and their start and end date.
#############################################################
##' Makes an overview of generations in Inventory data, with from-to period
##' and generation year.
##'
##' @title Generation overview
##' @param Inventory A data frame based on an Inventory csv-file
##' @return A data frame with one row per generation and columns
##'         named "Generation","FromDate","ToDate".
##' @author Magne Aldrin
##' @export
MakeGenerationStartEndDate <- function(Inventory){

  ## Years with generations
  generations <- as.integer(names(table(Inventory$Generation)))
  ## Sort by Generation and Date
  Inventory.sorted <- Inventory[order(Inventory$Generation,Inventory$Date),]
  ## Vectors with start and end date for the generations
  startDate <- NULL
  endDate <- NULL
  ## For all generation years
  for(g in generations){
    dateVec <- Inventory.sorted$Date[Inventory.sorted$Generation==g]
    startDate <- c(startDate,as.Date(dateVec[1]))
    endDate <- c(endDate,as.Date(dateVec[length(dateVec)]))

  }
  ## Make a data frame with dates
  GenerationStartEndDate <- data.frame(generations,as.Date(startDate,origin="1970-01-01"),as.Date(endDate,origin="1970-01-01"))
  colnames(GenerationStartEndDate) <- c("Generation","FromDate","ToDate")

  GenerationStartEndDate
}


######################################################################
### For lice treatments, make data frames with columns
### Date - Cage - TreatmentProduct - SiteName.
######################################################################

MakeTreatmentData <- function(Treatment,StartEndDate){  
  ## Sort by Cage and Date
  Treatment.sorted <- Treatment[order(Treatment$Cage,Treatment$Date),]
  rows <- which(as.Date(Treatment.sorted$Date)>StartEndDate$FromDate & as.Date(Treatment.sorted$Date)<=StartEndDate$ToDate)
  ##  res<-Treatment.sorted[rows,c(4,3,5,1)]
  cn<-colnames(Treatment.sorted)
  if (any(cn=="CHmortality")) {
    res<-Treatment.sorted[rows,c("Date","Cage","TreatmentProduct","SiteName",
                                 "CHmortality","PAmortality","Amortality")]
    colnames(res)<-c("Dato","Merd","Produkt","Lokalitet",
                     "CHmortality","PAmortality","Amortality")
  } else {
    res<-Treatment.sorted[rows,c("Date","Cage","TreatmentProduct","SiteName")]
    colnames(res)<-c("Dato","Merd","Produkt","Lokalitet")
  }
  
  res
}


###########################################################################
### From lice counts, make data frame with columns
### Date - Cage - FishNr - Mobile - Female Ovigorous - Chalimus - SiteName.
###########################################################################

MakeLiceCountData <- function(Count,StartEndDate){  
  Count.sorted <- Count[order(Count$Cage,Count$Date),]
  rows <- which(as.Date(Count.sorted$Date)>=StartEndDate$FromDate & as.Date(Count.sorted$Date)<=StartEndDate$ToDate)
  ##  res<-Count.sorted[rows,c(4,3,5,6,7,8,1)]
  res<-Count.sorted[rows,c("Date","Cage","FishNr",
                           "Lice..Mobile.","Lice..Female.Ovigorous.","Lice..Chalimus.","SiteName")]
  colnames(res) <- c("Dato","Merd","FiskNr","Bevegelige","Kj.m.holus","Fastsittende","Lokalitet")

  res
}


#################################################################################
### For movements of fish, make a data frame with all internal movements with columns
### Date - FromCage - ToCage - FishCount - SiteName.
#################################################################################

MakeMovementData <- function(Movement,StartEndDate){
  Movement <- Movement[Movement$MovementType=="Internal Movement",]
  ## Sort by Date and FromCages
  Movement.sorted <- Movement[order(Movement$TransactionDate,Movement$FromCages),]
  rows <- which(as.Date(Movement.sorted$TransactionDate)>=StartEndDate$FromDate & as.Date(Movement.sorted$TransactionDate)<=StartEndDate$ToDate)
  ##  res<-Movement.sorted[rows,c(1,4,9,13,2)]
  res<-Movement.sorted[rows,c("TransactionDate","FromCages","ToCage","FishCount","FromSite")]
  colnames(res) <- c("Dato","FraMerd","TilMerd","Antall","Lokalitet")

  res
}


###################################################################################
### From cleaner fish information, make a list with with one element for each cleaner fish
### type with a data frame with columns
# Date - Cage - PondingCount - SpeciesName - SpeciesID - SiteName.
#
# Only dates with insertion of cleaner fishes are printed to file.
###################################################################################

MakeCleanerfishData <- function(Cleanerfish,StartEndDate){
    # Sort by cage and date
  Cleanerfish.sorted <- Cleanerfish[order(Cleanerfish$Cage,Cleanerfish$Date),]
  rows <- which(as.Date(Cleanerfish.sorted$Date)>=StartEndDate$FromDate & as.Date(Cleanerfish.sorted$Date)<=StartEndDate$ToDate)
  ## Data for the generation
  Cleanerfish.gen <- Cleanerfish.sorted[rows,]
  ## Cleaner fish types for the generation
  types <- names(table(Cleanerfish.gen$SpeciesName))
  typesCount <- as.numeric(table(Cleanerfish.gen$SpeciesName))
  types <- types[typesCount>0]
  ## For all cleaner fish types
  for(t in 1:length(types)){
    Cleanerfish.type <- Cleanerfish.gen[Cleanerfish.gen$SpeciesName==types[t],]
    ##    tmp<-Cleanerfish.type[,c(6,3,7,4,5,1)]
    tmp<-Cleanerfish.type[,c("Date","Cage","PondingCount","SpeciesName","SpeciesNS9400ID","SiteDescriptionName")]
    colnames(tmp) <- c("Dato","Merd","Antall","TypeNavn","TypeNr","Lokalitet")
    res[[t]]<-tmp
  }
  names(res)<-types

  res
}


#####################################################################################
### From cleaner fish information, make a list with with one element for each cleaner fish
### type with a data frame with columns
### Date - Cage - PondingCount - SpeciesName - SpeciesID - SiteName.
### all dates for the generation are printed to file. I.e. zeros are inserted if there
### is no insertion of cleaner fishes.
#####################################################################################

MakeCleanerfishDataAllDates <- function( Cleanerfish,StartEndDate){
  ## Sort by cage and date
  Cleanerfish.sorted <- Cleanerfish[order(Cleanerfish$Cage,Cleanerfish$Date),]
  ## Start date for cleaner fish is set to 30 days before the generation
  startDate <- StartEndDate$FromDate-30
  endDate <- StartEndDate$ToDate
  dateVec <- seq(as.Date(startDate),as.Date(endDate),by=1)
  rows <- which(as.Date(Cleanerfish.sorted$Date)>=startDate & as.Date(Cleanerfish.sorted$Date)<=endDate)
  ## Cleaner fish data exists for the generation
  if(length(rows)==0){
    res<-NULL
  } else if(length(rows)>0){
    res<-list()
    ## Data for the generation
    Cleanerfish.gen <- Cleanerfish.sorted[rows,]
    ## Cleaner fish types for the generation
    types <- names(table(Cleanerfish.gen$SpeciesName))
    typesCount <- as.numeric(table(Cleanerfish.gen$SpeciesName))
    types <- types[typesCount>0]
    ## For all cleaner fish types
    for(t in 1:length(types)){
      Cleanerfish.type <- Cleanerfish.gen[Cleanerfish.gen$SpeciesName==types[t],]
      ## Find cages
      cages <- names(table(Cleanerfish.type$Cage))
      cagesCount <- as.numeric(table(Cleanerfish.type$Cage))
      cages <- cages[cagesCount>0]
      nCages <- length(cages)    
      outMatrix <- matrix(0,nrow=nCages*length(dateVec),ncol=6)
      outMatrix <- data.frame(outMatrix)
      colnames(outMatrix) <- c("Dato","Merd","Antall","TypeNavn","TypeNr","Lokalitet")
      outMatrix[,1] <- rep(dateVec,nCages)
      outMatrix[,2] <- rep(cages,each=length(dateVec))
      outMatrix[,4] <- rep(Cleanerfish.type$SpeciesName[1],dim(outMatrix)[1])
      outMatrix[,5] <- rep(Cleanerfish.type$SpeciesNS9400ID[1],dim(outMatrix)[1])
      outMatrix[,6] <- rep(Cleanerfish.type$SiteDescriptionName[1],dim(outMatrix)[1])
      ## For all rows in the data matrix, i.e. dates where cleaner fish are inserted
      for(r in 1:dim(Cleanerfish.type)[1]){
        date <- as.Date(Cleanerfish.type$Date[r])
        cage <- Cleanerfish.type$Cage[r]
        rowOut <- which(outMatrix$Dato==date & outMatrix$Merd==cage)
        outMatrix[rowOut,3] <- outMatrix[rowOut,3]+Cleanerfish.type$PondingCount[r]
      }
      res[[t]]<-outMatrix
    } # t
    names(res)<-types
  } # if(length(rows)>0)

  res
}


#############################################################################################
### From "Inventory" information, make data frame
### with the close count (inventory.variable="count"), i.e. the number of fish at the ent of the day,
### or the close biomass (inventory.variable="biomass"), i.e. the fish biomass at the end of the day,
### with columns Date - Cage1 - Cage2 - ... - CageN - SiteName.
### Take maximum to handle equal dates. 
#############################################################################################


MakeInventoryData <- function(Inventory,StartEndDate,inventory.variable="count"){
  ## Sort by cage and date
  Inventory.sorted <- Inventory[order(Inventory$Cage,Inventory$Date),]
  rows <- which(as.Date(Inventory.sorted$Date)>=StartEndDate$FromDate & as.Date(Inventory.sorted$Date)<=StartEndDate$ToDate)
  ## Data for the generation
  Inventory.gen <- Inventory.sorted[rows,]
  ## Find cages
  cages <- names(table(Inventory.gen$Cage))
  ## Cages with no data for the generation
  cagesCount <- as.numeric(table(Inventory.gen$Cage))
  cages <- cages[cagesCount>0]
  ## Remove temporary cages, i.e. cages with sum(Inventory)=0; Halsavik 2014
  sumInventory <- rep(NA,length(cages))
  for(c in 1:length(cages)){
    rowsForCage <- which(Inventory.gen$Cage==cages[c])
    sumInventory[c] <- sum(Inventory.gen$CloseCount[rowsForCage])
  }
  if(any(sumInventory==0)){
    removeCages <- cages[sumInventory==0]
    removeRows <- NULL
    for(c in 1:length(removeCages)){
      removeRows <- c(removeRows,which(Inventory.gen$Cage==removeCages[c]))
    }
    cages <- cages[sumInventory>0]
    Inventory.gen <- Inventory.gen[-removeRows,]
  }
  ## Number of cages
  nCages <- length(cages)
  ## Sort the cages (some are strings and some are numbers)
  cages <- mixedsort(cages)
  ## Year, start and end date for generation
  gen <- StartEndDate$Generation
  startDate <- StartEndDate$FromDate
  endDate <- StartEndDate$ToDate
  dateVec <- seq(as.Date(startDate),as.Date(endDate),by=1)
  equalDates <- matrix(0,nrow=length(dateVec),ncol=nCages)
  outMatrix <- matrix(NA,nrow=length(dateVec),ncol=nCages+2)
  outMatrix <- data.frame(outMatrix)
  colnames(outMatrix) <- c("Dato",cages,"Lokalitet")
  outMatrix[,1] <- dateVec
  outMatrix[,dim(outMatrix)[2]] <- rep(Inventory$SiteNr[1],dim(outMatrix)[1])
  ## For all rows in the data matrix
  for(r in 1:dim(Inventory.gen)[1]){
    date <- as.Date(Inventory.gen$Date[r])
    cage <- Inventory.gen$Cage[r]
    equalDates[which(dateVec==date),which(cages==cage)] <- equalDates[which(dateVec==date),which(cages==cage)]+1
    rowOut <- which(outMatrix$Dato==date)
    colOut <- which(colnames(outMatrix)==cage)        
    ## Get close count or biomass
    if (inventory.variable=="count") {
      ## Negative values for close count are set to 0
      data <- max(0,Inventory.gen$CloseCount[r])
    } else if (inventory.variable=="biomass") {
      data <- Inventory.gen$CloseBiomass[r]
    }
    ## Put zeros on all remaining rows, avoid cases with equal dates
    if(data==0 & equalDates[which(dateVec==date),which(cages==cage)]==1){
      outMatrix[rowOut:dim(outMatrix)[1],colOut] <- 0
    } else{
      ## Maximum of existing and new value (if several dates)
      outMatrix[rowOut,colOut] <- max(data,outMatrix[rowOut,colOut],na.rm=TRUE)
      outMatrix[rowOut,colOut] <- round(outMatrix[rowOut,colOut],0)
    }
  }

  res<-outMatrix

  res
}


OLD.MakeInventoryData <- function(CloseCount,StartEndDate,inventory.variable="count"){
  ## Sort by cage and date
  CloseCount.sorted <- CloseCount[order(CloseCount$Cage,CloseCount$Date),]
  rows <- which(as.Date(CloseCount.sorted$Date)>=StartEndDate$FromDate & as.Date(CloseCount.sorted$Date)<=StartEndDate$ToDate)
  ## Data for the generation
  CloseCount.gen <- CloseCount.sorted[rows,]
  ## Find cages
  cages <- names(table(CloseCount.gen$Cage))
  ## Cages with no data for the generation
  cagesCount <- as.numeric(table(CloseCount.gen$Cage))
  cages <- cages[cagesCount>0]
  ## Remove temporary cages, i.e. cages with sum(CloseCount)=0; Halsavik 2014
  sumCloseCount <- rep(NA,length(cages))
  for(c in 1:length(cages)){
    rowsForCage <- which(CloseCount.gen$Cage==cages[c])
    sumCloseCount[c] <- sum(CloseCount.gen$CloseCount[rowsForCage])
  }
  if(any(sumCloseCount==0)){
    removeCages <- cages[sumCloseCount==0]
    removeRows <- NULL
    for(c in 1:length(removeCages)){
      removeRows <- c(removeRows,which(CloseCount.gen$Cage==removeCages[c]))
    }
    cages <- cages[sumCloseCount>0]
    CloseCount.gen <- CloseCount.gen[-removeRows,]
  }
  ## Number of cages
  nCages <- length(cages)
  ## Sort the cages (some are strings and some are numbers)
  cages <- mixedsort(cages)
  ## Year, start and end date for generation
  gen <- StartEndDate$Generation
  startDate <- StartEndDate$FromDate
  endDate <- StartEndDate$ToDate
  dateVec <- seq(as.Date(startDate),as.Date(endDate),by=1)
  equalDates <- matrix(0,nrow=length(dateVec),ncol=nCages)
  outMatrix <- matrix(NA,nrow=length(dateVec),ncol=nCages+2)
  outMatrix <- data.frame(outMatrix)
  colnames(outMatrix) <- c("Dato",cages,"Lokalitet")
  outMatrix[,1] <- dateVec
  outMatrix[,dim(outMatrix)[2]] <- rep(Inventory$SiteNr[1],dim(outMatrix)[1])
  ## For all rows in the data matrix
  for(r in 1:dim(CloseCount.gen)[1]){
    date <- as.Date(CloseCount.gen$Date[r])
    cage <- CloseCount.gen$Cage[r]
    equalDates[which(dateVec==date),which(cages==cage)] <- equalDates[which(dateVec==date),which(cages==cage)]+1
    rowOut <- which(outMatrix$Dato==date)
    colOut <- which(colnames(outMatrix)==cage)        
    ## Get close count or biomass
    if (inventory.variable=="count") {
      ## Negative values for close count are set to 0
      data <- max(0,CloseCount.gen$CloseCount[r])
    } else if (inventory.variable=="biomass") {
      data <- CloseCount.gen$CloseBiomass[r]
    }
    ## Put zeros on all remaining rows, avoid cases with equal dates
    if(data==0 & equalDates[which(dateVec==date),which(cages==cage)]==1){
      outMatrix[rowOut:dim(outMatrix)[1],colOut] <- 0
    } else{
      ## Maximum of existing and new value (if several dates)
      outMatrix[rowOut,colOut] <- max(data,outMatrix[rowOut,colOut],na.rm=TRUE)
      outMatrix[rowOut,colOut] <- round(outMatrix[rowOut,colOut],0)
    }
  }

  res<-outMatrix

  res
}


#############################################################################################
### From "Environment" information (i.e. for all fish farms), make output 
### files for the medium temperature (environment.variable="temp") or for the medium salinity
### (environment.variable="salinity")
### for each generation. An empty file is written if no data exist. The files have the format
### Date - Temperature/Salinity - SiteName.
#############################################################################################

MakeEnvironmentData <- function(Environment,StartEndDate,environment.variable="temp"){
  ## Sort by date
  Environment.sorted <- Environment[order(Environment$Date),]
  rows <- which(as.Date(Environment.sorted$Date)>=StartEndDate$FromDate & as.Date(Environment.sorted$Date)<=StartEndDate$ToDate)
  ## Data for the generation
  Environment.gen <- Environment.sorted[rows,]
      ## Find column number with temperature or salinity data
  if (environment.variable=="temp"){
    colNr <- which(names(Environment.gen)=="X.Sea.Temperature..Medium..")
    colNames <- c("Dato","Temperatur (Medium)","Lokalitet")
  } else if (environment.variable=="salinity") {
    colNr <- which(names(Environment.gen)=="X.Salinity..Medium..")
    colNames <- c("Dato","Salinitet (Medium)","Lokalitet")
  }
  if(length(colNr)==0){
    res<-NULL
  } else{
    ## Start and end date for the generation
    startDate <- StartEndDate$FromDate
    endDate <- StartEndDate$ToDate
    dateVec <- seq(as.Date(startDate),as.Date(endDate),by=1)
    outMatrix <- matrix(NA,nrow=length(dateVec),ncol=3)
    outMatrix <- data.frame(outMatrix)
    colnames(outMatrix) <- colNames
    outMatrix[,1] <- dateVec
    outMatrix[,3] <- rep(Environment.gen$SiteName[1],dim(outMatrix)[1])
    ## For all rows in the data matrix
    for(r in 1:dim(Environment.gen)[1]){
      date <- as.Date(Environment.gen$Date[r])
      rowOut <- which(outMatrix$Dato==date)
      outMatrix[rowOut,2] <- Environment.gen[r,colNr]
    }
    ## returns NULL if no data exist or all data are zero;
    if(all(is.na(outMatrix[,2])) | sum(outMatrix[,2],na.rm=TRUE)==0){
      res<-NULL
    } else{
      res<-outMatrix
    }
  }

  res
}


