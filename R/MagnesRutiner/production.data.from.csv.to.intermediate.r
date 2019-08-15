##' Transforms data from raw csv-type format to intermediate format to
##' be used in make.FullData function.
##'
##' @title Transform data from raw to intermediate format
##' @inheritParams dummy.for.documentation
##' @return A list with ProductionData
##' @author Magne Aldrin
production.data.from.csv.to.intermediate<-function(ProductionDataFromCsv,StartEndDate) {

  genyear <- StartEndDate$Generation

  loc.name<-ProductionDataFromCsv$Inventory$SiteName[1]
  loc.no<-ProductionDataFromCsv$Inventory$SiteNr[1]

  TreatmentData<-MakeTreatmentData(ProductionDataFromCsv$Treatment,StartEndDate)
  LiceCountData<-MakeLiceCountData(ProductionDataFromCsv$Count,StartEndDate)
  MovementData<-MakeMovementData(ProductionDataFromCsv$Movement,StartEndDate)
  ## Cleaner fish files with all dates for the generation
  CleanerFishData<-MakeCleanerfishDataAllDates(ProductionDataFromCsv$Cleanerfish,StartEndDate)
  ## Close count
  CloseCountData<-MakeInventoryData(ProductionDataFromCsv$Inventory,StartEndDate,inventory.variable="count")
  ## Close biomass
  CloseBiomassData<-MakeInventoryData(ProductionDataFromCsv$Inventory,StartEndDate,inventory.variable="biomass")
  ## Temperature
  TemperatureData<-MakeEnvironmentData(ProductionDataFromCsv$Environment,StartEndDate,environment.variable="temp")
  ## Salinity
  SalinityData<-MakeEnvironmentData(ProductionDataFromCsv$Environment,StartEndDate,environment.variable="salinity")
  
  ProductionData<-list(StartEndDate=StartEndDate,
                       loc.no=loc.no,loc.name=loc.name,generation.year=genyear,
                       TreatmentData=TreatmentData,
                       MovementData=MovementData,
                       LiceCountData=LiceCountData,
                       CleanerFishData=CleanerFishData,
                       CloseCountData=CloseCountData,
                       CloseBiomassData=CloseBiomassData,
                       TemperatureData=TemperatureData,
                       SalinityData=SalinityData)
}




