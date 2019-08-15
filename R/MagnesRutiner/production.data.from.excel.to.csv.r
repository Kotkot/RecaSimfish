##' Converts data from excel files to csv files.
##' 
##' @title Converts data from excel files to csv files.
##' @param pathExcelSheets - path to file with an Excel file with production data.
##' @param pathCsvData - path to directory where csv files should be stored.
##' @return NULL
##' @author Magne Aldrin
##' @export
production.data.from.excel.to.csv<-function(pathExcelSheets,pathCsvData) {

library(readxl)     # read_excel
library(gdata)      # sheetCount, sheetNames
library(data.table) # fwrite
library(gtools)     # mixedsort

  ## A penalty to be applied when deciding to print numeric values in fixed or exponential
  ## notation. Positive values bias towards fixed and negative towards scientific notation:
  ## fixed notation will be preferred unless it is more than scipen digits wider.
  
  options(scipen = 999) 
  

  ## Make a list of all Excel spreadsheets
  fileNames <- list.files(pathExcelSheets,pattern=".xlsx")
  fileNames
  ##  [1] "BBA-Djupevik.xlsx"    "BBB-Fosså.xlsx"       "BBC-Herøy.xlsx"      
  ##  [4] "BBD-Kjeahola.xlsx"    "BBE-Halsavik.xlsx"    "BBF-Kobbavik.xlsx"   
  ##  [7] "BBH-Langavik.xlsx"    "BBI-Lindvik.xlsx"     "BBJ-Munkholmen.xlsx" 
  ## [10] "BBL-Ringja.xlsx"      "BBM-Vindsvik.xlsx"    "BBO-Bastlid.xlsx"    
  ## [13] "BBR-Skiftesvika.xlsx" "Hesbygrunnen.xlsx"    "Hidlekjerringa.xlsx" 
  ## [16] "Jorstadskjera.xlsx"   "Kalhag.xlsx"          "Langholmen.xlsx"     
  ## [19] "Lava.xlsx"            "Prestholmen.xlsx"    

                                        ## Name of fish farms
  fishFarmNames <- unlist(strsplit(fileNames,split=".xlsx"))
  fishFarmNames
  ##  [1] "BBA-Djupevik"    "BBB-Fosså"       "BBC-Herøy"       "BBD-Kjeahola"   
  ##  [5] "BBE-Halsavik"    "BBF-Kobbavik"    "BBH-Langavik"    "BBI-Lindvik"    
  ##  [9] "BBJ-Munkholmen"  "BBL-Ringja"      "BBM-Vindsvik"    "BBO-Bastlid"    
  ## [13] "BBR-Skiftesvika" "Hesbygrunnen"    "Hidlekjerringa"  "Jorstadskjera"  
  ## [17] "Kalhag"          "Langholmen"      "Lava"            "Prestholmen"    
  
  ## Remove the first part of the names from Marine Harvest
  fishFarmNames[1:13] <- substring(fishFarmNames[1:13],first=5)
  fishFarmNames
  ##  [1] "Djupevik"       "Fosså"          "Herøy"          "Kjeahola"      
  ##  [5] "Halsavik"       "Kobbavik"       "Langavik"       "Lindvik"       
  ##  [9] "Munkholmen"     "Ringja"         "Vindsvik"       "Bastlid"       
  ## [13] "Skiftesvika"    "Hesbygrunnen"   "Hidlekjerringa" "Jorstadskjera" 
  ## [17] "Kalhag"         "Langholmen"     "Lava"           "Prestholmen"   
  

#########################################################
### Make csv files of all Excel sheets for all fish farms
#########################################################

## Names for the Excel sheets
  sheetNames <- c("Inventory","Movements","Cleanerfish","Licecounting","Lice Treatments","Environment")
  
                                        # For all Excel input files
  for(f in 1:length(fileNames)){
    print(paste("Filnavn: ",fileNames[f],sep=""))
    ## For all sheets
    for(s in 1:length(sheetNames)){
      print(paste("    Ark nummer: ",s,sep=""))
      ## Specify column type for "Inventory"; some cages are strings and some are numbers
      ## Read in 'Cage' as text
      if(s==1){  
        colTypes <- c("text","numeric","text","numeric","numeric","text","text","numeric","date",rep("numeric",14))
        input <- read_excel(paste(pathExcelSheets,fileNames[f],sep=""),sheet=s,col_types=colTypes)
      } else{
        input <- read_excel(paste(pathExcelSheets,fileNames[f],sep=""),sheet=s)
      }
      ifelse(s==5,nameSheet <- "LiceTreatments",nameSheet <- sheetNames[s])
      outputFileName <- paste(pathCsvData,nameSheet,"/",fishFarmNames[f],"_",nameSheet,".csv",sep="")
      write.csv(input,outputFileName,row.names=FALSE)
      ##}
    }
  }
  ##fishFarmNames.sorted <- sort(fishFarmNames)
  ##fishFarmNames.sorted
}
