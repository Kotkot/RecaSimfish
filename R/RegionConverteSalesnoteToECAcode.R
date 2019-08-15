##' RegionConverteSalesnoteToECAcode: Region: sale notes data and ECA code
##'
##' @title RegionConverteSalesnoteToECAcode
##' @return A data frame with two columns; column 1: sale notes region 0:81, column 2: corresponding ECA-region 1:9 (the sale notes regions are based on the regions from 2015/16) 
##' @author Ingunn Fride Tvete
##' @export
RegionConverteSalesnoteToECAcode<-function(){
  
    Region1<-0:81
  Region2<-c(6, 2, 2, 1, 4, 5, 8, 9, NA, NA, 2, 2, 3, 2, 2, 2, 2, 2, 2, NA, 7, 7, 7, 7, 2, 7, 7, 7, NA, NA, NA, NA, NA, NA, NA, NA, 3, 3, 3, 3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  Region<-data.frame(Region1,Region2)
  names(Region)<-c("SaleNotesRegion","ECARegion")
  return(Region)
}
