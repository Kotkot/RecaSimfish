##' GearConverteSalesnoteToECAcode: Gear; sale notes data and ECA code
##'
##' @title RegionConverteSalesnoteToECAcode
##' @return A data frame with two columns; column 1: sale notes gear, column 2: corresponding ECA-gear 1:4 (the sale notes gear is based on gear used in 2015/16)
##' @author Ingunn Fride Tvete
##' @export
GearConverteSalesnoteToECAcode<-function(){
  
  Gear1<-c("Andre liner", "Annet", "Autoline", "Bomtrål", "Bunntrål", "Bunntrål par", "Dorg/harp/snik", "Drivgarn", "Flyteline", "Flytetrål", "Havteiner", "Juksa/pilk", "Krepsetrål", "Oppdrett", "Reketrål (herunder sputniktrål)", "Ruser", "Settegarn", "Skjellskrape", "Snurpenot med lys", "Snurpenot/ringnot", "Snurrevad", "Taretrål", "Teiner", "Udefinert garn", "Udefinert not")
  Gear2<-c(3, 4, 3, 1, 1, 1, 4, 2, 3, 1, 4, 4, 1, 4, 1, 4, 2, 4, 4, 4, 4, 1, 4, 2, 4)
  Gear<-data.frame(Gear1,Gear2)
names(Gear)<-c("SaleNotesGear","ECAGear")

  return(Gear)
}
