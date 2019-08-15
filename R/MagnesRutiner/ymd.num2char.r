##' Converts year, month, date from numeric to character string "yyyymmdd"
##' @title Converts date from numeric to text string
##' @param year Integer
##' @param month Integer
##' @param day Integer
##' @param sep.sign Character string that separates
##'        yyyy, mm and dd in return value
##' @return A character string of format "yyyymmdd"
##' @author Magne Aldrin
##' @export
ymd.num2char<-function(year,month,day,sep.sign="") {

id <- month<10
month1 <- month
month1[id] <- paste(0,month1,sep="")[id]
id <- day<10
day1 <- day
day1[id] <- paste(0,day1,sep="")[id]
ymd <- paste(year,month1,day1,sep=sep.sign)

ymd
}
