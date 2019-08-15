##' Converts year, week from numeric to character string "yyyyww"
##' @title Converts date from numeric to text string
##' @param year Integer
##' @param week Integer
##' @param sep.sign Character string that separates
##'        yyyy and ww in return value
##' @return A character string of format "yyyymmdd"
##' @author Magne Aldrin
##' @export
yw.num2char<-function(year,week,sep.sign="") {

id <- week<10
week1 <- week
week1[id] <- paste(0,week1,sep="")[id]
yw <- paste(year,week1,sep=sep.sign)

yw
}
