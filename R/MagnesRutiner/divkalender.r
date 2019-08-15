

gen.yeardayahead <- function(endyear,endmonth,endday,days){
  # Generates a date which is days after endyear,endmonth,enddays
  dates.all <- NULL
  for(i in 1:length(endyear)) {
    dates00 <- gen.yearday(endyear[i],12,31)
    id <- (1:length(dates00[,1]))[(dates00[,2]==endmonth[i]) & (dates00[,3]==endday[i])]
    id1 <- id:length(dates00[,1])
    dates0 <- dates00[id1,]
    dates1 <- gen.yearday(endyear[i]+1,12,31)
    dates.all <- rbind(dates.all,rbind(dates0,dates1)[1+days,])
  }
  dates.all
}
gen.yearday <- function(endyear,endmonth,endday){
  # Generates dates from first date in year to end date 
  nodaysinmonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  month <- c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),
rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
  day <- c(1:31,1:28,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31)
  if (leapyear(endyear)) {
    nodaysinmonth[2] <- 29
    day <- c(day[1:59],29,day[60:365])
    month <- c(month[1:59],2,month[60:365])
  }
  cumdays <- c(0,cumsum(nodaysinmonth))
  nn <- cumdays[endmonth]+endday
  year <- rep(endyear,nn)
  month <- month[1:nn]
  day <- day[1:nn]
  calendar <- cbind(year,month,day)
  calendar
}

leapyear <- function(year){
# Returns vector of TRUE if year is leap year. Otherwise, return FALSE.
  leap <- rep(FALSE,length(year))
  id.year <- year%%100==0
  year[id.year] <- year[id.year]/100
  id.leap <- year%%4==0
  leap[id.leap] <- TRUE
  leap
}
myyeardays <- function(startyear,startmonth,startday,endyear,endmonth,endday){
# For all days in the period from start date through end date,
# determine the day number in year.
  startdayno <- yearday(startyear,startmonth,startday)
  enddayno <- yearday(endyear,endmonth,endday)
  if (startyear==endyear){
    ydvec <- seq(startdayno,enddayno,1)
  } else {
    #ydvec <- seq(startdayno,nodaysinyear(startyear),1)
    ydvec <- seq(startdayno,yearday(startyear,12,31),1)
    if (startyear < endyear-1){
      y1 <- startyear+1
      y2 <- endyear-1
      for (year in y1:y2){
        #ydvec <- c(ydvec,seq(1,nodaysinyear(year),1))
        ydvec <- c(ydvec,seq(1,yearday(year,12,31),1))
      }
    }
    ydvec <- c(ydvec,seq(1,enddayno,1))
  }
  ydvec
}
yearday <- function(year,month,day){
# Day number of day/month in year.
  nodaysinmonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  if (leapyear(year)) nodaysinmonth[2] <- 29
  cumdays <- c(0,cumsum(nodaysinmonth))
  cumdays[month]+day
}
##' Generates a calendar
##' @title Generates a calendar
##' @param startyear Integer
##' @param startmonth Integer
##' @param startday Integer
##' @param endyear Integer
##' @param endmonth Integer 
##' @param endday Integer
##' @return A matrix with columns year, month, day, weekday, weekyear
##'         and year.with.week.number. Here, weekyear is the week number within a year,
##'         whereas year.with.week.number is the year where the weeks belong,
##'         and may differ from year for some days in weeks 1, 52 and 53.
##' @author Magne Aldrin
##' @export
GenKalender<-function(startyear,startmonth,startday,
                        endyear,endmonth,endday){
### Returns a matrix 
### The rows in the matrix starts from startyear startmonth startday
### and ends at endyear endmonth endday and each row represents a day
  calendar <- NULL
  weekday <- NULL
  weekyear <- NULL
  if(startyear == endyear) {
    calendar <- gen.yearday(endyear,12,31)
    z <- whichweekandday(startyear)
    weekday <- c(weekday, z$weekdays)
    weekyear <- c(weekyear, z$weekyears)
  }
  else {
    calendar <- gen.yearday(startyear,12,31)
    z <- whichweekandday(startyear)
    weekday <- c(weekday, z$weekdays)
    weekyear <- c(weekyear, z$weekyears)
    for(year in (startyear + 1):endyear) {
      z <- whichweekandday(year)
      weekday <- c(weekday, z$weekdays)
      weekyear <- c(weekyear, z$weekyears)
      calendar <- rbind(calendar, gen.yearday(year,12,31))
      
    }
  }
  number.start <- 0
  number.end <- 0
  number.start <- yearday(startyear, startmonth, startday)
  if(startyear <  endyear) {
    for(year in (startyear):(endyear - 1)) {
      number.end <- number.end + yearday(year, 12, 31)
    }
  }
  number.end <- number.end + yearday(endyear, endmonth, endday)
  z <- cbind(calendar, weekday, weekyear)
  year.with.week.number<-z[,"year"]
  year.with.week.number[z[,"weekyear"]==1 & z[,"month"]==12]<-year.with.week.number[z[,"weekyear"]==1 & z[,"month"]==12]+1
  year.with.week.number[z[,"weekyear"]==52 & z[,"month"]==1]<-year.with.week.number[z[,"weekyear"]==52 & z[,"month"]==1]-1
  year.with.week.number[z[,"weekyear"]==53 & z[,"month"]==1]<-year.with.week.number[z[,"weekyear"]==53 & z[,"month"]==1]-1
  z<-cbind(z,year.with.week.number)
  
  z <- z[number.start:number.end,  ]
  
  z
}
whichweekandday <- function(year1) {
# For a given year and weekdays in this year
# determine the week number in the year and the day number each week
  calendar1 <- gen.yearday(year1-1,12,31)
  calendar2 <- gen.yearday(year1,12,31)
  calendar <- rbind(calendar1,calendar2)
  weekday <-rep(0,length(calendar[,1]))
  for(i in 1:length(calendar[,1])) {
    weekday[i] <- whichweekday(calendar[i,1],calendar[i,2],calendar[i,3])
  }
  weekyear<-rep(0,length(weekday))
  weekyear[weekday == 1] <- 1
  weekdaylast<-rep(0,length(weekday))
  weekdaylast1 <- weekday[calendar[,1]==year1-1 & calendar[,2]==12 & calendar[,3]==31
]
  if(weekdaylast1 <4) {
    for(i in 1:weekdaylast1) {
      if(weekdaylast1>0) {
        weekdaylast[calendar[,1]==year1-1 & calendar[,2]==12 & calendar[,3]==32-i ] <
- 1
      }
    }
  }


  weekyear[calendar[,1]==year1-1 & calendar[,2]==1 & calendar[,3]==1 & (weekday == 2
| weekday == 3 | weekday == 4)] <- 1
  dummy <- cumsum( weekyear[(calendar[,1]==year1-1) ])
  weekyear[calendar[,1]==year1-1] <- dummy
  weekyear[calendar[,1]==year1-1 & weekdaylast==1] <- 1

  weekdaylast<-rep(0,length(weekday))
  weekdaylast1 <- weekday[calendar[,1]==year1 & calendar[,2]==12 & calendar[,3]==31]
  if(weekdaylast1 <4) {
    for(i in 1:weekdaylast1) {
      if(weekdaylast1>0) {
        weekdaylast[calendar[,1]==year1 & calendar[,2]==12 & calendar[,3]==32-i ] <-
1
      }
    }
  }


  weekyear[calendar[,1]==year1 & calendar[,2]==1 & calendar[,3]==1 & (weekday == 2 |
weekday == 3 | weekday == 4)] <- 1
  dummy <- cumsum( weekyear[(calendar[,1]==year1) ])
  weekyear[calendar[,1]==year1] <- dummy
  weekyear[calendar[,1]==year1 & weekdaylast==1] <- 1
  lastweek <- weekyear[calendar[,1]==year1-1]
  weekyear[weekyear==0] <- lastweek[length(lastweek)]
  res <- list(weekdays=weekday[calendar[,1]==year1],weekyears=weekyear[calendar[,1]==
year1])
  res

}
whichweekday <- function(year,month,day){
# For a given date specified by year, month, and day in month,
# determine the weekday.
# Monday=1, Tuesday=2, Wednesday=3, Thursday=4, Friday=5,
# Saturday=6, and Sunday=7
  fvec <- c(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
  gvec <- c(7, seq(1,6,1))
  x <- year
  if (month <= 2) x <- x-1
  y <- x%/%4-x%/%100+x%/%400
  z <- day+fvec[month]+x+y
  gvec[z%%7+1]
}
