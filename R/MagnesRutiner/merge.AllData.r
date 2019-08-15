##' Merging two AllData objects historical and future data periods
##'
##' @title Merging two AllData objects
##' @inheritParams dummy.for.documentation
##' @return An AllData objects with elements AllData.merd.level and AllData.farm.level
##' @author Magne Aldrin
##' @export merge.AllData
merge.AllData<-function(AllData.historical,AllData.future) {
  
  last.date.in.historical<-tail(AllData.historical$AllData.merd.level$FullData$divinfo$time.names,1)
  first.date.in.future<-AllData.future$AllData.merd.level$FullData$divinfo$time.names[1]
  if (last.date.in.historical!=first.date.in.future) {
    print("Incorrect time match - last.date.in.historical!=first.date.in.future, returns NULL!")
    return(NULL)
  }
  AllData<-list()
  
  AllData$AllData.merd.level<-merge.AllData.at.level(AllData.historical$AllData.merd.level,AllData.future$AllData.merd.level)
  AllData$AllData.farm.level<-merge.AllData.at.level(AllData.historical$AllData.farm.level,AllData.future$AllData.farm.level)
  AllData$last.day.in.historical<-last.date.in.historical
  AllData
}
