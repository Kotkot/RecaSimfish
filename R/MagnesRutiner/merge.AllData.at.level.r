merge.AllData.at.level<-function(AllData.historical,AllData.future) {
  last.date.in.historical<-tail(AllData.historical$FullData$divinfo$time.names,1)
  first.date.in.future<-AllData.future$FullData$divinfo$time.names[1]
  if (last.date.in.historical!=first.date.in.future) {
    print("Incorrect time match - last.date.in.historical!=first.date.in.future, returns NULL!")
    return(NULL)
  }
  AllData<-list()
  AllData$FullData<-merge.FullData(AllData.historical$FullData,AllData.future$FullData)
  AllData$pred.summary<-merge.summary(AllData.historical$pred.summary,AllData.future$pred.summary)
  AllData$count.summary<-merge.summary(AllData.historical$count.summary,AllData.future$count.summary)
  AllData$cleaner.fish.summary<-merge.summary(AllData.historical$cleaner.fish.summary,AllData.future$cleaner.fish.summary)
##  AllData$CleanerFishMean<-NULL
##  AllData$CleanerFishMean<-merge.vec(AllData.historical$CleanerFishMean,AllData.future$CleanerFishMean)

  AllData
}
