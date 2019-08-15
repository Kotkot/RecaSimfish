merge.summary<-function(summary.historical,summary.future) {

  time.names.historical<-rownames(summary.historical[[1]][[1]])
  time.names.future<-rownames(summary.future[[1]][[1]])
  last.date.in.historical<-tail(time.names.historical,1)
  first.date.in.future<-time.names.future[1]
  if (last.date.in.historical!=first.date.in.future) {
    print("Incorrect time match - last.date.in.historical!=first.date.in.future, returns NULL!")
    return(NULL)
  }

  merd.names.historical<-names(summary.historical)
  merd.names.future<-names(summary.historical)
  if (any(merd.names.historical!=merd.names.future)) {
    print("Inconsistent cage names, returns NULL!")
    return(NULL)
  }
  stage.names.historical<-names(summary.historical[[1]])
  stage.names.future<-names(summary.future[[1]])
  if (any(stage.names.historical!=stage.names.future)) {
    print("Inconsistent stage names, returns NULL!")
    return(NULL)
  }

  summary<-summary.historical
  merd.names<-merd.names.historical
  stage.names<-stage.names.historical
  for (merd in merd.names) {
    for (stage in stage.names) {
      summary[[merd]][[stage]]<-rbind(summary.historical[[merd]][[stage]],summary.future[[merd]][[stage]][-1,,drop=F])
    }
  }
  
  summary
}
