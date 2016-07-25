#' Convert a dataframe to a list of traces
#'
#' @param dat 2 column dataframe.  The first colum is the key and the second is the event. The data should be sorted by in chronilogical order by event.
#' @return list of traces
#' @export

DataPrep<- function(dat){
  dat[,2]<- as.factor(dat[,2])
  Events<- sapply(as.numeric(dat[,2]),EventsToAlpha)
  split(Events, dat[,1])
  }
