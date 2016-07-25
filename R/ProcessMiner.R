#' Creates a petri net based to visualize a process
#'
#'
#' @param dat dataframe with two columns. The first column is
#' the key and second is the event
ProcessMiner<- function(dat){

  Events<- unique(unlist(unique(dat)))
  Events
  }



