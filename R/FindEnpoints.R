#' @export
#'
FindEndpoints<- function(x){

  Start<-unique(as.character(x[which(!x[,1] %in% x[which(x[,3]=="L"),1]),1]))
  End <- unique(as.character(x[which(!x[,1] %in% x[which(x[,3]=="R"),1]),1]))


  return(list(Start=Start,End=End))
}
