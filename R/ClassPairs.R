#' Classifies the pairs to create a footprint
#'
#' Classifies each of the pairs as required to draw the Petri net
#' and the footprint. Pairs are identified by rows in the dataframe,
#' The first column is the preceding point and the second column is
#' the following point. There are 4 values the pair can take:
#' \enumerate{
#' \item R or Right.  The first value always precedes the second value.
#' It is called Right because the process flows towards the right.
#' \item L or Left. The first value always follows the second value. Similarly,
#' the process flows to the left.
#' \item # or unrelated. These two values never directly interact. One never directly follows the other.
#' \item || or parallel.  These two values are do not have an order between them and can happen in either order e.g. AB or BA
#' }
#'
#' @param L dataframe from the MakePairs function
#' @param traces list of traces
#' @export
ClassPairs<- function(L,traces){
  #L is a data.frame of interactions from MakePairs
  #K is a vector of unique events
  K<- unique(unlist(traces))
  Pars<- matrix(ncol=2,nrow=0)
  for (i in 1:length(K)){
    for (k in 1:length(K)){
      if (nrow(subset(L, V1==K[i] & V2==K[k]))>0) {
        if (nrow(subset(L, V1==K[k] & V2==K[i]))>0) {
          Pars<-rbind(Pars,cbind(K[i],K[k]))
        }
      }
    }
  }
  Pars<-cbind(Pars,"||")

  # Identify causalities
  CausR<- matrix(ncol=2,nrow=0)
  for (i in 1:length(K)){
    for (k in 1:length(K)){
      if (nrow(subset(L, V1==K[i] & V2==K[k])) > 0) {
        if (nrow(subset(L, V1==K[k] & V2==K[i])) == 0) {
          CausR<-rbind(CausR,cbind(K[i],K[k]))
        }
      }
    }
  }
  CausR<-cbind(CausR,"R")

  CausL<- matrix(ncol=2,nrow=0)
  for (i in 1:length(K)){
    for (k in 1:length(K)){
      if (nrow(subset(L, V1==K[i] & V2==K[k]))== 0) {
        if (nrow(subset(L, V1==K[k] & V2==K[i])) > 0) {
          CausL<-rbind(CausL,cbind(K[i],K[k]))
        }
      }
    }
  }
  CausL<-cbind(CausL,"L")

  # Identify choices
  Choic<- matrix(ncol=2,nrow=0)
  for (i in 1:length(K)){
    for (k in 1:length(K)){
      if (nrow(subset(L, V1==K[i] & V2==K[k])) ==0) {
        if (nrow(subset(L, V1==K[k] & V2==K[i])) == 0) {
          Choic<-rbind(Choic,cbind(K[i],K[k]))
        }
      }
    }
  }
  Choic<-cbind(Choic,"#")

  P<- as.data.frame(rbind(Pars,CausR,CausL,Choic), stringsAsFactors = F)
  P$V3<- factor(P$V3,levels=c("#","R","L","||"))
  return(P)
}
