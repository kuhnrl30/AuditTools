#' Create a Benford distribution
#'
#' reurns a dataframe of theoretical and actual distributions
#'
#' @param dat vector of
#' @return a matrix with the theoretical and actual distribution
#'
#' @export
Benford<- function(dat){
  ta <-table(substr(as.character(100*dat),1,1))
  sta<-sum(ta)
  pb <- sapply(1:9, function(x) log10(1+1/x))
  m<-cbind(ta/sta,pb)
  colnames(m)<- c("Observed Prop.", "Theoretical Prop.")
  return(m)
  }

