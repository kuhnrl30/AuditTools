#' Create pairs for factors that interact in the traces
#'
#' @param traces list of unique traces
#' @export

MakePairs<- function(traces){

  L<- matrix(nrow=0,ncol=2)

  for (j in 1:length(traces)){

    M<-matrix(nrow=length(traces[[j]])-1,ncol=2)

    for(i in 1:length(traces[[j]])-1){
      M[i,]<- cbind(traces[[j]][i],traces[[j]][i+1])
      }
      L<-rbind(L,M)
    }
  return(as.data.frame(L))
  }
