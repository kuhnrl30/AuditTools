#' Show the foot print for a process flow
#'
#' Converts a paired list of relations into a tabular foot print
#'
#' @param x list of classed pairs from the ClassPairs function
#' @return table
#' @export

ShowGrid<- function(x){
  names(x)<-c(" ","V2","V3")
  tidyr::spread(x, V2, V3, fill="L")
}
