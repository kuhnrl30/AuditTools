#' Create a Statistical sampling object
#'
#' This function creates a statistical samplingobject. 
#' The output will supply sample selections for detailed 
#' testing. Once testing is complete, the testin 
#' conclusions can be passed to the predict function to 
#' return the results of the audit test.
#
#' @param x Dataframe representing the sample population
#' @param method Sample selection method.  MUS for Monetary
#' Unit Sampling and PUS for Physical Object Sampling.
#' @param conf Confidence level. Numeric value between 0 an 1 
#' which will impact the range of the confidence interval.  
#' Higher confidence levels result in greater confidence interval
#' range. 
#' @param target Target of the sampling test. Can be a character 
#' value matching the column name or the column index value. For 
#' MUS, this is the column with the numerical data. As an 
#' example, the transaction dollar amount. If not defined, the
#' function Will default to the last column of the dataframe.
#' @export

StatSample <- function(x, method="MUS", conf=.95,target) {
  x
  
}
