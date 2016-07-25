#' Converts dataframes to alphabetical character symbol
#'
#' This function converts numeric factor levels to an alpha
#' characters.  As an example, the first factor level, 1,
#' would be converted to 'A'.
#'
#' @param x numeric to be converted to alpha
#' @return alpha
#'
EventsToAlpha<- function(x){
  a<- floor(x/26)
  b<- x%%26
  paste0(LETTERS[a],LETTERS[b])
}
