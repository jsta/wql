
#' meanSub
#'
#' @param x numeric vector
#' @param sub integer index
#' @param na.rm logical
#'
#' @export
meanSub <- 
function(x, sub, na.rm = FALSE) {
  mean(x[sub], na.rm = na.rm)
}
