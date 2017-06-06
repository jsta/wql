#' monthNum
#' @description Converts dates to the corresponding numeric month.
#' @param y date
#' @export
monthNum <- 
function(y) {
  match(months(y, abbreviate = TRUE), month.abb)
}