#' monthNum
#' @description Converts dates to the corresponding numeric month.
#' @param y date
#' @author 
#' Alan Jassby, James Cloern
#' @export
monthNum <- 
function(y) {
  match(months(y, abbreviate = TRUE), month.abb)
}
