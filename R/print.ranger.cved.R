#' Print object of class 'ranger.cved'.
#'
#' @param x Object of class 'ranger.cved'.
#' @param ... [\code{any}]\cr
#'
#' @export
print.ranger.cved <- function(x, ...){
  object_final_model <- x$final_model
  print(object_final_model, ...)
}
