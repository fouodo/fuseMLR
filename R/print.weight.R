#' Print weith for weighted final learner.
#'
#' @param x Object of class 'weoght'.
#' @param ... [\code{any}]\cr
#'
#' @export
print.weight <- function(x, ...){
  weight_names <- names(x)
  class(x) <- "numeric"
  names(x) <- weight_names
  cat("Weights:\n")
  print(x, ...)
  cat("\n")
}
