#' Print object of class \code{meta.lnr}.
#'
#' @param x Object of class \code{meta.lnr}.
#' @param ... [\code{any}]\cr
#'
#' @export
print.meta.lnr <- function(x, ...){
  cat("Final model                      \n")
  cat("---------------------------------\n")
  object_final_model <- x$aggregated_model$final_model
  print(object_final_model)
  cat("                                 \n")
  cat("Layer models                     \n")
  cat("---------------------------------\n")
  print(x$layer_models, ...)
}
