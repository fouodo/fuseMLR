#' Print object of class 'multiVarSelect'.
#'
#' @param x Object of class 'multiVarSelect'.
#' @param ... [\code{any}]\cr
#'
#' @export
print.multiVarSelect <- function(x, ...) {
  for (i in 1:length(x)) {
    selected_res <- x[[i]]
    if (class(selected_res[["selected_var"]]) %in% c("Boruta", "Boruta.ext")) {
      cat(sprintf("Layer               :%s\n\n", selected_res[["layer"]]))
      print(selected_res[["selected_var"]], ...)
      cat("\n")
    } else {
      print(utils::head(x = selected_res))
    }
  }
}
