#' Generic function to add element to the object passed as argument.
#'
#' @param object The given object from class \code{entity}, \code{lnrarg} or \code{varselectarg}.
#' @param ... More arguments to be passed to further function.
#'
#' @export
add <- function(object, ...) {
  UseMethod("add")
}

#' Default version of the generic function \code{add}.
#' @param object Current object.
#' @param ... More arguments to be passed to further function.
#'
#' @export
#'
add.default <- function(object, ...) {
  # Default behavior for my_function
  return(object)
}


#' Generic function to remove element from the object passed as argument.
#'
#' @param object The given object from class \code{entity}, \code{lnrarg} or \code{varselectarg}.
#' @param ... More arguments to be passed to further function.
#'
#' @export
remove <- function(object, ...) {
  UseMethod("remove")
}

#' Default version of the generic function \code{remove}.
#' @param object Current object.
#' @param ... More arguments to be passed to further function.
#'
#' @export
#'
remove.default <- function(object, ...) {
  return(object)
}
