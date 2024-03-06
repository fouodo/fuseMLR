#' Constructor of argument object to be used by the learner at each layer.
#'
#' @param object Arguments as \code{list}.
#' @param layer Layer label.
#'
#' @return Returns an object of class \code{lrnarg}.
#' @export
#'
#' @examples
#' test_lrnarg <- lrnarg(object = list(probability = TRUE),
#'                           layer = "methylation")
lrnarg <- function(object, layer){
  if(!missing(layer)&!(missing(object))){
    object <- list(layer = object)
    names(object) <- layer
  } else {
    object <- list()
  }
  class(object) <- c("lrnarg", "list")
  return(object)
}

#' This function will add a new list of argument to the current object given as parameter.
#'
#' @param object The current lrnarg object.
#' @param layer New layer name to be added.
#' @param param New parameter list.
#' @param ... More arguments to be passed to further function.
#'
#' @return Object from class \code{lrnarg}.
#' @export
#'
#' @examples
#' test_lrnarg <- lrnarg(object = list(probability = TRUE),
#'                           layer = "methylation")
#' test_lrnarg <- add(object = test_lrnarg,
#'                    layer = "genexpr",
#'                    list(probability = TRUE))
add.lrnarg <- function(object, layer, param, ...){
  if(!is.list(param)){
    stop("Argument 'param' must be a list.")
  }
  class_obj <- class(object)
  if(missing(object) | missing(layer) | missing(param)){
    stop("Please provide layer and data.")
  } else {
    if(layer %in% names(object)){
      stop("Existing entity for the given layer. Please remove it first.")
    } else {
      object <- c(object, param)
      names(object)[length(object)] <- layer
    }
  }
  class(object) <- class_obj
  return(object)
}

#' This function removes a parameter list for the corresponding layer passed as argument.
#'
#' @param object The current entity object.
#' @param layer Layer refers to the name of the entity to be removed.
#' @param ... More arguments to be passed to further function.
#'
#' @return The shorter list of arguments.
#' @export
#'
#' @examples
#' test_lrnarg <- lrnarg(object = list(probability = TRUE),
#'                           layer = "methylation")
#' test_lrnarg <- add(object = test_lrnarg,
#'                    layer = "genexpr",
#'                    list(probability = TRUE))
#' test_lrnarg <- remove(object = test_lrnarg,
#'                       layer = "genexpr")
remove.lrnarg <- function(object, layer, ...){
  if(missing(object) | missing(layer)){
    stop("Entity or layer can not be empty.")
  } else {
    if(!(layer) %in% names(object)){
      warning("None existing layer in the provided object.")
    } else {
      to_rm <- which(names(object) == "layer")
      object[to_rm] <- NULL
    }
  }
  return(object)
}

#' Print object of class lrnarg.
#'
#' @param x object to be printed.
#' @param ... More arguments to be passed to further function.
#'
#' @export
#'
#' @examples
#' test_lrnarg <- lrnarg(object = list(probability = TRUE),
#'                           layer = "methylation")
#' test_lrnarg <- add(object = test_lrnarg,
#'                    layer = "genexpr",
#'                    list(probability = TRUE))
#' print(test_lrnarg)
print.lrnarg <- function(x, ...){
  return(x)
}
