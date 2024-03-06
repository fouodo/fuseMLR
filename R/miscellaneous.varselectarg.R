#' Constructor of argument object to be used by the variable selection at each layer.
#'
#' @param object Arguments as \code{list}.
#' @param layer Layer label.
#'
#' @return Returns an object of class \code{varselectarg}.
#' @export
#'
#' @examples
#' my_varselectarg <- varselectarg(object = list(type = "probability",
#'                                         mtry.prop = 0.4),
#'                           layer = "methylation")
varselectarg <- function(object, layer){
  if(!missing(layer)&!(missing(object))){
    object <- list(object)
    names(object) <- layer
  } else {
    object <- list()
  }
  class(object) <- c("varselectarg", "list")
  return(object)
}

#' This function will add a new list of argument to the current object given as parameter.
#'
#' @param object The current lrnarg object.
#' @param layer New layer name to be added.
#' @param param New parameter list.
#' @param ... More arguments to be passed to further function.
#'
#' @return Object from class \code{varselectarg}.
#' @export
#'
#' @examples
#' test_varselectarg <- varselectarg(object = list(probability = TRUE),
#'                          layer = "methylation")
#' test_varselectarg <- add(object = test_varselectarg,
#'                   layer = "genexpr",
#'                   param = list(probability = TRUE))
add.varselectarg <- function(object, layer, param, ...){
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
      names_obj <- names(object)
      object <- c(object, list(param))
      names(object) <- c(names_obj, layer)
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
#' test_varselectarg <- varselectarg(object = list(probability = TRUE),
#'                           layer = "methylation")
#' test_varselectarg <- add(object = test_varselectarg,
#'                    layer = "genexpr",
#'                    list(probability = TRUE))
#' test_varselectarg <- remove(object = test_varselectarg,
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
