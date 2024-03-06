#' Constructor of entity object
#'
#' @param object Data entity as \code{matrix} or \code{array}.
#' @param layer Layer label.
#'
#' @return Returns an object of class \code{entity}.
#' @export
#'
#' @examples
#' data(entities)
#' test_entities <- entity(object = entities$methylation,
#'                           layer = "methylation")
entity <- function(object, layer){
  if(!missing(layer)&!(missing(object))){
    object <- list(layer = object)
    names(object) <- layer
  } else {
    object <- list()
  }
  class(object) <- c("entity", "list")
  return(object)
}

#' This function will add a new data entity to the entity object given as parameter.
#'
#' @param object The current entity object.
#' @param layer New layer name to be added.
#' @param data New data entity.
#' @param ... More arguments to be passed to further function.
#'
#' @return Object from class \code{entity}.
#' @export
#'
#' @examples
#' data(entities)
#' test_entities <- entity(object = entities$methylation,
#'                           layer = "methylation")
#' test_entities <- add(object = test_entities,
#'                    layer = "genexpr",
#'                    data = entities$genexpr)
add.entity <- function(object, layer, data, ...){
  class_obj <- class(object)
  if(missing(object) | missing(layer) | missing(data)){
    stop("Please provide layer and data.")
  } else {
    if(layer %in% names(object)){
      stop("Existing entity for the given layer. Please remove it first.")
    } else {
      object <- c(object, list(data))
      names(object)[length(object)] <- layer
    }
  }
  class(object) <- class_obj
  return(object)
}

#' This function removes data for the corresponding layer passed as argument.
#'
#' @param object The current entity object.
#' @param layer Layer refers to the name of the entity to be removed.
#' @param ... More arguments to be passed to further function.
#'
#' @return The shorter entity object.
#' @export
#'
#' @examples
#' data(entities)
#' test_entities <- entity(object = entities$methylation,
#'                           layer = "methylation")
#' test_entities <- add(object = test_entities,
#'                    layer = "genexpr",
#'                    data = entities$genexpr)
#' test_entities <- remove(object = test_entities,
#'                       layer = "genexpr")
remove.entity <- function(object, layer, ...){
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

#' Print object of class entity.
#'
#' @param x object to be printed.
#' @param ... More arguments to be passed to further function.
#'
#' @export
#'
#' @examples
#' data(entities)
#' test_entities <- entity(object = entities$methylation,
#'                           layer = "methylation")
#' test_entities <- add(object = test_entities,
#'                    layer = "genexpr",
#'                    data = entities$genexpr)
#' print(test_entities)
print.entity <- function(x,...){
  entity_summary <- lapply(x, function(e){
    res <- c(dim(e),
             sum(complete.cases(e)),
             nrow(e) - sum(complete.cases(e)))
    names(res) <- c("sample", "variable", "complete.case", "nb.NA")
    return(res)
  })
  res <- do.call("rbind", entity_summary)
  print(res, ...)
}
