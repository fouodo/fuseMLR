#' Variable selection is conducted for the provided function on each data entity.
#'
#' @param data A list containing data entities.
#' @param target The target variables.
#' @param var_selec The function name to be called for variable selection.
#' @param var_selec_arg Arguments to be passed to the variable selection function.
#'
#' @export
#' @return Object of class \code{multiVarSelect} containing following information on each layer:
#' \tabular{ll}{
#'    \code{learner} \tab Name of the learner, e.g. \code{ranger},\cr
#'    \code{var_selected} \tab Information about variable selection.\cr
#'  }
#'
#' @examples
#' \dontrun{
#' data(entities)
#' data(disease)
#' multiVarSelect(data = entities,
#'                target = disease,
#'                var_selec = "Boruta_ext",
#'                var_selec_arg = list())
#' ## With the original Boruta function
#' multiVarSelect(data = entities,
#' target = disease,
#' var_selec = "Boruta",
#' var_selec_arg = list(list(mtry = 2),
#'                      list(mtry = 2),
#'                      list(mtry = 2)))
#' }
multiVarSelect <- function(data,
                           target,
                           var_selec = "Boruta",
                           var_selec_arg = list()) {
  ## Parameter type check
  checkmate::assert_list(x = data, types = c("data.frame", "matrix"), min.len = 1)
  checkmate::assert_vector(x = target, any.missing = FALSE)
  checkmate::assert_string(x = var_selec)
  checkmate::assert_true(x = var_selec %in% c("Boruta", "Boruta_ext"))
  ## Consistency between data and target dimension
  checkmate::assert_matrix(x = data[[1]], min.rows = 2, min.cols = 1)
  checkmate::assert_vector(x = target)
  target_checked <- lapply(X = data, FUN = function(entity) {
    checkmate::assert_true(length(target) == nrow(entity))
  })

  ## Consistency between learner's arguments and learner's arguments
  len_var_selec_arg <- if (length(var_selec_arg) == 0) {
    length(data)
  } else {
    length(var_selec_arg)
  }
  checkmate::assert_true(length(data) == len_var_selec_arg)
  ## Set arguments to default learner arguments in case of an empty list.
  var_selec_arg <- if (length(var_selec_arg) == 0) {
    lapply(1:length(data), function(i) {
      return(formals(var_selec))
    })
  } else {
    var_selec_arg
  }
  ## Call variable selection function on each data entity
  selected_list <- lapply(X = 1:length(data), FUN = function(i, entity_list,
                                                             y, param_list) {
    param <- as.list(param_list[[i]])
    param$x <- as.matrix(entity_list[[i]])
    param$y <- y
    selected_var <- do.call(eval(var_selec), param)
    # class(selected_var) <- sprintf("%s.layer", class(selected_var))
    return(list(layer = names(data)[i], selected_var = selected_var))
  }, entity_list = data, y = target, param_list = var_selec_arg)
  class(selected_list) <- "multiVarSelect"
  return(selected_list)
}


