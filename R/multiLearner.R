#' A learner is built for each layer and then aggregated as a super learner
#'
#' @import ranger
#' @import Boruta
#' @import data.table
#'
#' @param data A list containing data entities.
#' @param target The target variables.
#' @param learner The name of the learner to be called.
#' @param learner_args The arguments to be passed to learner.
#' @param var_selec The name of the variable selection method.
#' @param var_selec_arg The arguments to be passed to the variable selection method.
#'
#' @return Object of class \code{multiLearner} containing list elements:
#' \tabular{ll}{
#'    \code{layer} \tab The name of the layer, \cr
#'    \code{data} \tab The layer entity,\cr
#'    \code{target} \tab target variable, \cr
#'    \code{learner} \tab Name of the learner, e.g. \code{ranger},\cr
#'    \code{learner_args} \tab Arguments of the learner,\cr
#'    \code{var_selected} \tab Boolean vector with 1 for selected variable and 0 for non selected variable,\cr
#'    \code{model} \tab The fitted model.
#'  }
#' @export
#' @include miscellaneous.entitty.R
#' @examples
#' ## In this example, we prepare three entities, arguments of
#' ## ranger leaner and arguments of Boruta variable selection algorithm at each
#' ## layer.
#' ## Prepare entities.
#' entity_obj <- entity(object = entities$methylation,
#'                         layer = "methylation")
#' entity_obj <- add(object = entity_obj,
#'                      layer = "genexpr",
#'                       data = entities$genexpr)
#' entity_obj <- add(object = entity_obj,
#'                      layer = "proteinexpr",
#'                       data = entities$proteinexpr)
#' ## Prepare learner arguments.
#' lrnarg_obj <- lrnarg(object = list(probability = TRUE),
#'                       layer = "methylation")
#' lrnarg_obj <- add(object = lrnarg_obj,
#'                    layer = "genexpr",
#'                    param = list(probability = TRUE))
#' lrnarg_obj <- add(object = lrnarg_obj,
#'                    layer = "proteinexpr",
#'                    param = list(probability = TRUE))
#' ## Prepare variable selection arguments.
#' varselectarg_obj <- varselectarg(object = list(type = "probability",
#'                                                 mtry.prop = 0.4),
#'                                   layer = "methylation")
#' varselectarg_obj <- add(object = varselectarg_obj,
#'                          layer = "genexpr",
#'                          param = list(type = "probability", mtry.prop = 0.5))
#' varselectarg_obj <- add(object = varselectarg_obj,
#'                          layer = "proteinexpr",
#'                          param = list(type = "probability", mtry.prop = 0.3))
#' ## Train machine learning models.
#' set.seed(321)
#' my_multiLearner <- multiLearner(data = entity_obj,
#'                                 target = disease,
#'                                 learner = "ranger",
#'                                 learner_args = lrnarg_obj,
#'                                 var_selec = "Boruta_ext",
#'                                 var_selec_arg = varselectarg_obj)
#' ## Predict using the meta learner.
#' data(test_entities)
#' predict(object = my_multiLearner, data = test_entities)
multiLearner <- function(data,
                         target,
                         learner = "ranger",
                         learner_args = list(),
                         var_selec = "Boruta",
                         var_selec_arg = list()) {
  ## Step 1: Variable selection.
  if(!missing(var_selec)) {
    ## Variable selection required.
    message("Variable selection.\n")
    var_select_res <- multiVarSelect(data = data,
                                     target = target,
                                     var_selec = var_selec,
                                     var_selec_arg = var_selec_arg)
    data_names <- names(data)
    ## Reduce data dimension after variable selection.
    data <- lapply(1:length(data), function(i,
                                            my_var_select_res,
                                            filtered_data){
      var_select_res_layer <- my_var_select_res[[i]][["selected_var"]]

      selected <- if(inherits(var_select_res_layer, "Boruta")) {
        as.integer(var_select_res_layer$finalDecision == "Confirmed")
      } else {
        if(var_selec == "Boruta_ext") {
          var_select_res_layer$info$selected
        } else {
          ## Variable selection methods implemented by the user muss contain a vector "selected".
          if("selected" %in% names(var_select_res)) {
            var_select_res$selected
          } else {
            stop('Your variable selection object muss contain a "selected" vector.')
          }
        }
      }
      entity <- filtered_data[[i]][ , selected]
      return(entity)
    }, my_var_select_res = var_select_res, filtered_data = data)
    names(data) <- data_names
    ## Check whether there are empty layers and remove it.
    empty_layers <- lapply(data, function(entity) {
      ncol(entity) != 0
    })
    data <- data[unlist(empty_layers)]
    ## Also prune the argument list.
    learner_args <- learner_args[unlist(empty_layers)]
  } else {
    ## No variable selection required.
    var_select_res <- lapply(1:length(data), function(i){
      rep(1, ncol(data[[i]]))
    })
    data <- data
  }
  if(length(data) == 0){
    stop("No remaining entity after variable selection because of no selected variable.")
  }
  message(sprintf("Integrative %s.\n", learner))
  ## Step 1: Build integrative random forest models
  ## Parameter type check
  checkmate::assert_list(x = data, types = c("data.frame", "matrix"), min.len = 1)
  checkmate::assert_vector(x = target, any.missing = FALSE)
  checkmate::assert_string(x = learner)
  checkmate::assert_true(x = learner %in% c("ranger"))
  ## Consistency between data and target dimension
  checkmate::assert_matrix(x = data[[1]], min.rows = 2, min.cols = 1)
  checkmate::assert_vector(x = target)
  target_checked <- lapply(X = data, FUN = function(entity) {
    checkmate::assert_true(length(target) == nrow(entity))
  })

  ## Consistency between learner's arguments and ranger's arguments
  len_learner_args <- if (length(learner_args) == 0) {
    length(data)
  } else {
    length(learner_args)
  }
  checkmate::assert_true(length(data) == len_learner_args)
  ## Set arguments to default learner arguments in case of an empty list.
  learner_args <- if (length(learner_args) == 0) {
    lapply(1:length(data), function(i) {
      return(formals(learner))
    })
  } else {
    learner_args
  }
  ## Check that provided argument names match with those of the learner.
  args_name_checked <- lapply(X = learner_args,
                              FUN = function (entity_arg) {
                                checkmate::assert_true(
                                  x = all(names(entity_arg) %in%
                                            names(formals(learner))))
                              })
  ## Call the provided learner on each data entity
  model_list <- lapply(X = 1:length(data), FUN = function(i, entity_list,
                                                          y, param_list) {
    param <- as.list(param_list[[i]])
    param$x <- as.matrix(entity_list[[i]])
    param$y <- y
    fitted_model <- do.call(eval(learner), param)
    class(fitted_model) <- sprintf("%s.layer", class(fitted_model))
    return(list(layer = names(data)[i],
                data = param$x,
                target = param$y,
                learner = learner,
                learner_args = learner_args[[i]],
                var_selected = var_select_res[[i]],
                model = fitted_model))
  }, entity_list = data, y = target, param_list = learner_args)
  class(model_list) <- "multiLearner"
  return(model_list)
}


