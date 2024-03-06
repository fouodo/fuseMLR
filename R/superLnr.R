#' Entity learners will be gathered together as one learner.
#'
#' @param data A list containing data entities.
#' @param target The target variables.
#' @param learner The name of the to be called.
#' @param learner_args Arguments to be passed to the learner function.
#' @param var_selec The function name to be called for variable selection. Do not set this argument to avoid variable selection step.
#' @param aggregation The aggregation function to built the final learner.
#' @param aggregation_args The arguments of the aggregation function.
#' @param var_selec_arg Arguments to be passed to the variable selection function.
#'
#' @return A learner of class \code{super.lnr} containing:
#' \tabular{ll}{
#'    \code{layer_models} \tab The fitted model at each layer,\cr
#'    \code{aggregation_args} \tab The arguments of the aggregated function and\cr
#'    \code{aggregated_model} \tab The aggregated model.
#'  }
#' @export
#'
#' @examples
#' data(entities)
#' data(disease)
#' ## Train layer and super learner
#'my_super_lnr <- super_lnr(data = entities,
#'                          target = disease,
#'                          learner = "ranger",
#'                          learner_args = list(methylation = list(mtry = 10,
#'                                                                 probability = TRUE),
#'                                              genexpr = list(mtry = 10,
#'                                                             probability = TRUE),
#'                                              proteinexpr = list(mtry = 10,
#'                                                                 probability = TRUE)),
#'                          var_selec = "Boruta_ext",
#'                          list(methylation = list(type = "probability"),
#'                               genexpr = list(type = "probability"),
#'                               proteinexpr = list(type = "probability")),
#'                          aggregation = ranger.cved,
#'                          aggregation_args = list(validation = "createFolds",
#'                                                  validation_args = list(),
#'                                                  final_learner = "ranger",
#'                                                  final_learner_args = list(mtry = 1,
#'                                                                            probability = TRUE)))
#' ## Predict using the super learner
#' data(test_entities)
#' data(test_disease)
#' my_super_predictions <- predict(object = my_super_lnr, data = test_entities)
#' print(my_super_predictions)
#' ## Brier score
#' print(mean((my_super_predictions$predictions[ , 1] - (test_disease == 2))^2))
#'
super_lnr <- function(data,
                      target,
                      learner = "ranger",
                      learner_args = list(),
                      var_selec = "Boruta",
                      var_selec_arg = list(),
                      aggregation = "ranger.cved",
                      aggregation_args = list()){
  ## Step 1: Generate multilayer models
  multilrn <- if(!missing(var_selec)) {
    message("Layer models with variable selection.\n")
    multiLearner(data = data,
                 target = target,
                 learner = learner,
                 learner_args = learner_args,
                 var_selec = var_selec,
                 var_selec_arg = var_selec_arg)
  } else {
    message("Layer models without variable selection.\n")
    multiLearner(data = data,
                 target = target,
                 learner = learner,
                 learner_args = learner_args)
  }
  data <- lapply(X = multilrn, FUN = function(lrn){
    return(lrn$data)
  })
  ## Set arguments of final learner to default arguments if not provided.
  aggregation_args <- if(!length(aggregation_args)){
    formals(aggregation)
  } else {
    aggregation_args
  }
  aggregation_args <- as.list(aggregation_args)
  aggregation_args$multilrn <- multilrn
  final_model <- do.call(eval(aggregation), aggregation_args)
  ## Step 4: Prepare and return the final object of class gather
  final_object <- list(layer_models = multilrn,
                       aggregation_args = aggregation_args,
                       aggregated_model = final_model)
  class(final_object) <- "super.lnr"
  return(final_object)
}
