#' Entity learners will be gathered together as one learner.
#'
#' @param object Object of class ranger.cved.
#' @param data A list containing data entities.
#' @param ... To be passed to further function.
#'
#' @return A predictor of class 'predict.ranger.cved'.
#' @export
#'
predict.meta.lnr <- function(object, data, ...){
  data_filtered <- lapply(1:length(data), function(i){
    ## ToDo: Strongly based on the Boruta variable selection output.
    var_select <- object$layer_models[[i]]$var_selected$selected_var$info$selected
    var_select <- var_select == 1
    return(data[[i]][ , var_select])
  })
  names(data_filtered) <- names(data)
  # data <- data_filtered
  pred_layer <- predict(object = object$layer_models,
                  data = data_filtered)
  pred_list <- lapply(1:length(pred_layer), function(i){
    pred_layer[[i]]$predictions$predictions
  })
  aggregated_model <- object$aggregated_model
  pred <- predict(object = aggregated_model,
                  data = pred_list)
  pred <- list(entity_pred = pred_list, meta_pred = pred)
  class(pred) <- c("predict.meta.lnr", class(pred))
  return(pred)
}
