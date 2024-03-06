#' Predict on each layer. The pre-defined predict function of the layer learner should return a list containing a 'predictions' elements. The 'predictions' element contains predicted values.
#'
#' @param object Object of class 'multiLearner'
#' @param data New dataset to be used for predictions.
#' @param ... Further arguments to be passed to the pre-defined predict function.
#'
#' @return Object of class multiLearner.predict.
#' @export
#'
predict.multiLearner <- function(object, data, ...) {
  len_object <- length(object)
  predict_list <- vector(mode = "list", length = len_object)
  for (i in 1:len_object) {
    model <- object[[i]]$model
    if (inherits(model, "ranger.layer")) {
      class(model) <- "ranger"
      predict_list[[i]][["predictions"]] <- predict(model,
                                                    data = data[[i]], ...)
      predict_list[[i]][["layer"]] <- object[[i]]$layer
    } else {
      cat("Unknow object. I cannot predict.")
    }
  }
  class(predict_list) <- "multiLearner.predict"
  return(predict_list)
}
