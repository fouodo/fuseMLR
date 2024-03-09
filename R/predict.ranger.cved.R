#' Entity learners will be gathered together as one learner.
#'
#' @param object Object of class ranger.cved.
#' @param data A list containing data entities.
#' @param ... To be passed to further function.
#'
#' @return A predictor of class \code{predict.ranger.cved}.
#' @export
predict.ranger.cved <- function(object, data, ...){
  object <- object$final_model
  pred_df <- as.data.frame(do.call("cbind", data))
  colnames(pred_df) <- object$forest$independent.variable.names
  class(object) <- c("ranger", class(object))
  final_prediction <- predict(object = object, data = pred_df)
  class(final_prediction) <- "predict.ranger.cved"
  return(final_prediction)
}
