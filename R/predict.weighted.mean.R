#' Layer learners will be gathered together as one learner.
#'
#' @param object Object of class weighted.mean
#' @param data A list containing data predicted values.
#' @param ... To be passed to further function.
#'
#' @return A predictor of class \code{predict.weighted.mean}.
#' @export
predict.weighted.mean <- function(object, data, ...){
  weight <- object$final_model
  ## weight layer predictions
  weighted_data <- lapply(1:length(data), function(i, tmp_weight){
    data[[i]] <- as.data.frame(data[[i]] * tmp_weight[i])
    data[[i]]$index <- 1:nrow(data[[i]])
    data[[i]]
  }, tmp_weight = weight)
  pred_df <- as.data.frame(do.call("rbind", weighted_data))
  index <- pred_df$index
  pred_df$index <- NULL
  mean_pred_df <- aggregate(x = pred_df, by = list(index), FUN = mean)
  final_prediction <- mean_pred_df[ , -1]
  final_prediction <- list(predictions = final_prediction)
  class(final_prediction) <- c("predict.weighted.mean", class(final_prediction))
  return(final_prediction)
}
