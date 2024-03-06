#' Meta learner using weighted mean. This function assigns weights to models.
#'
#' @param multilrn Model entities.
#' @param weight Weights of Model entities.
#'
#' @return A list containing a final learner, a final model and layer models.
#' @export
#'
weighted_mean <- function(multilrn,
                          weight){
  ## Built layer entity from multi learners
  data <- lapply(X = multilrn, FUN = function(lrn){
    return(lrn$data)
  })
  data_names <- names(data)
  names(data) <- data_names
  target <- multilrn[[1]]$target

  if(length(multilrn) != length(weight)){
    message("No so many weight as layer. Same weight will be used.")
    weight <- rep(1, length(multilrn))
  }
  layer_names <- lapply(1:length(multilrn), function(i){
    multilrn[[i]]$layer
  })
  names(weight) <- unlist(layer_names)
  ## Set a class to oriented the print function to final model
  class(weight) <- "weight"
  final_object <- list(data = data,
                       final_learner = "weighted.mean",
                       final_model = weight,
                       layer_model = multilrn)
  ## Set a class for predicting based on the final_object
  class(final_object) <- "weighted.mean"
  return(final_object)
}
