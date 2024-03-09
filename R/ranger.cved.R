#' Validation and ranger are used to build the final model.
#'
#' @import caret
#' @param multilrn Object containing layer models.
#' @param validation Validation function e.g 'createFolds' for cross validation.
#' @param validation_args Argumments to be passed to the validation function.
#' @param final_learner Name of the final learner.
#' @param final_learner_args Arguments to be passer to the final learner.
#'
#' @return An object of class \code{ranger.cved} containing:
#' \tabular{ll}{
#'    \code{data} \tab The traing data, e.g. \code{ranger},\cr
#'    \code{target} \tab The target variable,\cr
#'    \code{validation} \tab The validation function,\cr
#'    \code{predict_entities} \tab The predicted entities after validation,\cr
#'    \code{final_learner} \tab The name of the final learner and \cr
#'    \code{final_model} \tab The name of the final model.
#'  }
#' @export
ranger.cved <- function(multilrn,
                        validation = "createFolds",
                        validation_args = list(),
                        final_learner = "ranger",
                        final_learner_args = list()){
  ## Built layer entity from multi learners
  data <- lapply(X = multilrn, FUN = function(lrn){
    return(lrn$data)
  })
  data_names <- names(data)
  names(data) <- data_names
  target <- multilrn[[1]]$target
  learner <- multilrn[[1]]$learner
  ## Extract learner arguments
  learner_args <- lapply(X = multilrn, FUN = function(lrn){
    return(lrn$learner_args)
  })
  names(learner_args) <- learner_args
  ## Cross-validation and prediction on each layer.
  ## Set arguments for validation to their default values if not provided.
  validation_args <- if(!length(validation_args)){
    formals(validation)
  } else {
    validation_args
  }
  validation_args <- as.list(validation_args)
  validation_args$y <- target
  indexes <- do.call(eval(validation), validation_args)
  predict_entities <- lapply(1:length(indexes),
                             function(fold,
                                      test_list,
                                      data_pred,
                                      target_pred,
                                      learner_pred,
                                      learner_args_pred){
                               message(sprintf("Validation fold: %s\n", fold))
                               test_index <- test_list[[fold]]
                               n <- length(target_pred)
                               train_index <- setdiff(1:n, test_index)
                               ## Learner and predict
                               train_data <- vector(mode = "list",
                                                    length = length(data_pred))
                               test_data <- vector(mode = "list",
                                                   length = length(data_pred))
                               ## Prepare train and test datasets
                               names(train_data) <- names(data_pred)
                               names(test_data) <- names(data_pred)
                               lapply(1:length(data_pred),
                                      function (i){
                                        tmp_data <- data_pred[[i]]
                                        train_data[[i]] <<- tmp_data[train_index, ]
                                        test_data[[i]] <<- tmp_data[test_index, ]
                                        invisible(TRUE)
                                      })
                               ## Learn without variable selection
                               lrn <- multiLearner(data = train_data,
                                                   target = target[train_index],
                                                   learner = learner_pred,
                                                   learner_args = learner_args_pred)
                               ## Predict
                               pred <- predict(object = lrn,
                                               data = test_data)
                               ## return predictions as data.table object
                               pred_list <- lapply(1:length(data_pred),
                                                   function(i){
                                                     pred[[i]]$predictions$predictions
                                                   })
                               pred <- do.call(what = "cbind",
                                               args = pred_list)
                               pred <- data.table::as.data.table(pred)
                               pred$fold <- fold
                               pred$target <- target_pred[test_index]
                               # names(pred) <- c(names(data), "fold", "target")
                               return(pred)
                             },
                             test_list = indexes,
                             data_pred = data,
                             target_pred = target,
                             learner_pred = learner,
                             learner_args_pred = learner_args)
  ## Gather all predictions
  predict_entities <- data.table::rbindlist(predict_entities)
  ## Step 3: Estimate learner weights
  predictors <- names(predict_entities)
  x <- as.data.frame(predict_entities)[ , predictors]
  x$fold <- NULL
  x$target <- NULL
  final_learner_args$x <- as.matrix(x)
  # final_learner_args$y <- predict_entities$target
  final_learner_args$y <- target[unlist(indexes)]
  print(cbind(x, target[unlist(indexes)]))
  ## Set arguments of final learner to default arguments if not provided.
  final_learner_args <- if(!length(final_learner_args)){
    formals(final_learner)
  } else {
    final_learner_args
  }
  final_model <- do.call(eval(final_learner), final_learner_args)
  class(final_model) <- c("ranger.layer", class(final_model))
  final_object <- list(data = data,
                       target = target,
                       validation = validation,
                       predict_entities = predict_entities,
                       final_learner = final_learner,
                       final_model = final_model)
  class(final_object) <- "ranger.cved"
  return(final_object)
}
