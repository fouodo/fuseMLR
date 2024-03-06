#' Print an abject of class 'multiLearner'.
#'
#' @param x Object of class 'multiLearner'
#' @param ... [\code{any}]\cr
#'
#' @export
print.multiLearner <- function(x, ...) {
  for (i in 1:length(x)) {
    model <- x[[i]]
    if (class(model[["model"]]) %in% c("ranger.layer")) {
      cat(sprintf("Layer                           :%s\n\n", model[["layer"]]))
      print(model[["model"]], ...)
      cat("\n")
    } else {
      print(utils::head(x = model))
    }
  }
  cat(sprintf("Multilayer learner              : %s", x[[1]]$learner))
}

#' Print an abject of class 'ranger.layer'.
#'
#' @param x Object of class 'ranger.layer'
#' @param ... [\code{any}]\cr
#'
#' @export
print.ranger.layer <- function(x, ...) {
  cat("Ranger result\n")
  cat("Type                            :", x$treetype, "\n")
  cat("Number of trees                 :", x$num.trees, "\n")
  cat("Sample size                     :", x$num.samples, "\n")
  cat("Number of independent variables :", x$num.independent.variables, "\n")
  cat("Mtry                            :", x$mtry, "\n")
  cat("Target node size                :", x$min.node.size, "\n")
  cat("Variable importance mode        :", x$importance.mode, "\n")
  cat("Splitrule                       :", x$splitrule, "\n")
  if (x$treetype == "Survival") {
    cat("Number of unique death times    :", length(x$unique.death.times), "\n")
  }
  if (!is.null(x$splitrule) && x$splitrule == "extratrees" && !is.null(x$num.random.splits)) {
    cat("Number of random splits         :", x$num.random.splits, "\n")
  }
  if (x$treetype == "Classification") {
    cat("OOB prediction error            :", sprintf("%1.2f %%", 100*x$prediction.error), "\n")
  } else if (x$treetype == "Regression") {
    cat("OOB prediction error (MSE)      :", x$prediction.error, "\n")
  } else if (x$treetype == "Survival") {
    cat("OOB prediction error (1-C)      :", x$prediction.error, "\n")
  } else if (x$treetype == "Probability estimation") {
    cat("OOB prediction error (Brier s.) :", x$prediction.error, "\n")
  } else {
    cat("OOB prediction error            :", x$prediction.error, "\n")
  }
  if (x$treetype == "Regression") {
    cat("R squared (OOB)                 :", x$r.squared, "\n")
  }
}


#' Print object of class 'multiLearner.predict'
#'
#' @param x Object of class 'multiLearner.predict'
#' @param ... [\code{any}]\cr
#'
#' @export
print.multiLearner.predict <- function (x, ...) {
  for (i in 1:length(x)) {
    predicted_obj <- x[[i]]
    if (class(predicted_obj[["predictions"]]) %in% c("ranger.prediction")) {
      cat(sprintf("Layer: %s \n", predicted_obj[["layer"]]))
      print(predicted_obj[["predictions"]], ...)
      cat("\n")
    } else {
      print(utils::head(x = predicted_obj))
    }
  }
}

