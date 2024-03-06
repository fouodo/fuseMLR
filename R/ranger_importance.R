
#' Wrapper function to call random forests function.
#'
#' Provides an interface to different parallel implementations of the random
#' forest algorithm. Currently, only the \code{ranger} package is
#' supported.
#'
#' @param x matrix or data.frame of predictor variables with variables in
#'   columns and samples in rows (Note: missing values are not allowed).
#' @param y vector with values of phenotype variable (Note: will be converted to factor if
#'   classification mode is used).
#' @param ntree number of trees.
#' @param mtry.prop proportion of variables that should be used at each split.
#' @param nodesize.prop proportion of minimal number of samples in terminal
#'   nodes.
#' @param no.threads number of threads used for parallel execution.
#' @param method implementation to be used ("ranger").
#' @param type mode of prediction ("regression", "classification" or "probability").
#' @param importance Variable importance mode ('none', 'impurity',
#' 'impurity_corrected' or 'permutation'). Default is 'impurity_corrected'.
#' @param replace Sample with replacement.
#' @param sample.fraction Fraction of observations to sample. Default is 1 for sampling with replacement and 0.632 for sampling without replacement. For classification, this can be a vector of class-specific values.
#' @param case.weights Weights for sampling of training observations. Observations with larger weights will be selected with higher probability in the bootstrap (or subsampled) samples for the trees.
#' @param ... further arguments needed for ranger.
#'
#' @return An object of class \code{\link[ranger]{ranger}}.
#'
#' @import methods stats
#'
#' @export

wrapper.rf <- function(x,
                       y,
                       ntree = 500,
                       mtry.prop = 0.2,
                       nodesize.prop = 0.1,
                       no.threads = 1,
                       method = "ranger",
                       type = "regression",
                       importance = "impurity_corrected",
                       replace = TRUE,
                       sample.fraction = ifelse(replace, 1, 0.632),
                       case.weights = NULL, ...) {
  ## check data
  if (length(y) != nrow(x)) {
    stop("length of y and number of rows in x are different")
  }

  if (any(is.na(x))) {
    stop("missing values are not allowed")
  }

  if (type %in% c("probability", "regression") & (is.character(y) | is.factor(y))) {
    stop("only numeric y allowed for probability or regression mode")
  }

  ## set global parameters
  nodesize = floor(nodesize.prop * nrow(x))
  mtry = floor(mtry.prop * ncol(x))
  if (mtry == 0) mtry = 1

  if (type == "classification") {
    #    print("in classification")
    y = as.factor(y)
  }

  ## run RF
  if (method == "ranger") {
    if (type == "probability") {
      y = as.factor(y)
      prob = TRUE
    } else {
      prob = FALSE
    }
    rf = ranger::ranger(data = data.frame(y, x),
                        dependent.variable.name = "y",
                        probability = prob,
                        importance = importance,
                        scale.permutation.importance = FALSE,
                        num.trees = ntree,
                        mtry = mtry,
                        case.weights = case.weights,
                        min.node.size = nodesize,
                        num.threads = no.threads,
                        sample.fraction = sample.fraction,
                        write.forest = TRUE,
                        ...)
  } else {
    stop(paste("method", method, "undefined. Use 'ranger'."))
  }
  return(rf)
}


#' Get variable importance.
#'
#' Extracts variable importance depending on class of random forest object.
#'
#' @param rf Object of class \code{\link[ranger]{ranger}}
#'
#' @return numeric vector with importance value for each variable (in original order)
#'
#' @export

get.vim <- function(rf) {
  if (is(rf, "ranger")) {
    vim = ranger::importance(rf)
  } else {
    stop(paste("rf needs to be of class ranger"))
  }
  return(vim)
}
