#' Variable selection using Boruta function. The Pomona package inspires it.
#'
#' Variable selection using the Boruta function in the R package \code{\link[Boruta]{Boruta}}.
#'
#' This function selects only variables that are confirmed based on Boruta implementation.
#' For more details see \code{\link[Boruta]{Boruta}}.
#' Note that this function uses the ranger implementation for variable selection.
#'

#' @inheritParams wrapper.rf
#' @param pValue confidence level (default: 0.01 based on Boruta package)
#' @param maxRuns maximal number of importance source runs (default: 100 based on Boruta package)
#' @param holdout If TRUE, the holdout variable importance is calculated.
#'
#' @return List with the following components:
#'   \itemize{
#'   \item \code{info} data.frame
#'   with information of each variable
#'   \itemize{
#'   \item run.x = original variable importance (VIM) in run x
#'   (includes min, mean and max of VIM of shadow variables)
#'   \item decision = Boruta decision (Confirmed, Rejected or Tentative)
#'   \item selected = variable has been selected
#'   }
#'   \item \code{var} vector of selected variables
#'   \item \code{info.shadow.var} data.frame with information about
#'   minimal, mean and maximal shadow variables of each run
#'   }
#'
#'
#' @export


Boruta_ext <- function(x,
                       y,
                       pValue = 0.01,
                       maxRuns = 100,
                       ntree = 500,
                       mtry.prop = 0.2,
                       nodesize.prop = 0.1,
                       no.threads = 1,
                       method = "ranger",
                       type = "regression",
                       importance = "impurity_corrected",
                       replace = TRUE,
                       sample.fraction = 1,
                       holdout = FALSE,
                       case.weights = NULL) {
  ## variable selection using Boruta function
  ## ----------------------------------------
  # modified version of getImpRfRaw function to enable user defined mtry
  # values
  get.imp.r.f.raw.mtry <- function(x, y, ...){
    x <- data.frame(x)
    imp <- wrapper.rf(x = x,
                      y = y,
                      ...)$variable.importance
    return(imp)
  }
  if(!(is.logical(holdout))){
    stop("Logical value required for 'holdout'.")
  }
  unbiased_imp_method <- if(holdout){
    if(importance != "permutation"){
      stop("Set importance to 'permutation' for holdout.")
    }
    ## train holdout RFs
    new_holdout.rf <- function(x = x, y = y,
                               ntree = ntree,
                               mtry.prop = mtry.prop,
                               nodesize.prop = nodesize.prop,
                               no.threads = no.threads,
                               type = type,
                               importance = importance,
                               replace = replace,
                               sample.fraction = sample.fraction,
                               case.weights = case.weights,
                               ...){
      holdout.rf(x = x, y = y,
                 ntree = ntree,
                 mtry.prop = mtry.prop,
                 nodesize.prop = nodesize.prop,
                 no.threads = no.threads,
                 type = type,
                 importance = importance,
                 replace = replace,
                 sample.fraction = sample.fraction,
                 ...)$variable.importance
    }

  } else {
    get.imp.r.f.raw.mtry
  }
  ## variable selection using Boruta function
  ## ----------------------------------------
  res.boruta = Boruta::Boruta(x = x, y = y,
                              pValue = pValue,
                              maxRuns = maxRuns,
                              ntree = ntree,
                              nodesize.prop = nodesize.prop,
                              no.threads = no.threads,
                              mtry.prop = mtry.prop,
                              getImp = unbiased_imp_method,
                              importance = importance,
                              replace = replace,
                              sample.fraction = sample.fraction,
                              type = type,
                              case.weights = case.weights)

  ## select variables
  dec = res.boruta$finalDecision
  ind.sel = rep(0, ncol(x))
  ind.sel[dec == "Confirmed"] = 1
  info.sel = data.frame(decision = dec, selected = ind.sel)

  ## info about variables
  info.var = t(res.boruta$ImpHistory)
  colnames(info.var) = paste("run", 1:ncol(info.var), sep = ".")
  info.shadow.var = info.var[grep("shadow", rownames(info.var)),]
  info.var = info.var[-grep("shadow", rownames(info.var)),]
  if (all.equal(rownames(info.var), rownames(info.sel))) {
    info.var = cbind(info.var, info.sel)
  } else {
    info.var = merge(info.var, info.sel, by.x = "row.names", by.y = "row.names")
  }
  selected_res <- list(info = info.var,
                       var = sort(rownames(info.var)[info.var$selected == 1]),
                       info.shadow.var = info.shadow.var)
  class(selected_res) <- "Boruta_ext"
  return(selected_res)
}

#' Print object of class 'Boruta.ext'
#'
#' @param x Object of class Boruta.ext.
#' @param ... To be used by the further print function.
#'
#' @export
#'
print.Boruta_ext <- function(x, ...) {
  cat(sprintf("Predictors          : %s\n", nrow(x$info)))
  cat(sprintf("Selected            : %s\n", sum(x$info$selected)))
  cat(sprintf("Rejected            : %s\n", sum(!x$info$selected)))
  cat(sprintf("Number of iterations: %s\n", ncol(x$info) - 2L))
}


#' Helper function for variable selection using Vita approach.
#'
#' This function calculates a modified version of the permutation importance using two cross-validation folds (holdout folds)
#' as described in Janitza et al. (2015). Note that this function is a reimplementation of the \code{holdoutRF} function in the
#' R package \code{\link[ranger]{ranger}}.
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
#' @param type mode of prediction ("regression", "classification" or "probability").
#' @param importance See \code{\link[ranger]{ranger}} for details.
#' @param replace See \code{\link[ranger]{ranger}} for details.
#' @param sample.fraction See \code{\link[ranger]{ranger}} for details.
#' @param case.weights See \code{\link[ranger]{ranger}} for details.
#' @param ... additional parameters passed to \code{ranger}.
#'
#' @return Hold-out random forests with variable importance
#'
#' @references
#' Janitza, S., Celik, E. & Boulesteix, A.-L., (2015). A computationally fast variable importance test for random forest for high dimensional data, Technical Report 185, University of Munich, https://epub.ub.uni-muenchen.de/25587.

holdout.rf <- function(x, y, ntree = 500,
                       mtry.prop = 0.2,
                       nodesize.prop = 0.1,
                       no.threads = 1,
                       type = "regression",
                       importance = importance,
                       replace = TRUE,
                       sample.fraction = ifelse(replace, 1, 0.632),
                       case.weights = NULL,
                       ...) {

  ## define two cross-validation folds
  n = nrow(x)
  weights = rbinom(n, 1, 0.5)

  ## train two RFs
  res = list(rf1 = wrapper.rf(x = x, y = y,
                              ntree = ntree, mtry.prop = mtry.prop,
                              nodesize.prop = nodesize.prop, no.threads = no.threads,
                              method = "ranger", type = type,
                              case.weights = weights, replace = replace,
                              sample.fraction = sample.fraction,
                              holdout = TRUE, importance = importance,
                              ...),
             rf2 = wrapper.rf(x = x, y = y,
                              ntree = ntree, mtry.prop = mtry.prop,
                              nodesize.prop = nodesize.prop, no.threads = no.threads,
                              method = "ranger", type = type,
                              case.weights = 1 - weights, replace = replace,
                              sample.fraction = sample.fraction,
                              holdout = TRUE,  importance = importance,
                              ...))

  ## calculate mean VIM
  res$variable.importance = (res$rf1$variable.importance +
                               res$rf2$variable.importance)/2
  res$treetype = res$rf1$treetype
  res$importance.mode = res$rf1$importance.mode
  class(res) = "holdoutRF"
  return(res)
}
