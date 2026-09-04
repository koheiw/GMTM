#' Gaussian Mixture Model for topic clustering
#' @import Rcpp
#' @importFrom quanteda check_integer
#' @useDynLib GMTM
#' @export
textmodel_gmm <- function(x, k = 10, model = NULL, ...,
                          verbose = quanteda_options("verbose")) {

  if (!is.matrix(x))
    stop("model must be a dense matrix")

  label <- NULL
  if (is.null(model)) {
    k <- check_integer(k, min = 2)
    cl <- matrix(runif(ncol(x) * k), ncol = k)
    label <- paste0("topic", seq_len(k))
    message("k is overwritten by the cluster")
  } else {
    if (!is.textmodel_gmm(model))
      stop("model must be a fitted textmodel_gmm")
    cl <- model$centers
    label <- model$label
    message("k is overwritten by the fitted model")
  }

  result <- cpp_gmm(x, k, means = cl, verbose = verbose, ...)

  result$cluster <- as.integer(result$cluster + 1)
  result$label <- label
  result$docname <- rownames(x)
  class(result) <- "textmodel_gmm"
  return(result)
}

#' @export
topics <- function(x, ...) {
  UseMethod("topics")
}

#' @method topics textmodel_gmm
#' @export
topics.textmodel_gmm <- function(x) {
  # TODO: return factor with labels
  #v <- flexmix::clusters(x$flexmix)
  v <- factor(x$cluster, levels = seq_len(x$k), labels = x$label)
  names(v) <- x$docname
  return(v)
}

#' @method terms textmodel_gmm
#' @export
terms.textmodel_gmm <- function(x, data, ...) {
  get_terms(topics(x), data, ...)
}

#' #' @method predict textmodel_gmm
#' #' @export
#' predict.textmodel_gmm <- function(x, newdata, ...) {
#'   if (missing(newdata)) {
#'     p <- flexmix::posterior(x$flexmix, ...)
#'     dimnames(p) <- list(x$docname, x$label)
#'   } else {
#'     if (!is.matrix(newdata))
#'       stop("model must be a dense matrix")
#'     p <- flexmix::posterior(x$flexmix, newdata = list(x = newdata), ...)
#'     dimnames(p) <- list(rownames(newdata), x$label)
#'   }
#'   return(p)
#' }

#' @export
is.textmodel_gmm <- function(x) {
  "textmodel_gmm" %in% class(x)
}
