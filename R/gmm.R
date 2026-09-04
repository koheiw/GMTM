#' Gaussian Mixture Model for topic analysis
#'
#' Fast Gaussian mixture model for clustering of document vectors based on the Armadillo library.
#' @param x a dense matrix of document vectors in rows.
#' @param k the number of topics to identify.
#' @param model a fitted model from which initial centroids are extracted.
#' @param verbose print the progress if `TRUE`.
#' @param ... passed to the underlying function.
#' @import Rcpp
#' @importFrom quanteda check_integer
#' @importFrom stats runif
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

#' Extract topics of documents
#' @param x a fitted model.
#' @rdname topics
#' @export
topics <- function(x, ...) {
  UseMethod("topics")
}

#' @method topics textmodel_gmm
#' @export
topics.textmodel_gmm <- function(x, ...) {
  v <- factor(x$cluster, levels = seq_len(x$k), labels = x$label)
  names(v) <- x$docname
  return(v)
}

#' Extract frequent words for topics
#' @rdname terms
#' @param x a fitted model.
#' @param n the number of topic words.
#' @param ... passed to functions.
#' @param data a dfm from which words are extracted for each topic.
#' @export
terms <- function(x, ...) {
  UseMethod("terms")
}

#' @method terms textmodel_gmm
#' @export
terms.textmodel_gmm <- function(x, data, n = 10, ...) {
  get_terms(topics(x), data, n = n, ...)
}

# #' @method predict textmodel_gmm
# #' @export
# predict.textmodel_gmm <- function(x, newdata, ...) {
#   if (missing(newdata)) {
#     p <- flexmix::posterior(x$flexmix, ...)
#     dimnames(p) <- list(x$docname, x$label)
#   } else {
#     if (!is.matrix(newdata))
#       stop("model must be a dense matrix")
#     p <- flexmix::posterior(x$flexmix, newdata = list(x = newdata), ...)
#     dimnames(p) <- list(rownames(newdata), x$label)
#   }
#   return(p)
# }

#' @keywords internal
#' @export
is.textmodel_gmm <- function(x) {
  "textmodel_gmm" %in% class(x)
}
