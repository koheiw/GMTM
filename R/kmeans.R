#' K-means for topic clustering
#' @import Rcpp
#' @importFrom quanteda check_integer
#' @useDynLib GMTM
#' @export
textmodel_kmeans <- function(x, k = 10, model = NULL, ...,
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
    if (!is.textmodel_kmeans(model))
      stop("model must be a fitted textmodel_kmeans")
    cl <- model$centers
    label <- model$label
    message("k is overwritten by the fitted model")
  }

  result <- cpp_kmeans(x, k, means = cl, verbose = verbose, ...)

  dis <- proxyC::dist(x, t(result$centers), sparse = FALSE)
  result$cluster <- max.col(-1 * dis ^ 2)
  result$label <- label
  result$docname <- rownames(x)
  class(result) <- "textmodel_kmeans"
  return(result)
}

#' @export
topics <- function(x, ...) {
  UseMethod("topics")
}

#' @method topics textmodel_kmeans
#' @export
topics.textmodel_kmeans <- function(x) {
  # TODO: return factor with labels
  #v <- flexmix::clusters(x$flexmix)
  v <- factor(x$cluster, levels = seq_len(x$k), labels = x$label)
  names(v) <- x$docname
  return(v)
}

#' @method terms textmodel_kmeans
#' @export
terms.textmodel_kmeans <- function(x, data, ...) {
  get_terms(topics(x), data, ...)
}

#' #' @method predict textmodel_kmeans
#' #' @export
#' predict.textmodel_kmeans <- function(x, newdata, ...) {
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
is.textmodel_kmeans <- function(x) {
  "textmodel_kmeans" %in% class(x)
}
