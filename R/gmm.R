#' Gaussian Mixture Model for topic clustering
#' @importFrom flexmix flexmix FLXMCmvnorm
#' @export
textmodel_gmm <- function(x, k = 10, model = NULL, ...,
                          verbose = quanteda_options("verbose")) {

  dots <- list(...)

  if (!is.matrix(x))
    stop("model must be a dense matrix")

  label <- NULL
  if (!is.null(model)) {
    if (!is.textmodel_gmm(model))
      stop("model must be a fitted textmodel_gmm")
    cl <- flexmix::posterior(model$flexmix, newdata = list(x = x),
                             unscaled = TRUE)
    k <- ncol(cl)
    message("k is overwritten by the fitted model", call. = FALSE)
  } else if (!is.null(dots$cluster) && is.matrix(dots$cluster)) {
    cl <- dots$cluster
    k <- ncol(cl)
    label <- colnames(dots$cluster)
    message("k is overwritten by the cluster", call. = FALSE)
  } else {
    cl <- NULL
  }

  if (is.null(label))
    label <- paste0("topic", seq_len(k))

  flx <- flexmix(x ~ 1, data = list(x = x), k = k, cluster = cl,
                model = FLXMCmvnorm(diagonal = TRUE),
                control = list(verbose = as.integer(verbose),
                               minprior = 0))
  result <- list(flexmix = flx,
                 data = x,
                 k = flx@k,
                 label = label,
                 docname = rownames(x))
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
  v <- flexmix::clusters(x$flexmix)
  v <- factor(v, levels = seq_len(x$k), labels = x$label)
  names(v) <- x$docname
  return(v)
}

#' @method terms textmodel_gmm
#' @export
terms.textmodel_gmm <- function(x, data, ...) {
  get_terms(topics(x), data, ...)
}

#' @method predict textmodel_gmm
#' @export
predict.textmodel_gmm <- function(x, newdata, ...) {
  if (missing(newdata)) {
    p <- flexmix::posterior(x$flexmix, ...)
    dimnames(p) <- list(x$docname, x$label)
  } else {
    if (!is.matrix(newdata))
      stop("model must be a dense matrix")
    p <- flexmix::posterior(x$flexmix, newdata = list(x = newdata), ...)
    dimnames(p) <- list(rownames(newdata), x$label)
  }
  return(p)
}

#' @export
is.textmodel_gmm <- function(x) {
  "textmodel_gmm" %in% class(x)
}
