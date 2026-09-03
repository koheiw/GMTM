#' GMM model
#' @importFrom flexmix flexmix FLXMCmvnorm
#' @export
textmodel_gmm <- function(x, k = 10, model = NULL, ...,
                          verbose = quanteda_options("verbose")) {

  dots <- list(...)

  if (!is.matrix(x))
    stop("model must be a dense matrix")

  if (!is.null(model)) {
    if (!is.textmodel_gmm(model))
      stop("model must be a fitted textmodel_gmm")
    cl <- flexmix::posterior(model$flexmix, newdata = list(x = x),
                             unscaled = TRUE)
    k <- NULL
    label <- paste0("topic", seq_len(ncol(cl)))
    message("k is overwritten by the fitted model", call. = FALSE)
  } else {
    cl <- NULL
    label <- paste0("topic", seq_len(k))
  }

  if (!is.null(dots$cluster) && is.matrix(dots$cluster)) {
    cl <- dots$cluster
    label <- colnames(dots$cluster)
    message("k is overwritten by the cluster", call. = FALSE)
  }

  flx <- flexmix(x ~ 1, data = list(x = x), k = k, cluster = cl,
                model = FLXMCmvnorm(diagonal = TRUE),
                control = list(verbose = as.integer(verbose),
                               minprior = 0))
  result <- list(flexmix = flx,
                 data = x,
                 k = flx@k,
                 label = label)
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
  return(v)
}

#' @export
posterior <- function(x, ...) {
  UseMethod("posterior")
}

#' @method posterior textmodel_gmm
#' @export
posterior.textmodel_gmm <- function(x, newdata, ...) {
  if (missing(newdata)) {
    p <- flexmix::posterior(x$flexmix, ...)
  } else {
    p <- flexmix::posterior(x$flexmix, newdata = list(x = newdata), ...)
  }
  colnames(p) <- x$label
  return(p)
}

#' @method terms textmodel_gmm
#' @export
terms.textmodel_gmm <- function(x, data, ...) {
  get_terms(topics(x), data, ...)
}

#' @export
is.textmodel_gmm <- function(x) {
  "textmodel_gmm" %in% class(x)
}
