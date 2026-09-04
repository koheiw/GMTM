#' K-means model for topic clustering
#' @export
textmodel_kmeans <- function(x, k = 10, model = NULL, ...,
                             verbose = quanteda_options("verbose")) {

  dots <- list(...)

  if (!is.matrix(x))
    stop("model must be a dense matrix")

  label <- NULL
  if (!is.null(model)) {
    if (!is.textmodel_kmeans(model))
      stop("model must be a fitted textmodel_kmeans")
    cl <- model$kmeans$centers
    k <- nrow(cl)
    message("k is overwritten by the fitted model", call. = FALSE)
  } else if (!is.null(dots$centers) && is.matrix(dots$centers)) {
    cl <- dots$centers
    k <- nrow(cl)
    label <- rownames(dots$centers)
    message("k is overwritten by the centers", call. = FALSE)
  } else {
    cl <- k
  }

  if (is.null(label))
    label <- paste0("topic", seq_len(k))

  km <- kmeans(x, centers = cl, algorithm = "Lloyd", iter.max = 100)
  result <- list(kmeans = km,
                 data = x,
                 k = nrow(km$centers),
                 label = label,
                 docname = rownames(x))
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
  v <- x$kmeans$cluster
  v <- factor(v, levels = seq_len(x$k), labels = x$label)
  names(v) <- x$docname
  return(v)
}

#' @method terms textmodel_kmeans
#' @export
terms.textmodel_kmeans <- function(x, data, ...) {
  get_terms(topics(x), data, ...)
}

#' @method predict textmodel_kmeans
#' @export
predict.textmodel_kmeans <- function(x, newdata, ...) {
  if (missing(newdata)) {
    p <- fitted(x$kmeans)
    dimnames(p) <- list(x$docname, NULL)
  } else {
    dis <- as.matrix(proxyC::dist(newdata, x$kmeans$center,
                                  sparse = FALSE, use_nan = TRUE)) ^ 2
    p <- x$kmeans$center[max.col(dis * -1),, drop = FALSE]
    dimnames(p) <- list(rownames(newdata), NULL)
  }
  return(p)
}

#' @export
is.textmodel_kmeans <- function(x) {
  "textmodel_kmeans" %in% class(x)
}
