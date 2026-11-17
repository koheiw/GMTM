
get_centers <- function(nrow, ncol) {
  matrix(runif(nrow * ncol), ncol = ncol)
}

get_threads <- function() {

  # respect other settings
  default <- c("omp" = as.integer(Sys.getenv("OMP_THREAD_LIMIT")),
               "max" = RcppArmadillo::armadillo_get_number_of_omp_threads())
  default <- unname(min(default, na.rm = TRUE))
  suppressWarnings({
    value <- as.integer(getOption("GMTM.threads", default))
  })
  if (length(value) != 1 || is.na(value)) {
    stop("GMTM.threads must be an integer")
  }
  return(value)
}

#' Compute sum of rows by a given factor
#' @param x a matrix.
#' @param factor a factor indicating groups.
#' @param normalize if `TRUE`, normalize rows before grouping.
#' @keywords internal
#' @export
group_matrix <- function(x, factor, normalize = TRUE) {

  if (!is.matrix(x))
    stop("x must be a matrix")
  if (length(factor) != nrow(x))
    stop("The length of the factor does not much nrow(x)")

  if (normalize)
    x <- x / rowSums(abs(x))
  lis <- split(x, factor, drop = FALSE)
  t(sapply(lis, function(y) {
    if (length(y) == 0)
      y <- rep(0, ncol(x))
    p <- matrix(y, ncol = ncol(x))
    colSums(p, na.rm = TRUE)
  }))

}

get_terms <- function(x, n = 10) {
  if (nrow(x) == 0)
    return(x)
  if (nrow(x) == 1)
    return(t(apply(x, 2, function(y) rownames(x)[1])))
  utils::head(apply(x, 2, function(y) rownames(x)[order(y, decreasing = TRUE)]), n)
}

get_topics <- function(x) {

  if (!is.null(x$cluster)) { # v0.1.0
    topic <- x$cluster
  } else if (!is.null(x$topic)) { # v0.2.x
    topic <- max.col(x$topic.likelihood, ties.method = "first")
  } else {
    topic <- x$topic
  }
  structure(factor(topic, levels = seq_along(x$label), labels = x$label),
            names = rownames(x))
}

get_probability <- function(x, group = NULL) {

  if (is.null(x$topic.likelihood))
    stop("The model does not include topic.likelihood")

  if (!is.null(group)) {
    if (length(group) != nrow(x$topic.likelihood))
      stop("The length of group does not much the number of documents")
    prob <- group_matrix(x$topic.likelihood, group)
  } else {
    prob <- x$topic.likelihood
  }
  colnames(prob) <- x$label

  return(prob / rowSums(prob))
}


# copied from quanteda
unlist_character <- function(x, ...) {
  result <- unlist(x, ...)
  if (is.null(result)) {
    return(character())
  } else {
    return(result)
  }
}

# normalize <- function(x) {
#   s <- rowSums(abs(x))
#   l <- s == 0
#   x[] <- x / (s / ncol(x))
#   x[l,] <- 0 # replace NA with zero
#   return(x)
# }

upgrade_gmm <- function(x) {

  if (x$version < "0.2.0") {
    x$topic.likelihood <- x$likelihood
    x$topic <- x$cluster
    x$docvars <- data.frame("docname_" = x$docname)
    x[c("likelihood", "cluster", "docname")] <- NULL
  } else if (x$version < "0.3.0") {
    x$topic.likelihood <- x$cluster.likelihood
    x$topic <- x$cluster
    x[c("cluster.likelihood", "cluster", "docname")] <- NULL
  }
  return(x)
}

upgrade_kmeans <- function(x) {

  if (x$version < "0.2.0") {
    x$topic <- x$cluster
    x$docvars <- data.frame("docname_" = x$docname)
    x[c("cluster", "docname")] <- NULL
  } else if (x$version < "0.3.0") {
    x$topic <- x$cluster
    x[c("cluster", "docname")] <- NULL
  }
  return(x)
}

