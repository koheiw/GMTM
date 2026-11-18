
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

  if (x$version >= "0.3.0")
    return(x)

  if (is.null(x[["topic.likelihood"]])) {
    if (!is.null(x[["likelihood"]])) { # < v0.2.0
      x$topic.likelihood <- x$likelihood
      rownames(x$topic.likelihood) <- x$docname
    } else if (!is.null(x[["cluster.likelihood"]])) { # < v0.3.0
      x$topic.likelihood <- x$cluster.likelihood
      rownames(x$topic.likelihood) <- x$docname
    }
  }
  if (is.null(x[["topic"]])) {
    x$topic <- structure(x$cluster, names = x$docname)
  }
  x[c("likelihood", "cluster.likelihood", "cluster", "docname")] <- NULL
  x$version <- utils::packageVersion("GMTM")
  return(x)
}

upgrade_kmeans <- function(x) {

  if (x$version >= "0.3.0")
    return(x)

  if (is.null(x[["topic"]])) {
    x$topic <- structure(x$cluster, names = x$docname)
  }
  x[c("cluster", "docname")] <- NULL
  x$version <- utils::packageVersion("GMTM")
  return(x)
}

