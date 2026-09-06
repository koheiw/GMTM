#' Gaussian mixture model for topic analysis
#'
#' Gaussian mixture model for clustering of document vectors based on the Armadillo library.
#' @param x a [wordvector::textmodel_doc2vec] or a dense matrix of document vectors in the rows.
#' @param k the number of topics to identify.
#' @param model a fitted model from which initial centroids are extracted.
#' @param verbose print the progress if `TRUE`.
#' @param ... passed to the underlying function.
#' @import Rcpp
#' @importFrom quanteda check_integer check_logical
#' @importFrom stats runif
#' @useDynLib GMTM
#' @export
#' @details
#' User can change the number of threads for the parallel computing via
#' `options(GMTM.threads)` or `OMP_THREAD_LIMIT` in the environmental
#' variable.
#'
#' @returns Returns a fitted `textmodel_gmm` object.
#' @examples
#' # dummy document vectors with 50 dimensions
#' mat <- t(replicate(1000, rnorm(50)))
#' gmm <- textmodel_gmm(mat, k = 10)
#' table(topics(gmm))
textmodel_gmm <- function(x, k = 10, model = NULL, seeds = NULL, ...,
                             verbose = quanteda_options("verbose")) {
  UseMethod("textmodel_gmm")
}

#' @export
#' @method textmodel_gmm matrix
textmodel_gmm.matrix <- function(x, k = 10, model = NULL, seeds = NULL, ...,
                                 verbose = quanteda_options("verbose")) {

  if (!is.matrix(x))
    stop("model must be a dense matrix")
  verbose <- check_logical(verbose)

  label <- NULL
  if (is.null(model) && is.null(seeds)) {
    k <- check_integer(k, min = 2)
    cl <- get_centers(ncol(x), k)
    label <- paste0("topic", seq_len(k))
  } else {
    if (!is.null(model)) {
      if (!is.textmodel_gmm(model))
        stop("model must be a fitted textmodel_gmm")
      k <- ncol(model$centers)
      cl <- model$centers
      label <- model$label
      message("k is overwritten by the fitted model")
    } else {
      k <- nrow(seeds)
      cl <- t(seeds)
      label <- rownames(seeds)
      message("k is overwritten by the seeds")
    }
  }

  RcppArmadillo::armadillo_set_number_of_omp_threads(get_threads())
  result <- cpp_gmm(x, k, means = cl, verbose = verbose, ...)

  result$cluster <- as.integer(result$cluster + 1)
  result$label <- label
  result$docname <- rownames(x)
  result$call <- try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  result$version <- utils::packageVersion("GMTM")
  class(result) <- "textmodel_gmm"
  return(result)
}

#' @export
#' @method textmodel_gmm textmodel_doc2vec
#' @import wordvector
textmodel_gmm.textmodel_doc2vec <- function(x, k = 10, model = NULL, seeds = NULL,
                                            verbose = quanteda_options("verbose"), ...) {
  textmodel_gmm(as.matrix(x, normalize = FALSE), k = k, model = model,
                seeds = seeds, verbose = verbose, ...)
}

#' Extract topics of documents
#' @param x a fitted model.
#' @param ... not used.
#' @rdname topics
#' @returns Returns predicted topics as a vector.
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

#' Extract words for topics from documents
#'
#' Identify distinctive words for each topic by applying TF-IDF weights to the
#' original [quanteda::dfm].
#' @rdname terms
#' @param x a fitted model.
#' @param n the number of topic words.
#' @param data a [quanteda::dfm] or [quanteda::tokens] from which words are extracted
#'   for each topic.
#' @param ... passed to functions.
#' @returns Returns a character matrix with the most distinctive words for each topic.
#' @details
#' To identify distinctive words for topics, original documents must be provided
#' along with a fitted model because the information about individual words are lost in
#' document vectors.
#' The documents in `data` is grouped by topic and weighted by TF-IDF
#' to select the most distinctive words for each topic. This technique is
#' commonly known as c-TF-IDF.
#' @export
terms <- function(x, data, n = 10, ...) {
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

#' @method print textmodel_gmm
#' @keywords internal
#' @export
print.textmodel_gmm <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\n", prettyNum(x$k, big.mark = ","), " topics; ",
      prettyNum(length(x$cluster), big.mark = ","), " documents; ",
      "\n", sep = "")
}

is.textmodel_gmm <- function(x) {
  "textmodel_gmm" %in% class(x)
}
