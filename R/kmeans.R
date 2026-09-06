#' K-means for topic analysis
#'
#' K-means clustering of document vectors based on the Armadillo library.
#' @inheritParams textmodel_gmm
#' @import Rcpp
#' @importFrom quanteda check_integer check_logical
#' @importFrom stats runif
#' @useDynLib GMTM
#' @export
#' @returns Returns a fitted `textmodel_kmeans` object.
#' @examples
#' library(quanteda)
#' library(wordvector)
#' options(wordvector_threads = 2)
#'
#' corp <- head(wordvector::data_corpus_news2014, 1000)
#' toks <- tokens(corp, remove_punct = TRUE,
#'                remove_symbols = TRUE, remove_number = TRUE) %>%
#'         tokens_remove(stopwords("en"), min_nchar = 2)
#' wov <- textmodel_word2vec(toks, dim = 50)
#' dov <- as.textmodel_doc2vec(dfm(toks), wov)
#'
#' km <- textmodel_kmeans(dov, k = 10)
#' table(topics(km))
textmodel_kmeans <- function(x, k = 10, model = NULL, seeds = NULL,
                             verbose = quanteda_options("verbose"), ...) {
  UseMethod("textmodel_kmeans")
}

#' @export
#' @method textmodel_kmeans matrix
textmodel_kmeans.matrix <- function(x, k = 10, model = NULL, seeds = NULL,
                             verbose = quanteda_options("verbose"), ...) {

  if (!is.matrix(x))
    stop("model must be a dense matrix")
  verbose <- check_logical(verbose)

  label <- NULL
  if (is.null(model) && is.null(seeds)) {
    k <- check_integer(k, min = 2)
    cl <- get_centers(ncol(x), k)
    label <- paste0("topic", seq_len(k))
  } else if (!is.null(model) && !is.null(seeds)) {
    stop("either model or seeds must be NULL")
  } else {
    if (!is.null(model)) {
      if (!is.textmodel_kmeans(model))
        stop("model must be a fitted textmodel_kmeans")
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
  result <- cpp_kmeans(x, k, means = cl, verbose = verbose, ...)

  dis <- proxyC::dist(x, t(result$centers), sparse = FALSE)
  result$cluster <- max.col(-1 * dis ^ 2)
  result$label <- label
  result$docname <- rownames(x)
  result$call <- try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  result$version <- utils::packageVersion("GMTM")
  class(result) <- "textmodel_kmeans"
  return(result)
}

#' @export
#' @method textmodel_kmeans textmodel_doc2vec
#' @import wordvector
textmodel_kmeans.textmodel_doc2vec <- function(x, k = 10, model = NULL, seeds = NULL,
                                               verbose = quanteda_options("verbose"), ...) {
  textmodel_kmeans(as.matrix(x, normalize = FALSE), k = k, model = model,
                   seeds = seeds, verbose = verbose)
}

#' @method topics textmodel_kmeans
#' @export
topics.textmodel_kmeans <- function(x, ...) {
  v <- factor(x$cluster, levels = seq_len(x$k), labels = x$label)
  names(v) <- x$docname
  return(v)
}

#' @method terms textmodel_kmeans
#' @export
terms.textmodel_kmeans <- function(x, data, n = 10, ...) {
  get_terms(topics(x), data, n = n, ...)
}

# #' @method predict textmodel_kmeans
# #' @export
# predict.textmodel_kmeans <- function(x, newdata, ...) {
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

#' @method print textmodel_kmeans
#' @keywords internal
#' @export
print.textmodel_kmeans <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\n", prettyNum(x$k, big.mark = ","), " topics; ",
      prettyNum(length(x$cluster), big.mark = ","), " documents; ",
      "\n", sep = "")
}

is.textmodel_kmeans <- function(x) {
  "textmodel_kmeans" %in% class(x)
}

is.textmodel_doc2vec <- function(x) {
  "textmodel_doc2vec" %in% class(x)
}

