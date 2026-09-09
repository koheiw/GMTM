#' Gaussian mixture model for topic analysis
#'
#' Gaussian mixture model for clustering of document vectors based on the Armadillo library.
#' @param x a [wordvector::textmodel_doc2vec] or a dense matrix of document vectors in the rows.
#' @param k the number of topics to identify.
#' @param model a fitted model from which initial centroids are extracted.
#' @param seeds a matrix created using [GMTM::as.seedwords].
#' @param verbose print the progress if `TRUE`.
#' @param ... passed to the underlying function.
#' @import Rcpp
#' @importFrom quanteda check_integer check_logical
#' @importFrom stats runif
#' @useDynLib GMTM
#' @export
#' @details
#' Users can change the number of threads for the parallel computing via
#' `options(GMTM.threads)` or `OMP_THREAD_LIMIT` in the environmental
#' variable.
#'
#' The number of iterations in kmeans (`iter_km`) and expectation maximization
#' (`iter_em`) stages can be set via `...`.
#' @returns Returns a fitted `textmodel_gmm` object.
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
#' gmm <- textmodel_gmm(dov, k = 10)
#' table(topics(gmm))
textmodel_gmm <- function(x, k = 10, model = NULL, seeds = NULL, ...,
                             verbose = quanteda_options("verbose")) {
  UseMethod("textmodel_gmm")
}

#' @export
#' @method textmodel_gmm matrix
textmodel_gmm.matrix <- function(x, k = 10, model = NULL, seeds = NULL, ...,
                                 verbose = quanteda_options("verbose")) {

  verbose <- check_logical(verbose)

  label <- NULL
  if (is.null(model) && is.null(seeds)) {
    k <- check_integer(k, min = 2)
    cl <- get_centers(ncol(x), k)
    label <- paste0("topic", seq_len(k))
  } else if (!is.null(model) && !is.null(seeds)) {
    stop("either the model or seeds must be NULL")
  } else {
    if (!is.null(model)) {
      if (!is.textmodel_gmm(model))
        stop("the model must be a fitted textmodel_gmm")
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

  result <- cpp_gmm(x, k, means = cl, verbose = verbose, threads = get_threads(), ...)

  # NA for empty documents
  b <- rowSums(abs(x)) == 0
  result$cluster[b] <- NA_real_
  result$cluster.likelihood[b,] <- NA_real_

  result$cluster <- as.integer(result$cluster + 1)
  result$label <- label
  result$docname <- rownames(x)
  result$docvars <- data.frame(docname_ = rownames(x))
  result$call <- try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  result$version <- utils::packageVersion("GMTM")
  class(result) <- c("textmodel_gmm", "textmodel_gmtm")
  return(result)
}

#' @export
#' @method textmodel_gmm textmodel_doc2vec
#' @import wordvector
textmodel_gmm.textmodel_doc2vec <- function(x, k = 10, model = NULL, seeds = NULL,
                                            verbose = quanteda_options("verbose"), ...) {
  result <- textmodel_gmm(as.matrix(x, normalize = FALSE), k = k, model = model,
                          seeds = seeds, verbose = verbose, ...)
  if (!is.null(x$docvars))
    result$docvars <- x$docvars
  return(result)
}

#' Extract the topics of documents
#' @param x a fitted model.
#' @param group if `TRUE`, aggregate the probability of topics by the original
#'   document `doc_id`. Ignored if `x` is a `textmodel_kmeans` object.
#' @param ... not used.
#' @rdname topics
#' @returns Returns predicted topics as a vector.
#' @details
#' The original `doc_id` is inherited from [quanteda::dfm] or [quanteda::tokens]
#' and saved in `x$dovars$docid_` as factor.
#'
#' @export
topics <- function(x, group = FALSE, ...) {
  UseMethod("topics")
}

#' @method topics textmodel_gmm
#' @export
topics.textmodel_gmm <- function(x, group = FALSE, ...) {
  get_topics(x, group)
}

#' @importFrom wordvector probability
#' @export
wordvector::probability

#' Extract the probabilities of topics
#' @inheritParams topics
#' @returns Returns the probabilities of topics as a matrix.
#' @details
#' The original `doc_id` is inherited from [quanteda::dfm] or [quanteda::tokens]
#' and saved in `x$dovars$docid_` as factor.
#' @method probability textmodel_gmm
#' @export
probability.textmodel_gmm <- function(x, group = FALSE, ...) {
  get_probability(x, group)
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
