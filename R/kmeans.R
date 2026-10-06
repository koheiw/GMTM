#' Topic analysis using k-means
#'
#' Perform topic analysis of document vectors using k-means.
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
#'                remove_symbols = TRUE, remove_numbers = TRUE) %>%
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

  if (any(is.na(x)))
    stop("x should not contain any NA")
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
      if (!is.textmodel_kmeans(model))
        stop("the model must be a fitted textmodel_kmeans")
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

  temp <- cpp_kmeans(x, k, means = cl, verbose = verbose, threads = get_threads(), ...)
  dis <- proxyC::dist(x, t(temp$centers), sparse = FALSE)
  topic <- max.col(-1 * dis ^ 2, ties.method = "first")
  names(topic) <- rownames(x)

  # NA for empty documents
  b <- rowSums(abs(x)) == 0
  temp$cluster[b] <- NA_integer_

  build_kmeans(
    k = k,
    centers = temp$centers,
    topic = topic,
    label = label,
    call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  )
}

#' @export
#' @method textmodel_kmeans textmodel_doc2vec
#' @import wordvector
textmodel_kmeans.textmodel_doc2vec <- function(x, k = 10, model = NULL, seeds = NULL,
                                               verbose = quanteda_options("verbose"), ...) {
  temp <- textmodel_kmeans(as.matrix(x, normalize = FALSE), k = k, model = model,
                           seeds = seeds, verbose = verbose)
  build_kmeans(
    model = temp,
    frequency = x$frequency,
    docvars = x$docvars,
    call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  )
}

#' @method topics textmodel_kmeans
#' @export
topics.textmodel_kmeans <- function(x, ...) {
  factor(x$topic, levels = seq_along(x$label), labels = x$label)
}

#' @method terms textmodel_kmeans
#' @export
terms.textmodel_kmeans <- function(x, data, n = 10, filter = NULL, ...) {
  terms(topics(x), data, n, filter, ...)
}

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
