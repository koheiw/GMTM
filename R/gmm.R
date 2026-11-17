#' Topic analysis using Gaussian mixture models
#'
#' Perform topic analysis of document vectors using Gaussian mixture models.
#' @param x a [wordvector::textmodel_doc2vec] or a dense matrix of document vectors in the rows.
#' @param k the number of topics to identify.
#' @param model a fitted model from which initial centroids are extracted.
#' @param seeds a matrix created using [GMTM::as.seedwords].
#' @param omit \[experimental\] indices of singular values of `x` to be zero. See the details.
#' @param include_prob if `TRUE`, the resulting object includes `topic.likelihood` that records
#'   probabilities for all the topics. `include_prob = FALSE` reduces the size of the object
#'   significantly.
#' @param verbose print the progress if `TRUE`.
#' @param ... passed to the underlying function.
#' @import Rcpp
#' @importFrom quanteda check_integer check_logical
#' @importFrom stats runif
#' @useDynLib GMTM
#' @export
#' @details
#'
#' ### multi-threading
#'
#' Users can change the number of threads for the parallel computing via
#' `options(GMTM.threads)` or `OMP_THREAD_LIMIT` in the environmental
#' variable. To reproduce results, set `options(GMTM.threads = 1)` and call
#' `set.seed()` immediately before `textmodel_gmm()` or `textmodel_kmeans()`.
#'
#' On MacOS, only one thread is used regardless of `GMTM.threads`
#' because CRAN's toolchain for the platform does not support OpenMP.
#'
#' ### noise reduction
#'
#' `omit` is used to reduce the noise in the `x` by applying `base::svd` before
#' clustering. If it is not `NULL`, singular values corresponding to `omit` are
#' set to zero, removing their variance in `x`. See Chan et al. (2020)
#' <doi:10.1080/19312458.2020.1812555> for the methodology.
#'
#' ### additional arguments
#'
#' The number of iterations in k-means (`iter_km`) and expectation maximization
#' (`iter_em`) stages can be set via `...`.
#' @returns Returns a fitted `textmodel_gmm` object.
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
#' gmm <- textmodel_gmm(dov, k = 10)
#' table(topics(gmm))
textmodel_gmm <- function(x, k = 10, model = NULL,
                          seeds = NULL, include_prob = TRUE, omit = NULL,
                          verbose = quanteda_options("verbose"),
                          ...) {
  UseMethod("textmodel_gmm")
}

#' @export
#' @method textmodel_gmm matrix
textmodel_gmm.matrix <- function(x, k = 10, model = NULL,
                                 seeds = NULL, include_prob = TRUE, omit = NULL,
                                 verbose = quanteda_options("verbose"),
                                 ...) {

  if (any(is.na(x)))
    stop("x should not contain any NA")

  include_prob <- check_logical(include_prob)
  verbose <- check_logical(verbose)

  if (!is.null(omit)) {
    omit <- check_integer(omit, min = 1, max = ncol(x), max_len = ncol(x))
    s <- svd(x)
    s$d[omit] <- 0
    x[] <- s$u %*% diag(s$d) %*% t(s$v)
  }

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

  temp <- cpp_gmm(x, k, means = cl, verbose = verbose, threads = get_threads(), ...)
  temp$topic <- max.col(temp$topic.likelihood)

  # NA for empty documents
  b <- rowSums(abs(x)) == 0
  temp$topic[b] <- NA_real_
  if (include_prob) {
    temp$topic.likelihood[b,] <- NA_real_
    rownames(temp$topic.likelihood) <- rownames(x)
  } else {
    temp$topic.likelihood <- NULL
  }

  build_gmm(
    k = temp$k,
    omit = omit,
    centers = temp$centers,
    topic = temp$topic,
    covariance = temp$covariance,
    topic.likelihood = temp$topic.likelihood,
    model.likelihood = temp$model.likelihood,
    label = label,
    call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  )
}

#' @export
#' @method textmodel_gmm textmodel_doc2vec
#' @import wordvector
textmodel_gmm.textmodel_doc2vec <- function(x, k = 10, model = NULL,
                                            seeds = NULL, omit = NULL,
                                            verbose = quanteda_options("verbose"),
                                            ...) {

  temp <- textmodel_gmm(as.matrix(x, normalize = FALSE), k = k, model = model,
                        seeds = seeds, omit = omit, verbose = verbose, ...)
  build_gmm(
    model = temp,
    frequency = x$frequency,
    docvars = x$docvars,
    call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  )
}

#' @importFrom wordvector probability
#' @export
wordvector::probability

#' Extract the probabilities for topics
#' @inheritParams topics
#' @param ... not used.
#' @returns Returns the probabilities of topics as a matrix.
#' @method probability textmodel_gmm
#' @export
probability.textmodel_gmm <- function(x, group = NULL, ...) {
  get_probability(x, group)
}

#' Extract the most likely topics of documents
#' @param x a fitted model.
#' @param group a factor to group documents and average their probability for
#' topics. Ignored if `x` is a `textmodel_kmeans` object.
#' @param ... not used.
#' @returns Returns predicted topics as a vector.
#' @export
topics <- function(x, group = NULL, ...) {
  UseMethod("topics")
}

#' @method topics textmodel_gmm
#' @export
topics.textmodel_gmm <- function(x, group = NULL, ...) {

  if (x$k != length(x$label))
    stop("The length of label is invalid")

  if (!is.null(group)) {
    topic <- get_topics(probability(x, group))
  } else {
    if (!is.null(x$cluster)) { # v0.1.0
      topic <- x$cluster
    } else if (!is.null(x$topic)) { # v0.2.x
      topic <- max.col(x$topic.likelihood, ties.method = "first")
    } else {
      topic <- x$topic
    }
    topic <- structure(factor(topic, levels = seq_along(x$label), labels = x$label),
                       names = rownames(x))
  }
  return(topic)
}

#' Extract lost likely topic terms from documents
#' @rdname terms
#' @param x a fitted model or a factor from `GMTM::topics()`.
#' @param n the number of topic words.
#' @param data a [quanteda::dfm] or [quanteda::tokens] from which words are extracted
#'   for each topic.
#' @param filter a character vector of words to be included in the output.
#' @param ... not used.
#' @returns Returns a character matrix with the most distinctive words for each topic.
#' @details
#' To identify topic terms, `data` must be provided along with a fitted model because
#' the information about the frequency of individual words are lost in document vectors.
#'
#' When `x` is a `textmodel_gmm`, `data` is weighted by the documents' probabilities
#' for topics to extract most likely terms. When `x` is `textmodel_kmeans` or a factor,
#' `data` is grouped by topic and weighted by TF-IDF to select the most distinctive
#' words for each topic (known as c-TF-IDF).
#' @export
terms <- function(x, data, n = 10, filter = NULL, ...) {
  UseMethod("terms")
}

#' @method terms textmodel_gmm
#' @export
terms.textmodel_gmm <- function(x, data, n = 10, filter = NULL, ...) {

  data <- dfm(data, tolower = FALSE, remove_padding = TRUE,
              trim = FALSE, verbose = FALSE)
  prob <- probability(x)
  d <- intersect(rownames(data), rownames(prob))
  if (length(d) == 0)
    stop ("data must contain documents on which the model was trained")

  data <- dfm_sort(data) # give frequent words priority
  temp <- as.matrix(t(data[d,]) %*% prob[d,,drop = FALSE])
  names(dimnames(temp)) <- NULL

  if (!is.null(filter))
    temp <- temp[rownames(temp) %in% filter,, drop = FALSE]
  get_terms(temp, n = n)
}

#' @method terms factor
#' @export
terms.factor <- function(x, data, n = 10, filter = NULL, ...) {

  data <- dfm(data, tolower = FALSE, remove_padding = TRUE,
              trim = FALSE, verbose = FALSE)
  d <- intersect(rownames(data), names(x))
  if (length(d) == 0)
    stop ("data must contain documents for which topics were predicted")

  temp <- dfm_group(data[d,], x[d], fill = TRUE, verbose = FALSE)
  temp <- t(as.matrix(dfm_tfidf(temp)))
  names(dimnames(temp)) <- NULL

  if (!is.null(filter))
    temp <- temp[rownames(temp) %in% filter,, drop = FALSE]
  get_terms(temp, n = n)
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
