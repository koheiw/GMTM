#' @importFrom utils head
#' @import quanteda
get_terms <- function(topic, data, n = 10, min_count = 1) {

  if (length(topic) != ndoc(data))
    stop("the number of documents do not match")

  data$topic <- topic
  data <- dfm(data, remove_padding = TRUE)
  data <- dfm_group(data, topic, fill = TRUE)
  data <- dfm_trim(data, min_termfreq = min_count)
  data <- dfm_tfidf(data)
  result <- apply(data, 1, function(y)
    head(colnames(data)[order(y, decreasing = TRUE)], n)
  )
  dimnames(result) <- list(NULL, colnames(result))
  return(result)
}

#' Convert a dictionary to a seed word matrix
#' @param x a [quanteda::dictionary] of seed words.
#' @param model a [wordvector::textmodel_word2vec] object.
#' @param residual the number of unseeded topics.
#' @param levels integers specifying the levels of entries in
#'   a hierarchical dictionary.
#' @details
#' Unseeded topics are named "other" but can be changed via
#' options("GMTM.residual.name").
#' @export
#' @import quanteda wordvector
#' @returns Returns a seed word matrix.
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
#'
#' dict <- dictionary(list(eco = "econom*", sec = "securit*", spo = "sport*",
#'                         pol = "politi*", cri = "crime"))
#' seed <- as.seedwords(dict, wov, residual = 2)
as.seedwords <- function(x, model, residual = 0, levels = 1) {

  if (!quanteda::is.dictionary(x))
    stop("x must be a dictionary object")
  residual <- check_integer(residual, min = 0)
  x <- flatten_dictionary(x, levels = levels)
  v <- unlist(object2fixed(x, types = rownames(model$values$word),
                           match_pattern = "single"))
  d <- dfm(as.tokens(split(v, factor(names(v), levels = names(x)))))
  e <- as.textmodel_doc2vec(d, model)
  seed <- e$values$doc
  if (residual == 0)
    return(seed)

  other <- get_centers(residual, ncol(seed))
  if (residual == 1) {
    rownames(other) <- getOption("GMTM.residual.name", "other")
  } else {
    rownames(other) <- paste0(getOption("GMTM.residual.name", "other"),
                              seq_len(residual))
  }
  rbind(seed, other)
}

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

