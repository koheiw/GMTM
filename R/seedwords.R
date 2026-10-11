#' Convert a dictionary to a seed word matrix
#' @param x a [quanteda::dictionary] of seed words.
#' @param model a [wordvector::textmodel_word2vec] object.
#' @param residual the number of unseeded topics.
#' @param levels integers specifying the levels of entries in
#'   a hierarchical dictionary.
#' @details
#' Unseeded topics are labeled "other", but it can be changed via
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
#'                remove_symbols = TRUE, remove_numbers = TRUE) %>%
#'         tokens_remove(stopwords("en"), min_nchar = 2)
#' wov <- textmodel_word2vec(toks, dim = 50)
#'
#' dict <- dictionary(list(eco = "econom*", sec = "securit*", spo = "sport*",
#'                         pol = "politi*", cri = "crime"))
#' seed <- as.seedwords(dict, wov, residual = 2)
as.seedwords <- function(x, model, residual = 0, levels = 1) {

  if (!quanteda::is.dictionary(x))
    stop("x must be a dictionary object")
  if (is.null(model$values$word))
    stop("the model does not have the layer for words")

  residual <- check_integer(residual, min = 0)
  x <- flatten_dictionary(x, levels = levels)
  w <- object2fixed(x, types = rownames(model$values$word),
                    concatenator = model$concatenator)
  v <- structure(unlist_character(w), names = rep(names(w), lengths(w)))
  d <- dfm(as.tokens(split(v, factor(names(v), levels = names(x)))))
  e <- as.textmodel_doc2vec(d, model)
  seed <- e$values$doc
  names(dimnames(seed)) <- NULL
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
