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

<<<<<<< HEAD

#' dict <- dictionary(list(eco = "econom*", sec = "securit*", spo = "sport*",
#'                         pol = "politi*", cri = "crime"))
#' as.seedwords(dict, wov)

as.seedwords <- function(dictionary, model) {
  v <- unlist(object2fixed(dictionary, types = rownames(model$values$word)))
  d <- dfm(as.tokens(split(v, factor(names(v), levels = names(dictionary)))))
  e <- as.textmodel_doc2vec(d, model)
  e$values$doc
}

=======
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
>>>>>>> main
