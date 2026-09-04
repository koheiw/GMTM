get_terms <- function(topic, data, n = 10, min_count = 1) {

  if (length(topic) != quanteda::ndoc(data))
    stop("the number of documents do not match")

  data$topic <- topic
  data <- quanteda::dfm(data, remove_padding = TRUE)
  data <- quanteda::dfm_group(data, topic, fill = TRUE)
  data <- quanteda::dfm_trim(data, min_termfreq = min_count)
  data <- quanteda::dfm_tfidf(data)
  result <- apply(data, 1, function(y)
    head(colnames(data)[order(y, decreasing = TRUE)], n)
  )
  dimnames(result) <- list(NULL, colnames(result))
  return(result)
}

