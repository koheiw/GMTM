get_terms <- function(cluster, data, n = 10, min_count = 1) {

  if (length(cluster) != quanteda::ndoc(data))
    stop("the number of documents do not match")

  data$cluster <- cluster
  data <- quanteda::dfm(data, remove_padding = TRUE)
  data <- quanteda::dfm_group(data, cluster, fill = TRUE) |>
    quanteda::dfm_trim(min_termfreq = min_count) |>
    quanteda::dfm_tfidf()
  apply(data, 1, function(y)
    head(colnames(data)[order(y, decreasing = TRUE)], n)
  )
}

