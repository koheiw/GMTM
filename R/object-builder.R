build_gmm <- function(...) {

  args <- list(...)
  result <- list(
    k = NULL,
    omit = NULL,
    centers = NULL,
    covariance = NULL,
    topic.likelihood = NULL,
    model = NULL,
    model.likelihood = NULL,
    frequency = NULL,
    label = NULL,
    docvars = NULL,
    mode = NULL,
    call = NULL,
    version = utils::packageVersion("GMTM")
  )
  for (m in intersect(names(result), names(args$model)))
    result[m] <- args$model[m]
  for (n in intersect(names(result), names(args)))
    result[n] <- args[n]
  class(result) <- c("textmodel_gmm", "textmodel_gmtm")
  return(result)
}


build_kmeans <- function(...) {

  args <- list(...)
  result <- list(
    k = NULL,
    omit = NULL,
    centers = NULL,
    topic = NULL,
    frequency = NULL,
    label = NULL,
    call = NULL,
    version = utils::packageVersion("GMTM")
  )
  for (m in intersect(names(result), names(args))) {
    result[m] <- args[m]
  }
  class(result) <- c("textmodel_kmeans", "textmodel_gmtm")
  return(result)
}

