build_gmm <- function(...) {

  args <- list(...)
  result <- list(
    k = NULL,
    omit = NULL,
    centers = NULL,
    covariance = NULL,
    cluster = NULL,
    cluster.likelihood = NULL,
    model = NULL,
    model.likelihood = NULL,
    frequency = NULL,
    label = NULL,
    docname = NULL,
    docvars = NULL,
    #type = "document",
    call = NULL,
    version = utils::packageVersion("GMTM")
  )
  for (m in intersect(names(result), names(args))) {
    result[m] <- args[m]
  }
  class(result) <- c("textmodel_gmm", "textmodel_gmtm")
  return(result)
}


build_kmeans <- function(...) {

  args <- list(...)
  result <- list(
    k = NULL,
    omit = NULL,
    centers = NULL,
    cluster = NULL,
    frequency = NULL,
    label = NULL,
    docname = NULL,
    docvars = NULL,
    #type = "document",
    call = NULL,
    version = utils::packageVersion("GMTM")
  )
  for (m in intersect(names(result), names(args))) {
    result[m] <- args[m]
  }
  class(result) <- c("textmodel_kmeans", "textmodel_gmtm")
  return(result)
}

