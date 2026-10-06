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
    label = NULL
  )
  if (arg$mode == "document") {
    result <- c(result,
                mode = "document",
                docname = NULL,
                docvars = NULL)
  } else if (arg$mode == "word") {
    result <- c(result,
                mode = "word",
                featname = NULL)
  }
  result <- c(result,
              call = NULL,
              version = utils::packageVersion("GMTM"))

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
    cluster = NULL,
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

