custom_kmeans <- function(data, k, max_iter = 100) {
  # Convert data frame to matrix for faster calculations
  X <- as.matrix(data)
  n_obs <- nrow(X)

  # Step 1: Initialize K random centroids from the dataset points
  set.seed(42)
  centroids <- X[sample(1:n_obs, k), , drop = FALSE]

  # Initialize vectors to track assignments
  cluster_assignments <- rep(0, n_obs)
  old_assignments <- rep(-1, n_obs)

  # Iteration loop
  for (iter in 1:max_iter) {

    # Step 2: Assign each point to the closest centroid
    for (i in 1:n_obs) {
      # Calculate Euclidean distance from observation i to all centroids
      distances <- apply(centroids, 1, function(c) sqrt(sum((X[i, ] - c)^2)))
      cluster_assignments[i] <- which.min(distances)
    }

    # Check convergence: stop early if assignments didn't change
    if (all(cluster_assignments == old_assignments)) {
      cat("Converged at iteration:", iter, "\n")
      break
    }
    old_assignments <- cluster_assignments

    # Step 3: Recalculate centroids (mean of points in each cluster)
    for (j in 1:k) {
      points_in_cluster <- X[cluster_assignments == j, , drop = FALSE]
      if (nrow(points_in_cluster) > 0) {
        centroids[j, ] <- colMeans(points_in_cluster)
      }
    }
  }

  # Return assignments and final centroids
  return(list(cluster = cluster_assignments, centers = centroids))
}

custom_kmeans_cpp <- function(data, k, max_iter = 100) {
  # Convert data frame to matrix for faster calculations
  X <- t(as.matrix(data))
  n_obs <- ncol(X)

  # Step 1: Initialize K random centroids from the dataset points
  set.seed(42)
  centroids <- X[,sample(1:n_obs, k), drop = FALSE]

  # Initialize vectors to track assignments
  cluster_assignments <- rep(0, n_obs)
  old_assignments <- rep(-1, n_obs)

  # Iteration loop
  for (iter in 1:max_iter) {

    # Step 2: Assign each point to the closest centroid
    d <- proxyC::dist(X, centroids, margin = 2, sparse = FALSE)
    cluster_assignments <- max.col(d * -1)

    # Check convergence: stop early if assignments didn't change
    if (all(cluster_assignments == old_assignments)) {
      cat("Converged at iteration:", iter, "\n")
      break
    }
    old_assignments <- cluster_assignments

    # Step 3: Recalculate centroids (mean of points in each cluster)
    for (j in 1:k) {
      points_in_cluster <- X[,cluster_assignments == j, drop = FALSE]
      if (ncol(points_in_cluster) > 0) {
        centroids[,j] <- rowMeans(points_in_cluster)
      }
    }
  }

  # Return assignments and final centroids
  return(list(cluster = cluster_assignments, centers = centroids))
}

# --- Example Usage ---
df_scaled <- scale(iris[, 1:4])

mat[is.na(mat)] <- 0
microbenchmark::microbenchmark(
  km <- kmeans(mat, 10, algorithm = "Forgy", iter.max = 10),
  #km2 <- custom_kmeans(mat, k = 10),
  km2c <- custom_kmeans_cpp(mat, k = 10, max_iter = 10),
  times = 5
)

profvis::profvis({
  custom_kmeans_cpp(mat, k = 10, max_iter = 10)
})

# Inspect custom output
table(km2$cluster)
get_terms(dfmt, km2$cluster)
get_terms(dfmt, km2c$cluster)

microbenchmark::microbenchmark(
  proxyC::dist(mat[1:100,], mat),
  proxyC::dist(mat, mat[1:100,]),
  times = 10
)
d <- t(proxyC::dist(mat[1:10,], mat))
out <- proxyC::dist(dfmt, sparse = FALSE)
out <- proxyC::simil(dfmt, dfmt[1:100,], sparse = FALSE)
out <- proxyC::dist(mat, mat[1:100,], sparse = FALSE)
proxyC:::getThreads()
