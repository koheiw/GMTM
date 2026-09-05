#include <RcppArmadillo.h>
#include <chrono>

using namespace arma;
using namespace std;
using namespace Rcpp;

inline std::vector<double> to_vector(const arma::urowvec& v) {
  return arma::conv_to< std::vector<double> >::from(v);
}

// [[Rcpp::export]]
List cpp_kmeans(arma::mat &data, int k, arma::mat &means,
                int iter = 10, bool verbose = false) {

  inplace_trans(data); // vectors are columns

  bool status = kmeans(means, data, k, keep_existing, iter, verbose);

  if (!status)
    throw std::runtime_error("Training of k-means failed");

  return List::create(Rcpp::Named("k") = k,
                      Rcpp::Named("centers") = means);
}

/*** R
#mat <- matrix(rnorm(10000 * 10), ncol = 10)
k <- 20
m <- matrix(rnorm(ncol(mat) * k), ncol = k)
out <- cpp_kmeans(mat, k, means = m, verbose = TRUE)
out$cluster <- max.col(-1 * proxyC::dist(mat, t(out$centers), 1, sparse = FALSE) ^ 2)
GMTM:::get_terms(out$cluster, dfmt, min_count = 1)
*/
