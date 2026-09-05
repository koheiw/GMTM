#include <RcppArmadillo.h>
#include <chrono>

using namespace arma;
using namespace std;
using namespace Rcpp;

inline std::vector<double> to_vector(const arma::urowvec& v) {
  return arma::conv_to< std::vector<double> >::from(v);
}

// [[Rcpp::export]]
List cpp_gmm(arma::mat &data, int k, arma::mat means,
             int mode = 1, int iter_km = 10, int iter_em = 10,
             bool verbose = false) {

  inplace_trans(data); // vectors are columns

  //Rcout << "means.n_rows:" << means.n_rows << "\n";
  //Rcout << "means.n_cols:" << means.n_cols << "\n";
  //model.means.print("means:");

  gmm_diag model;
  model.reset(data.n_rows, k);
  model.set_means(means);

  bool status = false;
  if (mode == 1) {
    status = model.learn(data, k, eucl_dist, keep_existing,
                         iter_km, iter_em, 1e-10, verbose);
  } else {
    status = model.learn(data, k, maha_dist, keep_existing,
                         iter_km, iter_em, 1e-10, verbose);
  }

  if (!status)
    throw std::runtime_error("Training of GMM failed");

  //model.means.print("means:");

  //double  scalar_likelihood = model.log_p( data.col(0)    );
  //rowvec     set_likelihood = model.log_p( data.cols(0,9) );

  //double overall_likelihood = model.avg_log_p(data);
  urowvec cl_eucl = model.assign(data, eucl_dist);
  urowvec cl_prob = model.assign(data, prob_dist);

  arma::mat log_prob(data.n_cols, k, arma::fill::zeros);
  for (int j = 0; j < k; j++) {
    log_prob.col(j) = model.log_p(data, j).t();
  }
  return List::create(Rcpp::Named("k") = k,
                      Rcpp::Named("centers") = model.means,
                      //Rcpp::Named("dcovs") = model.dcovs,
                      //Rcpp::Named("hefts") = model.hefts
                      Rcpp::Named("likelihood") = exp(log_prob),
                      //Rcpp::Named("cluster") = to_vector(cl_eucl),
                      Rcpp::Named("cluster") = to_vector(cl_prob));
}

/*** R
#mat <- matrix(rnorm(100 * 10), ncol = 10)
k <- 20
m <- matrix(rnorm(100 * k), ncol = k)
out <- cpp_gmm(mat, k, means = m, verbose = FALSE)
GMTM:::get_terms(out$cluster, dfmt, min_count = 1)
*/
