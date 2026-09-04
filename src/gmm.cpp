#include <RcppArmadillo.h>
#include <chrono>

using namespace arma;
using namespace std;
using namespace Rcpp;

inline std::vector<double> to_vector(const arma::urowvec& v) {
  return arma::conv_to< std::vector<double> >::from(v);
}

// [[Rcpp::export]]
List cpp_gmm(arma::mat &data, arma::mat means,
             int k, bool verbose = false) {

  inplace_trans(data); // vectors are columns


  Rcout << "means.n_rows:" << means.n_rows << "\n";
  Rcout << "means.n_cols:" << means.n_cols << "\n";

  gmm_diag;
  model.reset(data.n_rows, k);
  model.set_means(means);
  //model.means.print("means:");

  bool status = model.learn(data, k, eucl_dist , keep_existing,
                            10, 5, 1e-10, verbose);

  if(status == false) {
    cout << "learning failed" << endl;
  }

  //model.means.print("means:");

  //double  scalar_likelihood = model.log_p( data.col(0)    );
  //rowvec     set_likelihood = model.log_p( data.cols(0,9) );

  //double overall_likelihood = model.avg_log_p(data);
  urowvec cl_eucl = model.assign(data, eucl_dist);
  urowvec cl_prob = model.assign(data, prob_dist);

  arma::mat log_prob(data.n_cols, k, arma::fill::zeros);
  //Rcout << "log_prob.n_rows:" << log_prob.n_rows << "\n";
  //Rcout << "log_prob.n_cols:" << log_prob.n_cols << "\n";

  for (int j = 0; j < k; j++) {
    //Rcout << j << "\n";
    //Rcout << model.log_p(data, j) << "\n";
    log_prob.col(j) = model.log_p(data, j).t();
  }

  //IntegerVector cl_prob_ = arma::conv_to<IntegerVector>::from(cl_prob);

  return List::create(Rcpp::Named("k") = k,
                      Rcpp::Named("cl_eucl") = to_vector(cl_eucl),
                      Rcpp::Named("cl_prob") = to_vector(cl_prob),
                      Rcpp::Named("log_prob") = log_prob,
                      Rcpp::Named("means") = model.means,
                      Rcpp::Named("dcovs") = model.dcovs,
                      Rcpp::Named("hefts") = model.hefts);
}

/*** R
#mat <- matrix(rnorm(100 * 10), ncol = 10)
k <- 20
m <- matrix(rnorm(100 * k), ncol = k)
out <- cpp_gmm(mat, k, means = m, verbose = FALSE)
out$cluster
names(out)
cluster <- max.col(out$log_prob)
# p <- exp(out$log_prob)
# p <- p / rowSums(p)
# hist(p[100,])
# table(cluster)
EBTM:::get_terms(out$cl_eucl, dfmt, min_count = 1)
EBTM:::get_terms(out$cl_prob, dfmt, min_count = 1)
*/
