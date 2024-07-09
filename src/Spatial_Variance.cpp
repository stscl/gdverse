#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

double matrix_sum(NumericMatrix mat) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  double out = 0;
  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      out += mat(i, j);
    }
  }
  return out;
}

// [[Rcpp::export]]
double spatial_variance(NumericVector x, NumericMatrix wt) {
  int n = x.size();
  double out = 0;
  for(int i = 0; i < n; ++i) {
    for(int j = 0; j < n; ++j) {
      double w = wt(i, j);
      out += w * pow((x[i]-x[j]),2) / 2;
    }
  }
  out = out / matrix_sum(wt);
  return out;
}
