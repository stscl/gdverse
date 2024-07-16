#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

IntegerVector rcpp_which(LogicalVector x){
  IntegerVector v = seq(0,x.size()-1);
  return v[x];
}

bool AnyCommonElementVec2(IntegerVector vec1, IntegerVector vec2) {
  for (int i = 0; i < vec1.size(); ++i) {
    if (vec1[i] == vec2[i]) {
      return true;
    }
  }
  return false;
}

bool AnyCommonElementVecMat(IntegerVector vec1, IntegerMatrix mat1) {
  for (int i = 0; i < mat1.nrow(); ++i) {
    IntegerVector vec2 = mat1(i,_);
    if (AnyCommonElementVec2(vec1,vec2)) {
      return true;
    }
  }
  return false;
}

IntegerMatrix slice_matrix_rows(IntegerMatrix mat, IntegerVector rows) {
  int new_nrow = rows.size();
  int ncol = mat.ncol();
  IntegerMatrix new_mat(new_nrow, ncol);
  for (int i = 0; i < new_nrow; ++i) {
    int row_index = rows[i];
    for (int j = 0; j < ncol; ++j) {
      new_mat(i, j) = mat(row_index, j);
    }
  }
  return new_mat;
}

IntegerMatrix slice_matrix_cols(IntegerMatrix mat, IntegerVector cols) {
  int new_ncol = cols.size();
  int nrow = mat.nrow();
  IntegerMatrix new_mat(nrow, new_ncol);
  for (int i = 0; i < new_ncol; ++i) {
    int col_index = cols[i];
    for (int j = 0; j < nrow; ++j) {
      new_mat(j, i) = mat(j, col_index);
    }
  }
  return new_mat;
}

NumericVector rcpp_log2(NumericVector vec) {
  NumericVector res(vec.size());
  for (int i = 0; i < vec.size(); ++i) {
    res[i] = std::log2(vec[i]);
  }
  return res;
}

// [[Rcpp::export]]

List SRS_PD(IntegerMatrix xobs,
            IntegerMatrix wt){
  NumericVector res(xobs.nrow());
  for (int i = 0; i < xobs.nrow(); ++i){
    IntegerVector wti = wt(i,_);
    wti = rcpp_which(wti != 0);
    IntegerVector apprx(wti.size());
    IntegerMatrix mat1 = slice_matrix_rows(xobs,wti);
    for (int n = 0; n < wti.size(); ++n){
      IntegerVector vec1 = xobs(wti[n],_);
      if (AnyCommonElementVecMat(vec1,mat1)){
        apprx[n] = 1;
      }
    }
    apprx = apprx[apprx != 0];
    res[i] = apprx.size() / wti.size();
  }
  double pd = Rcpp::sum(res) / xobs.nrow();
  NumericVector pdN = res / Rcpp::sum(res);
  double sepd = -1 * Rcpp::sum(pdN * rcpp_log2(pdN));
  List out = List::create(Named("PD",pd),
                          Named("SE_PD",sepd));
  return out;
}

// [[Rcpp::export]]

double SRSFactor_P(IntegerMatrix xobs,
                   IntegerMatrix wt){
  NumericVector res(xobs.nrow());
  for (int i = 0; i < xobs.nrow(); ++i){
    IntegerVector wti = wt(i,_);
    wti = rcpp_which(wti != 0);
    IntegerVector apprx(wti.size());
    IntegerMatrix mat1 = slice_matrix_rows(xobs,wti);
    for (int n = 0; n <= wti.size(); ++n){
      IntegerVector vec1 = xobs(wti[n],_);
      if (AnyCommonElementVecMat(vec1,mat1)){
        apprx[n] = 1;
      }
    }
    apprx = rcpp_which(apprx == 1);
    res[i] = apprx.size() / wti.size();
  }
  return Rcpp::mean(res);
}
