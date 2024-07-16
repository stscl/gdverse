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

bool AllCommonElementVec2(IntegerVector vec1, IntegerVector vec2) {
  for (int i = 0; i < vec1.size(); ++i) {
    if (vec1[i] != vec2[i]) {
      return false;
    }
  }
  return true;
}

bool AllCommonElementVecMat(IntegerVector vec1, IntegerMatrix mat1) {
  for (int i = 0; i < mat1.nrow(); ++i) {
    IntegerVector vec2 = mat1(i,_);
    if (!AllCommonElementVec2(vec1,vec2)) {
      return false;
    }
  }
  return true;
}

bool AnyRowCommonElementVecMat(IntegerVector vec1, IntegerMatrix mat1) {
  for (int i = 0; i < mat1.nrow(); ++i) {
    IntegerVector vec2 = mat1(i,_);
    if (AllCommonElementVec2(vec1,vec2)) {
      return true;
    }
  }
  return false;
}

bool AnyRowColCommonElementVecMat(IntegerVector vec1, IntegerMatrix mat1) {
  for (int i = 0; i < mat1.nrow(); ++i) {
    IntegerVector vec2 = mat1(i,_);
    if (AnyCommonElementVec2(vec1,vec2)) {
      return true;
    }
  }
  return false;
}

IntegerMatrix slice_matrix_rows(IntegerMatrix mat, IntegerVector rows) {
  IntegerMatrix new_mat(rows.size(), mat.ncol());
  for (int i = 0; i < rows.size(); ++i) {
    for (int j = 0; j < mat.ncol(); ++j) {
      new_mat(i, j) = mat(rows[i], j);
    }
  }
  return new_mat;
}

IntegerMatrix slice_matrix_cols(IntegerMatrix mat, IntegerVector cols) {
  IntegerMatrix new_mat(mat.nrow(), cols.size());
  for (int i = 0; i < cols.size(); ++i) {
    for (int j = 0; j < mat.nrow(); ++j) {
      new_mat(j, i) = mat(j, cols[i]);
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

bool rcpp_alleuqal(IntegerVector xvec, int x){
  for (int i = 0; i < xvec.size(); ++i) {
    bool xi = xvec[i] == x;
    if (!xi) {
      return false;
    }
  }
  return true;
}

// [[Rcpp::export]]

List SRS_PD(IntegerVector yobs,
            IntegerMatrix xobs,
            IntegerMatrix wt){
  NumericVector res(xobs.nrow());
  for (int i = 0; i < xobs.nrow(); ++i){
    IntegerVector wti = wt(i,_);
    wti = rcpp_which(wti != 0);
    double wti_size = wti.size();
    IntegerVector apprx(wti.size());
    for (int n = 0; n < wti.size(); ++n){
      IntegerVector vec1 = xobs(wti[n],_);
      IntegerMatrix mat1 = slice_matrix_rows(xobs,wti[wti!=wti[n]]);
      if (AnyRowCommonElementVecMat(vec1,mat1) and (yobs[i] == yobs[n])){
        apprx[n] = 1;
      }
    }
    if (rcpp_alleuqal(apprx,0)) {
      // When doing division operations in cpp, be careful to set at least one element to double
      res[i] = 1.0 / wti_size;
    } else {
      apprx = apprx[apprx != 0];
      res[i] = apprx.size() / wti_size;
    }
  }
  // Rcout << "res: " << res << "\n";
  double pd = Rcpp::sum(res) / xobs.nrow();
  NumericVector pdN = res / Rcpp::sum(res);
  double sepd = -1 * Rcpp::sum(pdN * rcpp_log2(pdN));
  List out = List::create(Named("PD",pd),
                          Named("SE_PD",sepd));
  return out;
}

// [[Rcpp::export]]

List SRS_MULTIPD(IntegerVector yobs,
                 IntegerMatrix xobs,
                 IntegerMatrix wt){
  NumericVector res(xobs.nrow());
  for (int i = 0; i < xobs.nrow(); ++i){
    IntegerVector wti = wt(i,_);
    wti = rcpp_which(wti != 0);
    double wti_size = wti.size();
    IntegerVector apprx(wti.size());
    for (int n = 0; n < wti.size(); ++n){
      IntegerVector vec1 = xobs(wti[n],_);
      IntegerMatrix mat1 = slice_matrix_rows(xobs,wti[wti!=wti[n]]);
      if (AnyRowColCommonElementVecMat(vec1,mat1) and (yobs[i] == yobs[n])){
        apprx[n] = 1;
      }
    }
    if (rcpp_alleuqal(apprx,0)) {
      res[i] = 1.0 / wti_size;
    } else {
      apprx = apprx[apprx != 0];
      res[i] = apprx.size() / wti_size;
    }
  }
  // Rcout << "res: " << res << "\n";
  double pd = Rcpp::sum(res) / xobs.nrow();
  NumericVector pdN = res / Rcpp::sum(res);
  double sepd = -1 * Rcpp::sum(pdN * rcpp_log2(pdN));
  List out = List::create(Named("PD",pd),
                          Named("SE_PD",sepd));
  return out;
}

// [[Rcpp::export]]

NumericVector SRS_PDTEST(IntegerVector yobs,
                         IntegerMatrix xobs,
                         IntegerMatrix wt){
  NumericVector res(xobs.nrow());
  for (int i = 0; i < xobs.nrow(); ++i){
    IntegerVector wti = wt(i,_);
    wti = rcpp_which(wti != 0);
    double wti_size = wti.size();
    IntegerVector apprx(wti.size());
    for (int n = 0; n < wti.size(); ++n){
      IntegerVector vec1 = xobs(wti[n],_);
      IntegerMatrix mat1 = slice_matrix_rows(xobs,wti[wti!=wti[n]]);
      if (AnyRowCommonElementVecMat(vec1,mat1) and (yobs[i] == yobs[n])){
        apprx[n] = 1;
      }
    }
    if (rcpp_alleuqal(apprx,0)) {
      res[i] = 1.0 / wti_size;
    } else {
      apprx = apprx[apprx != 0];
      res[i] = apprx.size() / wti_size;
    }
  }
  return res;
}
