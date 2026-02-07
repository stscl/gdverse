#include <Rcpp.h>
using namespace Rcpp;

Rcpp::IntegerVector rcpp_which(Rcpp::LogicalVector x){
  Rcpp::IntegerVector v = seq(0,x.size()-1);
  return v[x];
}

bool AnyCommonElementVec2(Rcpp::IntegerVector vec1,
                          Rcpp::IntegerVector vec2) {
  for (int i = 0; i < vec1.size(); ++i) {
    if (vec1[i] == vec2[i]) {
      return true;
    }
  }
  return false;
}

bool AnyCommonElementVecMat(Rcpp::IntegerVector vec1,
                            Rcpp::IntegerMatrix mat1) {
  for (int i = 0; i < mat1.nrow(); ++i) {
    Rcpp::IntegerVector vec2 = mat1(i,_);
    if (AnyCommonElementVec2(vec1,vec2)) {
      return true;
    }
  }
  return false;
}

bool AllCommonElementVec2(Rcpp::IntegerVector vec1,
                          Rcpp::IntegerVector vec2) {
  for (int i = 0; i < vec1.size(); ++i) {
    if (vec1[i] != vec2[i]) {
      return false;
    }
  }
  return true;
}

bool AllCommonElementVecMat(Rcpp::IntegerVector vec1,
                            Rcpp::IntegerMatrix mat1) {
  for (int i = 0; i < mat1.nrow(); ++i) {
    Rcpp::IntegerVector vec2 = mat1(i,_);
    if (!AllCommonElementVec2(vec1,vec2)) {
      return false;
    }
  }
  return true;
}

bool AnyRowCommonElementVecMat(Rcpp::IntegerVector vec1,
                               Rcpp::IntegerMatrix mat1) {
  for (int i = 0; i < mat1.nrow(); ++i) {
    Rcpp::IntegerVector vec2 = mat1(i,_);
    if (AllCommonElementVec2(vec1,vec2)) {
      return true;
    }
  }
  return false;
}

bool AnyRowColCommonElementVecMat(Rcpp::IntegerVector vec1,
                                  Rcpp::IntegerMatrix mat1) {
  for (int i = 0; i < mat1.nrow(); ++i) {
    Rcpp::IntegerVector vec2 = mat1(i,_);
    if (AnyCommonElementVec2(vec1,vec2)) {
      return true;
    }
  }
  return false;
}

Rcpp::IntegerMatrix slice_matrix_rows(Rcpp::IntegerMatrix mat,
                                      Rcpp::IntegerVector rows) {
  Rcpp::IntegerMatrix new_mat(rows.size(), mat.ncol());
  for (int i = 0; i < rows.size(); ++i) {
    for (int j = 0; j < mat.ncol(); ++j) {
      new_mat(i, j) = mat(rows[i], j);
    }
  }
  return new_mat;
}

Rcpp::IntegerMatrix slice_matrix_cols(Rcpp::IntegerMatrix mat,
                                      Rcpp::IntegerVector cols) {
  Rcpp::IntegerMatrix new_mat(mat.nrow(), cols.size());
  for (int i = 0; i < cols.size(); ++i) {
    for (int j = 0; j < mat.nrow(); ++j) {
      new_mat(j, i) = mat(j, cols[i]);
    }
  }
  return new_mat;
}

Rcpp::NumericVector rcpp_log2(Rcpp::NumericVector vec) {
  Rcpp::NumericVector res(vec.size());
  for (int i = 0; i < vec.size(); ++i) {
    res[i] = std::log2(vec[i]);
  }
  return res;
}

bool rcpp_alleuqal(Rcpp::IntegerVector xvec, int x){
  for (int i = 0; i < xvec.size(); ++i) {
    bool xi = xvec[i] == x;
    if (!xi) {
      return false;
    }
  }
  return true;
}

// [[Rcpp::export(rng = false)]]
Rcpp::List SRS_PD(Rcpp::IntegerVector yobs,
                  Rcpp::IntegerMatrix xobs,
                  Rcpp::IntegerMatrix wt){
  Rcpp::NumericVector res(xobs.nrow());
  for (int i = 0; i < xobs.nrow(); ++i){
    Rcpp::IntegerVector wti = wt(i,_);
    wti = rcpp_which(wti != 0);
    double wti_size = wti.size();
    Rcpp::IntegerVector apprx(wti.size());
    for (int n = 0; n < wti.size(); ++n){
      Rcpp::IntegerVector vec1 = xobs(wti[n],_);
      Rcpp::IntegerMatrix mat1 = slice_matrix_rows(xobs,wti[wti!=wti[n]]);
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
  Rcpp::NumericVector pdN = res / Rcpp::sum(res);
  double sepd = -1 * Rcpp::sum(pdN * rcpp_log2(pdN));
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("PD",pd),
                                      Rcpp::Named("SE_PD",sepd));
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::List SRS_MULTIPD(Rcpp::IntegerVector yobs,
                       Rcpp::IntegerMatrix xobs,
                       Rcpp::IntegerMatrix wt){
  Rcpp::NumericVector res(xobs.nrow());
  for (int i = 0; i < xobs.nrow(); ++i){
    Rcpp::IntegerVector wti = wt(i,_);
    wti = rcpp_which(wti != 0);
    double wti_size = wti.size();
    Rcpp::IntegerVector apprx(wti.size());
    for (int n = 0; n < wti.size(); ++n){
      Rcpp::IntegerVector vec1 = xobs(wti[n],_);
      Rcpp::IntegerMatrix mat1 = slice_matrix_rows(xobs,wti[wti!=wti[n]]);
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
  Rcpp::NumericVector pdN = res / Rcpp::sum(res);
  double sepd = -1 * Rcpp::sum(pdN * rcpp_log2(pdN));
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("PD",pd),
                                      Rcpp::Named("SE_PD",sepd));
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector SRS_PDTEST(Rcpp::IntegerVector yobs,
                               Rcpp::IntegerMatrix xobs,
                               Rcpp::IntegerMatrix wt){
  Rcpp::NumericVector res(xobs.nrow());
  for (int i = 0; i < xobs.nrow(); ++i){
    Rcpp::IntegerVector wti = wt(i,_);
    wti = rcpp_which(wti != 0);
    double wti_size = wti.size();
    Rcpp::IntegerVector apprx(wti.size());
    for (int n = 0; n < wti.size(); ++n){
      Rcpp::IntegerVector vec1 = xobs(wti[n],_);
      Rcpp::IntegerMatrix mat1 = slice_matrix_rows(xobs,wti[wti!=wti[n]]);
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
