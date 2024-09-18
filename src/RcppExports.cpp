// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// SRS_PD
List SRS_PD(IntegerVector yobs, IntegerMatrix xobs, IntegerMatrix wt);
RcppExport SEXP _gdverse_SRS_PD(SEXP yobsSEXP, SEXP xobsSEXP, SEXP wtSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type yobs(yobsSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type xobs(xobsSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type wt(wtSEXP);
    rcpp_result_gen = Rcpp::wrap(SRS_PD(yobs, xobs, wt));
    return rcpp_result_gen;
END_RCPP
}
// SRS_MULTIPD
List SRS_MULTIPD(IntegerVector yobs, IntegerMatrix xobs, IntegerMatrix wt);
RcppExport SEXP _gdverse_SRS_MULTIPD(SEXP yobsSEXP, SEXP xobsSEXP, SEXP wtSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type yobs(yobsSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type xobs(xobsSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type wt(wtSEXP);
    rcpp_result_gen = Rcpp::wrap(SRS_MULTIPD(yobs, xobs, wt));
    return rcpp_result_gen;
END_RCPP
}
// SRS_PDTEST
NumericVector SRS_PDTEST(IntegerVector yobs, IntegerMatrix xobs, IntegerMatrix wt);
RcppExport SEXP _gdverse_SRS_PDTEST(SEXP yobsSEXP, SEXP xobsSEXP, SEXP wtSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type yobs(yobsSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type xobs(xobsSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type wt(wtSEXP);
    rcpp_result_gen = Rcpp::wrap(SRS_PDTEST(yobs, xobs, wt));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_gdverse_SRS_PD", (DL_FUNC) &_gdverse_SRS_PD, 3},
    {"_gdverse_SRS_MULTIPD", (DL_FUNC) &_gdverse_SRS_MULTIPD, 3},
    {"_gdverse_SRS_PDTEST", (DL_FUNC) &_gdverse_SRS_PDTEST, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_gdverse(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}