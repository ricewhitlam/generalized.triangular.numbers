// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// generalized_triangular_numbers_point
ComplexVector generalized_triangular_numbers_point(const ComplexVector& z, const int& n);
RcppExport SEXP _generalized_triangular_numbers_generalized_triangular_numbers_point(SEXP zSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const ComplexVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< const int& >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generalized_triangular_numbers_point(z, n));
    return rcpp_result_gen;
END_RCPP
}
// generalized_triangular_numbers_sequence
ComplexVector generalized_triangular_numbers_sequence(const ComplexVector& z, const int& n);
RcppExport SEXP _generalized_triangular_numbers_generalized_triangular_numbers_sequence(SEXP zSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const ComplexVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< const int& >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generalized_triangular_numbers_sequence(z, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_generalized_triangular_numbers_generalized_triangular_numbers_point", (DL_FUNC) &_generalized_triangular_numbers_generalized_triangular_numbers_point, 2},
    {"_generalized_triangular_numbers_generalized_triangular_numbers_sequence", (DL_FUNC) &_generalized_triangular_numbers_generalized_triangular_numbers_sequence, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_generalized_triangular_numbers(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}