// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// TotalDelay
List TotalDelay(DataFrame SepTimes, DataFrame FlightsScheduled, IntegerVector sequence);
RcppExport SEXP _airops_TotalDelay(SEXP SepTimesSEXP, SEXP FlightsScheduledSEXP, SEXP sequenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type SepTimes(SepTimesSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type FlightsScheduled(FlightsScheduledSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type sequence(sequenceSEXP);
    rcpp_result_gen = Rcpp::wrap(TotalDelay(SepTimes, FlightsScheduled, sequence));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_airops_TotalDelay", (DL_FUNC) &_airops_TotalDelay, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_airops(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
