// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_boundary
Rcpp::DataFrame rcpp_boundary(Rcpp::LogicalMatrix image);
RcppExport SEXP _mapscanner_rcpp_boundary(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_boundary(image));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_components
Rcpp::IntegerMatrix rcpp_components(Rcpp::LogicalMatrix image);
RcppExport SEXP _mapscanner_rcpp_components(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_components(image));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_edge_thin
int rcpp_edge_thin(Rcpp::LogicalMatrix image);
RcppExport SEXP _mapscanner_rcpp_edge_thin(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_edge_thin(image));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mapscanner_rcpp_boundary", (DL_FUNC) &_mapscanner_rcpp_boundary, 1},
    {"_mapscanner_rcpp_components", (DL_FUNC) &_mapscanner_rcpp_components, 1},
    {"_mapscanner_rcpp_edge_thin", (DL_FUNC) &_mapscanner_rcpp_edge_thin, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_mapscanner(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
