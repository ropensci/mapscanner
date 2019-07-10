#include <set>

#include <Rcpp.h>

const size_t INFINITE_SIZET =  std::numeric_limits <size_t>::max ();

typedef std::pair <int, int> XYPoint;

int findStartPixel (Rcpp::LogicalMatrix image, int startj);

XYPoint nextClockwise (XYPoint p_in, XYPoint p_mid);

Rcpp::DataFrame rcpp_boundary (Rcpp::LogicalMatrix image);
