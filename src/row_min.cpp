#include <Rcpp.h>

#include <algorithm>

// [[Rcpp::export]]
Rcpp::NumericVector row_mins_cpp(const Rcpp::NumericMatrix& mat) {
  int nRows = mat.nrow();
  int nCols = mat.ncol();
  Rcpp::NumericVector mins(nRows);

  for (int i = 0; i < nRows; ++i) {
    double minValue = mat(i, 0);

    for (int j = 1; j < nCols; ++j) {
      minValue = std::min(minValue, mat(i, j));
    }

    mins[i] = minValue;
  }
  return mins;
}