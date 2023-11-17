#include <R.h>
#include <Rinternals.h>

SEXP row_mins_(SEXP mat, SEXP nRows, SEXP nCols) {
  int nrows = INTEGER(nRows)[0];
  int ncols = INTEGER(nCols)[0];
  SEXP mins = PROTECT(allocVector(INTSXP, nrows));
  int *pmat = INTEGER(mat);
  int *pmins = INTEGER(mins);

  for (int i = 0; i < nrows; i++) {
    int minVal = pmat[i];
    for (int j = 1; j < ncols; j++) {
      int currentVal = pmat[i + j * nrows];
      if (currentVal < minVal) {
        minVal = currentVal;
      }
    }
    pmins[i] = minVal;
  }

  UNPROTECT(1);
  return mins;
}
