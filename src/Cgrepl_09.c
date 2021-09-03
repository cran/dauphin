#include "dauphin.h"

SEXP Cgrepl_digit(SEXP xx) {
  if (!isString(xx)) {
    error("xx must be a character vector."); // # nocov
  }
  R_xlen_t N = xlength(xx);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    SEXP CX = STRING_ELT(xx, i);
    int n = length(CX);
    const char * x = CHAR(CX);
    bool o = false;
    for (int j = 0; j < n; ++j) {
      if (isdigit(x[j])) {
        o = true;
        break;
      }
    }
    ansp[i] = o;
  }
  UNPROTECT(1);
  return ans;
}



