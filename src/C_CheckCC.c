#include "dauphin.h"

bool non_au_prefix(char x0, char x1, char x2) {
  if (x0 == '+') {
    return x1 != '6' || x2 != '1';
  }
  return x0 == '+' ? (x1 != '6' || x2 != '1') : false;
}

SEXP C_CCRequired(SEXP x, SEXP ignore_calling_code) {
  // # nocov start
  if (!isString(x)) {
    error("`x` was type '%s', but must be a character vector.", type2char(TYPEOF(x)));
  }
  if (!isLogical(ignore_calling_code) || xlength(ignore_calling_code) != 1) {
    error("`ignore_calling_code` was a '%s' of length > 1. Change `ignore_calling_code` to TRUE, FALSE, or NA.",
          type2char(TYPEOF(ignore_calling_code)));
  }
  // # nocov end
  int icc = asLogical(ignore_calling_code);
  if (icc != NA_INTEGER) {
    return ScalarLogical(!icc);
  }
  R_xlen_t N = xlength(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    int n = length(STRING_ELT(x, i));
    if (n <= 3) {
      continue;
    }
    const char * xi = CHAR(STRING_ELT(x, i));
    char x0 = xi[0];
    char x1 = xi[1];
    char x2 = xi[2];
    if (non_au_prefix(x0, x1, x2)) {
      return ScalarLogical(1);
    }
  }
  return ScalarLogical(0);
}






