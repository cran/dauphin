#include "dauphin.h"


bool intIsMobRange(unsigned int ux) {
  ux -= 400000000;
  return ux < 100000000;
}

bool intIsAusRange(unsigned int ux, bool mob_ok) {
  // true && purely for alignment
  return
  true &&
    ux >= 100000000 &&
    ux < 1000000000 &&
    (mob_ok ||
    ux <  400000000 ||
    ux >= 500000000);
}

unsigned int char2number(char x) {
  return isdigit(x) ? (x - '0') : 0;
}

// List of Calling codes
#define N_CALLING_CODES 228
const unsigned int CC[256] = {61, 0, 1, 7, 20, 27, 30, 31, 32, 33, 34, 36, 39, 40, 41, 43,
                              44, 45, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 57, 58, 60,    62,
                              63, 64, 65, 66, 81, 82, 84, 86, 90, 91, 92, 93, 94, 95, 98, 211,
                              212, 213, 216, 218, 220, 221, 222, 223, 224, 225, 226, 227, 228,
                              229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241,
                              242, 243, 244, 245, 246, 248, 249, 250, 251, 252, 253, 254, 255,
                              256, 257, 258, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269,
                              290, 291, 297, 298, 299, 350, 351, 352, 353, 354, 355, 356, 357,
                              358, 359, 370, 371, 372, 373, 374, 375, 376, 377, 378, 380, 381,
                              382, 385, 386, 387, 389, 420, 421, 423, 500, 501, 502, 503, 504,
                              505, 506, 507, 508, 509, 590, 591, 592, 593, 594, 595, 596, 597,
                              598, 599, 670, 672, 673, 674, 675, 676, 677, 678, 679, 680, 681,
                              682, 683, 685, 686, 687, 688, 689, 690, 691, 692, 850, 852, 853,
                              855, 856, 870, 880, 886, 960, 961, 962, 963, 964, 965, 966, 967,
                              968, 970, 971, 972, 973, 974, 975, 976, 977, 992, 993, 994, 995,
                              996, 998, 1242, 1246, 1264, 1268, 1284, 1340, 1345, 1441, 1473,
                              1649, 1664, 1670, 1671, 1684, 1721, 1758, 1767, 1784, 1868, 1869,
                              1876, 3906, 9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999,
                              9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999,
                              9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999};

// converts the 'integer' version of the calling code (i.e. the 'real' calling code)
//
unsigned char cc2uc(unsigned int x) {
  if (x == 61) {
    return 0;
  }
  int j = 0;
  if (x >= CC[j + 128]) j += 128;
  if (x >= CC[j + 64]) j += 64;
  if (x >= CC[j + 32]) j += 32;
  if (x >= CC[j + 16]) j += 16;
  if (x >= CC[j + 8]) j += 8;
  if (x >= CC[j + 4]) j += 4;
  if (x >= CC[j + 2]) j += 2;
  if (x >= CC[j + 1]) j += 1;
  return j;
}

SEXP EncodeIntCC(SEXP x) {
  if (!isInteger(x)) {
    return x; // # nocov
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * restrict ansp = RAW(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = cc2uc(xp[i]);
  }
  UNPROTECT(1);
  return ans;
}

SEXP DecodeRawCC(SEXP x) {
  if (TYPEOF(x) != RAWSXP) {
    return x; // # nocov
  }
  R_xlen_t N = xlength(x);
  const unsigned char * xp = RAW(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int j = xp[i];
    ansp[i] = CC[j];
  }
  UNPROTECT(1);
  return ans;;
}


SEXP Cgsub_09(SEXP xx) {
  if (!isString(xx)) {
    error("xx was type '%s' but must be a character vector.", type2char(TYPEOF(xx))); // # nocov
  }
  R_xlen_t N = xlength(xx);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    SEXP CX = STRING_ELT(xx, i);
    if (CX == NA_STRING) {
      // SET_STRING_ELT(ans, i, CX);
      ansp[i] = NA_INTEGER;
      continue;
    }
    int n = length(CX);
    const char * x = CHAR(CX);
    uint64_t o = 0;
    uint64_t ten = 1;
    for (int j = n - 1; j >= 0; --j) {
      char xj = x[j];
      if (isdigit(xj)) {
        int oj = char2number(xj);
        o += ten * oj;
        ten *= 10;
      } else {
        if (xj != ' ' && xj != '-' && xj != ',') {
          // we continue the number if it's a comma or dash or space etc
          o = 0;
          ten = 1;
        }
      }
    }
    ansp[i] = (unsigned int)o;
  }
  UNPROTECT(1);
  return ans;
}



int extract_au_mobile(const char * x, int n) {
  if (n < 9) {
    return NA_INTEGER;
  }
  unsigned int o = 0;
  if (n <= 10) {
    int j1 = x[0] == '0';
    for (int j = j1; j < n; ++j) {
      char xj = x[j];
      if (!isdigit(xj)) {
        return NA_INTEGER;
      }
      o = (o << 1) + (o << 3);
      o += xj - '0';
    }
    return intIsMobRange(o) ? o : NA_INTEGER;
  }

  int j1 = 0;
  while (j1 < n && !isdigit(x[j1]) && x[j1] != '+') {
    ++j1;
  }
  if (j1 + 9 >= n) {
    return NA_INTEGER;
  }
  if (x[j1] == '+') {
    if (x[j1 + 1] != '6') {
      return NA_INTEGER;
    }
    ++j1;
  }
  if (x[j1] == '6') {
    ++j1;
  }
  if (x[j1] == '1') {
    ++j1;
  }
  if (x[j1] == ' ') {
    ++j1;
  }
  if (x[j1] == '0') {
    ++j1;
  }

  if (x[j1] == '4') {
    o = 4;
    ++j1;
    for (int j = j1; j < n; ++j) {
      char xj = x[j];
      if (!isdigit(xj)) {
        // spaces are ok between digits
        if (xj == ' ') {
          continue;
        }
        // if it's not a digit or space, possibly extraneous text
        // hopefully the mobile is formed but we shouldn't add digits
        // trailing this text
        break;
      }
      // Same as o *= 10 but for unsigned int to avoid overflow
      o = (o << 1) + (o << 3);
      o += xj - '0';
    }
    return intIsMobRange(o) ? o : NA_INTEGER;
  }
  return NA_INTEGER;
}


SEXP CStandardMobile(SEXP xx) {
  if (isInteger(xx)) {
    return xx; // # nocov
  }
  if (!isString(xx)) {
    error("`mob` was type '%s' but must be type character", type2char(TYPEOF(xx))); // # nocov
  }
  R_xlen_t N = xlength(xx);
  const SEXP * xp = STRING_PTR(xx);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  SEXP Int = PROTECT(allocVector(RAWSXP, N));
  int * restrict ansp = INTEGER(ans);
  unsigned char * restrict intp = RAW(Int);
  for (R_xlen_t i = 0; i < N; ++i) {
    SEXP CX = xp[i];
    int n = length(CX);
    intp[i] = 61;
    ansp[i] = NA_INTEGER;

    const char * x = CHAR(CX);
    int au_mob = extract_au_mobile(x, n);
    if (au_mob > 0) {
      ansp[i] = au_mob;
      intp[i] = cc2uc(61);
      continue;
    }
    unsigned int intl_cd = 0;
    unsigned int intl_mob = 0;
    int j1 = 0;
    while (j1 < n && x[j1] != '+') {
      ++j1;
    }
    if (j1 == n) {
      continue;
    }
    while (++j1 < n) {
      char xj = x[j1];
      if (!isdigit(xj) || intl_cd >= 256) {
        break;
      }
      intl_cd = (intl_cd << 1) + (intl_cd << 3);
      intl_cd += xj - '0';
    }
    // after intl code
    while (++j1 < n) {
      char xj = x[j1];
      if (!isdigit(xj)) {
        if (xj != '-' && xj != ' ') {
          break;
        }
        continue;
      }
      intl_mob = (intl_mob << 1) + (intl_mob << 3);
      intl_mob += xj - '0';
    }
    intp[i] = cc2uc(intl_cd);
    ansp[i] = (intl_mob > 1e7 && intl_mob < INT_MAX) ? intl_mob : NA_INTEGER;


  }
  SEXP List = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(List, 0, ans);
  SET_VECTOR_ELT(List, 1, Int);
  UNPROTECT(3);
  return List;
}

int extract_landline(const char * x, int n, unsigned int area_cd) {
  if (n < 8) {
    return NA_INTEGER;
  }

  if (n == 8) {
    unsigned int o = atoi(x);
    o += area_cd;
    return intIsAusRange(o, true) ? o : NA_INTEGER;
  }
  if (n == 9) {
    // "9876 5432"
    // or
    // "398765432"
    // Parse then if a space then add area_cd, otherwise the first digit
    // ought to be the area code
    bool has_space = false;
    unsigned int o = 0;
    for (int j = 0; j < n; ++j) {
      char xj = x[j];
      if (!isdigit(xj)) {
        // spaces are ok between digits (but only once)
        if (xj == ' ') {
          has_space = true;
          continue;
        }
        // if it's not a digit or space, possibly extraneous text
        // hopefully the mobile is formed but we shouldn't add digits
        // trailing this text
        break;
      }
      // Same as o *= 10 but for unsigned int to avoid overflow
      o = (o << 1) + (o << 3);
      o += xj - '0';
    }
    if (has_space) {
      o += area_cd;
    }
    return intIsAusRange(o, true) ? o : NA_INTEGER;
  }
  if (n == 10) {
    unsigned int o = 0;

    for (int j = 0; j < n; ++j) {
      char xj = x[j];
      if (!isdigit(xj)) {
        // spaces are ok between digits
        if (xj == ' ') {
          continue;
        }
        // if it's not a digit or space, possibly extraneous text
        // hopefully the mobile is formed but we shouldn't add digits
        // trailing this text
        break;
      }
      // Same as o *= 10 but for unsigned int to avoid overflow
      o = (o << 1) + (o << 3);
      o += xj - '0';
    }
    if (o < 1e8 && o > 1e7) {
      o += area_cd;
    }
    return intIsAusRange(o, true) ? o : NA_INTEGER;
  }

  if (x[0] == '(' && x[3] == ')') {

    unsigned int o = isdigit(x[2]) ? x[2] - '0' : (area_cd / 1e8);

    for (int j = 4; j < n; ++j) {
      char xj = x[j];
      if (!isdigit(xj)) {
        // spaces are ok between digits
        if (xj == ' ') {
          continue;
        }
        // if it's not a digit or space, possibly extraneous text
        // hopefully the mobile is formed but we shouldn't add digits
        // trailing this text
        break;
      }
      // Same as o *= 10 but for unsigned int to avoid overflow
      o = (o << 1) + (o << 3);
      o += xj - '0';
    }
    return intIsAusRange(o, false) ? o : NA_INTEGER;
  }

  unsigned int ten = 1;
  unsigned int o = 0;
  for (int j = n - 1; j >= 0; --j) {
    char xj = x[j];
    if (o > 1e7) {
      if (isdigit(xj)) {
        o += ten * (x[j] - '0');
        break;
      }
      o += area_cd;
      break;
    }

    if (isdigit(xj)) {
      o += ten * (x[j] - '0');
      ten *= 10;
    } else {
      if (xj != ' ') {
        return NA_INTEGER;
      }
    }
  }
  return intIsAusRange(o, false) ? o : NA_INTEGER;

}



SEXP C_DauphinLandline(SEXP xx, SEXP AreaCd) {
  const unsigned int area_cd = asInteger(AreaCd);
  if (!intIsAusRange(area_cd, false)) {
    error("`area_cd = %u` which is not a permitted area code.", area_cd); // # nocov
  }
  R_xlen_t N = xlength(xx);
  if (!isString(xx)) {
    return xx; // # nocov
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  const SEXP * xp = STRING_PTR(xx);
  for (R_xlen_t i = 0; i < N; ++i) {
    int n = length(xp[i]);
    ansp[i] = NA_INTEGER;
    if (n >= 25 && n <= 27) {
      //  Mobile Number Not Provide
      continue; // 0Mobile Number Not Provided
    }
    const char * x = CHAR(xp[i]);
    int au_mob = extract_au_mobile(x, n);
    // If it's simply a mobile
    if (au_mob > 0) {
      ansp[i] = au_mob;
      continue;
    }
    int o = extract_landline(x, n, area_cd);
    ansp[i] = o;
  }
  UNPROTECT(1);
  return ans;
}

SEXP C_Mobile_Home(SEXP xx, SEXP yy, SEXP AreaCd) {
  const unsigned int area_cd = asInteger(AreaCd);
  if (!intIsAusRange(area_cd, false)) {
    error("`area_cd = %u` which is not a permitted area code.", area_cd); // # nocov
  }
  R_xlen_t N = xlength(xx);
  if (N != xlength(yy)) {
    error("Internal error(C_Mobile_Home): Lengths of x and y differ."); // # nocov
  }
  if (!isString(xx) || !isString(yy)) {
    error("Internal error(C_Mobile_Home): Wrong types."); // # nocov
  }
  const SEXP * xp = STRING_PTR(xx);
  const SEXP * yp = STRING_PTR(yy);

  SEXP mob = PROTECT(allocVector(INTSXP, N));
  SEXP hom = PROTECT(allocVector(INTSXP, N));

  int * restrict mobp = INTEGER(mob);
  int * restrict homp = INTEGER(hom);

  for (R_xlen_t i = 0; i < N; ++i) {
    int xn = length(xp[i]);
    int yn = length(yp[i]);

    const char * xpi = CHAR(xp[i]);
    const char * ypi = CHAR(yp[i]);

    int x_mob = extract_au_mobile(xpi, xn);
    int x_hom = extract_landline(xpi, xn, area_cd);
    int y_mob = extract_au_mobile(ypi, yn);
    int y_hom = extract_landline(ypi, yn, area_cd);

    mobp[i] = x_mob != NA_INTEGER ? x_mob : y_mob;
    homp[i] = y_hom != NA_INTEGER ? y_hom : x_hom;
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, mob);
  SET_VECTOR_ELT(ans, 1, hom);
  UNPROTECT(3);
  return ans;

}



SEXP PrintMobile(SEXP Mob, SEXP Ccd, SEXP doLongVec) {
  R_xlen_t N = xlength(Mob);
  if (!isInteger(Mob) || (TYPEOF(Ccd) != RAWSXP)) {
    warning("Internal error: Mob and Ccd not INTSXP and RAWSXP of equal length."); // # nocov
    return R_NilValue; // # nocov
  }
  const bool use_ccd = xlength(Ccd) == N;
  const unsigned char * ccd = RAW(Ccd);
  const int * mob = INTEGER(Mob);
  const R_xlen_t topn = 4;

  bool above_top = true;
  int n_digits_last = log10(N);
  // bool long_vec = sizeof(R_xlen_t) > sizeof(int);
  bool long_vec = asLogical(doLongVec);

  for (R_xlen_t i = 0; i < N; ++i) {
    if (i > topn && i < (N - topn)) {
      if (above_top) {
        for (int lz = 0; lz < (n_digits_last - 1); ++lz) {
          Rprintf(" ");
        }
        Rprintf("---\n");
        above_top = false;
      }
      continue;
    }

    unsigned int ucci = 61;
    if (use_ccd) {
      unsigned int uccj = ccd[i];
      ucci = CC[uccj];
    }

    int mobi = mob[i];
    if (mobi <= 0) {
      if (i <= topn) {
        for (int lz = 0; lz < n_digits_last; ++lz) {
          Rprintf(" ");
        }
      }
      if (long_vec) {
        Rprintf("%lld: NA\n", (long long)i + 1);
      } else {
        if (i < INT_MAX) {
          Rprintf("%d: NA\n", (int)i + 1);
        }
      }
      continue;
    }

    int m3 = mobi % 1000;
    int m2 = (mobi / 1000) % 1000;
    int m1 = (mobi / 1000000) % 1000;
    if (i <= topn) {
      for (int lz = 0; lz < n_digits_last; ++lz) {
        Rprintf(" ");
      }
    }
    if (long_vec) {
      Rprintf("%lld: +%d %03d %03d %03d\n", (long long)i + 1, ucci, m1, m2, m3);
    } else {
      if (i < INT_MAX) {
        Rprintf("%d: +%d %03d %03d %03d\n", (int)i + 1, ucci, m1, m2, m3);
      }
    }
  }
  return R_NilValue;
}

// SEXP FormatMobile(SEXP Mob, SEXP Ccd) {
//   R_xlen_t N = xlength(Mob);
//   if (!isInteger(Mob) || (TYPEOF(Ccd) != RAWSXP)) {
//     warning("Internal error: Mob and Ccd not INTSXP and RAWSXP of equal length."); // # nocov
//     return R_NilValue; // # nocov
//   }
//   const bool use_ccd = xlength(Ccd) == N;
//   const unsigned char * ccd = RAW(Ccd);
//   const int * mob = INTEGER(Mob);
//   SEXP ans = PROTECT(allocVector(STRSXP, N));
//
//   for (R_xlen_t i = 0; i < N; ++i) {
//     unsigned int ucci = 61;
//     if (use_ccd) {
//       unsigned int uccj = ccd[i];
//       ucci = CC[uccj];
//     }
//
//     int mobi = mob[i];
//     if (mobi <= 0) {
//       SET_STRING_ELT(ans, i, NA_STRING);
//       continue;
//     }
//
//     int m3 = mobi % 1000;
//     int m2 = (mobi / 1000) % 1000;
//     int m1 = (mobi / 1000000) % 1000;
//     char o[18];
//     snprintf(o, 18, "+%d %03d %03d %03d", ucci, m1, m2, m3);
//   }
//   return R_NilValue;
// }

