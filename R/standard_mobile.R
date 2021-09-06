#' Standardize Australian phone numbers
#'
#' @param x A vector, generally a character vector, in which phone numbers
#' are expected.
#'
#' @param ignore_calling_code \code{logical(1)} Whether to ignore the calling code
#' \code{+61} in the result.
#'
#' @param mob,landline Character vectors in which mobile numbers and landline
#' numbers are expected.
#'
#' @param default_area_code An integer between 1 and 10 giving, for
#' landline numbers with 8 digits, what area code should be set. By default,
#' it is \code{1L}, but users should set the option
#' \code{"dauphin.default_area_code"} so to correctly standardize non-mobile
#' numbers.
#'
#' @param ... Arguments passed to other methods.
#'
#' @return
#' Mobile phone numbers or landline numbers are represented as integer vectors. International
#' calling prefixes extend the number beyond the representation of signed
#' integers. We use \code{raw} vectors for the international prefix, if required.
#'
#'
#' If \code{ignore_calling_code = TRUE}, the integer vector is returned.
#' Elements of \code{x} for which the mobile phone number could not be
#' extracted map to \code{NA_integer_} in the result.
#'
#' If \code{ignore_calling_code = FALSE}, then a list is returned. The second element
#' of the list is the calling prefix.
#'
#' If \code{ignore_calling_code = NA} then it is set to \code{TRUE} if \code{x}
#' appears to have international prefixes already.
#'
#' \describe{
#' \item{\code{dauphin_mobile}}{An integer vector, the integer representation of
#' the mobile phone. If the calling code is required or requested, a list
#' of two vectors is returned, with the second element a raw vector with
#' a representation of the corresponding number.}
#' \item{\code{dauphin_landline}}{An integer vector, the integer representation of the landline.}
#' \item{\code{dauphin_mobile_landline}}{A list of two vectors with the
#' mobile and landline vectors respectively, even if the character
#' vectors passed are in the wrong order (or partially in the wrong order).
#'  Useful if some of the entries
#' are in the wrong place.}
#'
#' }
#'
#' @examples
#' dauphin_mobile("0400 123 456")
#' dauphin_mobile("+61400123456", ignore_calling_code = FALSE)
#'
#' dauphin_mobile_landline("0424 123 456", "03 1234 5678")
#' dauphin_mobile_landline(c("0424 123 456", "03 1234 5678"),
#'                         c(NA, "0424 123 456"))
#'
#' @export

dauphin_mobile <- function(mob, ignore_calling_code = NA) {
  # calling code required?
  cc_required <- .Call("C_CCRequired", mob, ignore_calling_code, PACKAGE = packageName())
  ans <- .Call("CStandardMobile", mob, PACKAGE = packageName())
  if (!cc_required) {
    return(.subset2(ans, 1L))
  }
  # class(ans) <- "dauphin_mobile"
  ans
}

#' @rdname dauphin_mobile
#' @export
dauphin_landline <- function(landline, default_area_code = getOption("daiphin.default_area_code", 1L)) {
  if (is.character(default_area_code <- check_area_cd(default_area_code))) {
    stop(default_area_code)
  }
  .Call("C_DauphinLandline", landline, default_area_code, PACKAGE = packageName())
}


#' @rdname dauphin_mobile
#' @export
dauphin_mobile_landline <- function(mob, landline, default_area_code = getOption("dauphin.default_area_code", 1L)) {
  if (is.character(default_area_code <- check_area_cd(default_area_code))) {
    stop(default_area_code)
  }
  ans <- .Call("C_Mobile_Home", mob, landline, default_area_code, PACKAGE = packageName())
}

#' @rdname dauphin_mobile
#' @export
print.dauphin_mobile <- function(x, ...) {
  # Mobile and calling code
  if (is.atomic(x)) {
    MOB <- x
  } else {
    MOB <- .subset2(x, 1L)
  }
  N <- length(MOB)
  CCD <- raw()
  if (!is.atomic(x) && length(x) >= 2L) {
    CCD <- .subset2(x, 2L)
  }
  if (!is.raw(CCD) || length(CCD) != N) {
    CCD <- raw(0)
  }

  doLongVec <- .Machine$sizeof.pointer == 8L && isTRUE(capabilities("long.double")) && isTRUE(getOption(".dauphin.long.vec", TRUE))

  invisible(.Call("PrintMobile", MOB, CCD, doLongVec, PACKAGE = packageName()))
}

format_dauphin_mobile <- function(x, ...) {
  if (is.atomic(x)) {
    MOB <- x
  } else {
    MOB <- .subset2(x, 1L)
  }
  N <- length(MOB)
  CCD <- raw()
  if (!is.atomic(x) && length(x) >= 2L) {
    CCD <- .subset2(x, 2L)
  }
  if (!is.raw(CCD) || length(CCD) != N) {
    sprintf("+61 %03d %03d %03d", (MOB %/% 1e6L) %% 1000L, (MOB %/% 1e3L) %% 1000L, MOB %% 1000L)
  } else {
    cc_int <- DecodeCC(CCD)
    ifelse(cc_int == 61L,
           sprintf("+61 %03d %03d %03d", (MOB %/% 1e6L) %% 1000L, (MOB %/% 1e3L) %% 1000L, MOB %% 1000L),
           # Can't be sure of the format of foreign mobiles
           sprintf("+%d %d", cc_int, MOB))
  }
}

intl_calling_code_reqd <- function(mob) {
  .Call("C_CCRequired", mob, NA, PACKAGE = packageName()) # nocov
}

check_area_cd <- function(area_cd) {
  if (!is.numeric(area_cd)) {
    return(paste0("`area_cd` was type '", typeof(area_cd), "' but must be integer."))
  }
  if (length(area_cd) != 1L) {
    return(paste0("`length(area_cd) = ", length(area_cd), "` but must be length-one."))
  }
  if (is.na(area_cd)) {
    return(paste0("`area_cd = NA` but this is not permitted."))
  }
  if (area_cd <= 0L || area_cd >= 10L) {
    return(paste0("`area_cd = ", area_cd, "` but must be between 1 and 10."))
  }
  as.integer(area_cd) * 1e8L
}

EEncodeCC <- function(x) {
  .Call("EncodeIntCC", x, PACKAGE = packageName())
}

DecodeCC <- function(x) {
  .Call("DecodeRawCC", x, PACKAGE = packageName())
}

