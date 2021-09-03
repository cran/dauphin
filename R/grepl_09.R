

gsub_09 <- function(x) {
  .Call("Cgsub_09", x, PACKAGE = packageName())
}

grepl_digit <- function(x) {
  .Call("Cgrepl_digit", x, PACKAGE = packageName())
}

