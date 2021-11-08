#' @param offset What is the value of the offset used when taking log(Y+offset).
#' If unspecified, then the offset is 1/2 of the smallest positive values.
#'
#' The *offset* is needed to avoid taking log(0).
#'
