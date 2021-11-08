# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' Miscellaneous functions used in the routines for error checking and formatting output.
#'

#' @rdname format_p
#' @title Format a p-value.

#' @param p P-value to be formatted
#' @param precision How many decimal places to be displayed in p-value
#' @export format_p

# define a simple formatter for p.values
# see https://stackoverflow.com/questions/8442322/the-use-of-format-pval-in-r-and-with-sexpr-in-sweave
format_p <- function(p, precision=0.001) {
  if(is.null(p))return(" ")
  digits <- -log(precision, base=10)
  p <- ifelse(is.na(p),"NA",formatC(p, format='f', digits=digits))
  p[p == formatC(0, format='f', digits=digits)] <- paste0('<', precision)
  p
}


#######################################################################3
#' @rdname is_wholenumber
#'
#' @title Check that argument is a whole number
#'
#' @param x A value to be checked to see if it is whole number
#' @param tol Tolerance for checking if a whole number
#' @export is_wholenumber
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol}  # taken from examples of is.integer


#' @rdname check_numeric
#'
#' @title Check that a variable is numeric in a certain range
#'
#' @param variable Value to check if numeric
#' @param min.value Minimum value for variable
#' @param max.value Maximum value for variable
#' @param req.length Required length for the variable.
#' @param check.whole Should the variabile be checked to see if it a whole number?
#' @export check_numeric
#'
check_numeric <- function(variable, min.value=0, max.value=Inf, req.length=1, check.whole=TRUE){
   var.name <- deparse(substitute(variable))
   if(!is.numeric(min.value) | !is.numeric(max.value) | !is.numeric(req.length))
      stop("Bad input values to check.numeric: ", paste(c(var.name,"; ", min.value,"; ", max.value,"; ", req.length), collapse=", "))
   if(length(req.length) !=1)stop("Bad required length for check.numeric. You had ", req.length)
   if(length(min.value) !=1 & length(min.value) != req.length)
      stop("Min values wrong length in check.numeric. You have: ",paste(min.value, collapse=", "))
   if(length(max.value) !=1 & length(max.value) != req.length)
      stop("Max values wrong length in check.numeric. You have: ",paste(max.value, collapse=", "))
   if(!is.numeric(variable))stop("'",var.name, "' must be numeric")
   if( any(is.na(variable)))stop("'",var.name, "' cannot be missing. You have ", paste(variable, collapse=", "))
   if( length(variable) != req.length)stop("'",var.name, "' must have length ", req.length)
   if( any(variable < min.value) | any(variable > max.value))
      stop("'",var.name, "' must between (",paste(min.value, collapse=", "),") and (",
           paste(max.value, collapse=", "),
           ') (inclusive). You have ',paste(variable, collapse=", "))
   # check if values are all whole numbers
   if(check.whole){
     if(any(!is_wholenumber(variable)))stop("'",var.name, "' must be integers. You had ", paste(variable, collapse=", "))
   }
   invisible()
}
