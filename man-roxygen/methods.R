#' @param methods Which methods should be used to estimate trend lines. Possible values are one or more of
#'    \itemize{
#'      \item{LM}{: Ordinary regression using the lm() function}
#'      \item{MK}{: Mann-Kendall estimates using Sen estimator}
#'      \item{LMrob}{: Robust linear regression fit using the lmrob() function from the robustbase package}
#'
#'    }
