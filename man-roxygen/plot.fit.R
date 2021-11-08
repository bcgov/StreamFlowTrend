#' @param plot.fit Which trend lines should be added to the plots. Possible values are one or more of
#'    \itemize{
#'      \item{LM}{: Ordinary regression using the lm() function}
#'      \item{MK}{: Mann-Kendall estimates using Sen estimator}
#'      \item{LMrob}{: Robust linear regression fit using the lmrob() function from the robustbase package}
#'
#'    }
#'    If you try and plot a method that was not computed, a warning message is printed and the request is ignored.
