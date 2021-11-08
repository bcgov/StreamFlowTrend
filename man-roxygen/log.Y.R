#' @param  log.Y Should the analysis be done on the log(Y+offset) scale where log() referes to natural logarithms.
#' If log(y) makes no sense (e.g., if statistic is DoY), the log() is ignored and set to FALSE automatically.
#' Refer to the *offset* argument for more details.
#'
#' In most cases, the analysis on the *log(Y)* scale is preferred because of the simple interpretation of the trend.
#' For example, an slope of .02 on the *log(Y)* corresponds to a 2% increase/year regardless of the underlying units.
#'
#' In some cases, a *log(Y)* transform makes no sense. For example, the Day_of_Year (DoY) when a minimum occurs is arbitrary
#' and depends on when the start of the year begins (e.g. a water year starts on 1 October).
#'
#' The *stfl_get_avail_stat()* function has a list of available statistics and if a *log(Y)* transform is allowable.
