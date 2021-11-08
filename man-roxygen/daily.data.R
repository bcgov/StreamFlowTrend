#' @param daily.data Data frame (typically from the HYDAT data base) with the following fields
#' \itemize{
#'
#'  \item{STATION_NUMBER}{Station number, usually taken from the HYDAT database}
#'  \item{Date}{Date of the reading in YYYY-MM-DD format}
#'  \item{Parameter}{Type of measurement, typically either 'Flow'  or 'Level'}
#'  \item{Value}{Value of the parameter for this date for this station. Numeric.}
#'  }
