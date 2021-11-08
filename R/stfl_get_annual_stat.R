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

#' Extract annualized Statistics from HYDAT database using fasstr package

#' Queries the HYDAT database to extract the daily data and then computes
#' the annualized statistics
#'
#' @template STATION_NUMBER
#' @template Parameter
#' @template Statistic
#' @template water_year_start
#' @template ignore_missing
#' @template debug

#' @return  Data frame in long format with the the following values
#' \itemize{
#'   \item{STATION_NUMBER}{The station number from the daily.data}
#'   \item{Parameter}{The parameter from the daily data}
#'   \item{Statistic}{The annualized statistics requested}
#'   \item{Year}{The year the annualized statistic is computed}
#'   \item{Value}{The value of the annualized statistic}
#' }

#' @import fasstr
#' @import plyr
#' @import tidyhydat
#'
#' @examples
#' \dontrun{
#' # needs the HYDAT package installed
#' station.id <- c("08NM116")
#' stfl_get_annual_stat(station.id, Statistic=c("MEAN","P10"),
#'       water_year_start=1, months_in_year=12)
#'
#' station.id <- c("08NM053","08NM116")
#' res <- stfl_get_annual_stat(station.id, Statistic=c("MEAN","P10"),
#'       water_year_start=1, months_in_year=12)
#' head(res)
#' #' }
#'
#' @export
#'
#'
stfl_get_annual_stat <- function (
       STATION_NUMBER,
       Parameter=c("Flow","Level")[1],
       Statistic,
       water_year_start=1, months_in_year=12,
       ignore_missing=FALSE, debug=FALSE){
#
# checks on the data values
  if( !is.vector(STATION_NUMBER))stop("trend_get_annual_stat: station ids must be a vector")
  if( length(STATION_NUMBER)<1)  stop("trend_get_annual_stat: must have at least 1 station id")

  if( !is.vector(Parameter))stop("trend_get_annual_stat: only specify 1 Parameter")
  values = c("Flow","Level")
  if( !all(Parameter %in% values)) stop("trend_get_annual_stat: Parameter must be one of ", paste(values,collapse=", "))

  if( !is.vector(Statistic))stop("trend_get_annual_stat: stat must be a vector")
  Statistic <- tolower(Statistic)
  if(!all(Statistic %in% tolower(stfl_get_avail_stat()[,1])))stop("one of requested Statistics is not available ", paste(Statistic, compress=", "))

  if( !is.logical(ignore_missing))stop("ignore_missing must be logical")
  if( !is.vector (ignore_missing))stop("ignore_missing must be vector")
  if( length(ignore_missing) !=1)stop ("ignore_missing must be length 1")

  check_numeric(water_year_start, min.value=1, max.value=12, req.length=1, check.whole=TRUE)
  check_numeric(months_in_year,   min.value=1, max.value=12, req.length=1, check.whole=TRUE)

  water_year_start <- as.numeric(water_year_start)
  months_in_year   <- as.numeric(months_in_year)
  if(exists("stfl_options") && stfl_options()$debug){
      cat("stfl_get_annual_stat ", STATION_NUMBER, water_year_start,months_in_year, " \n")
  }

  # get the daily data for each station
  daily <- plyr::ldply(STATION_NUMBER, function(STATION_NUMBER){
     daily <- tidyhydat::hy_daily(station_number=STATION_NUMBER)
  })

  # restrict to Parameter of interest
  daily       <- daily[ daily$Parameter==Parameter, ]

  # get the annual Statistics
  outdf <- plyr::ddply(daily, c("STATION_NUMBER"), function(daily){
     outdf <- stfl_calc_annual_stats(daily, Statistic=Statistic, water_year_start=water_year_start,
                                     months_in_year=months_in_year, ignore_missing=ignore_missing)
     outdf
  })
  outdf
}



