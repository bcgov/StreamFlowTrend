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

#' @title Estimate trend for multiple stations for multiple statistics

#- Estimate trend for multiple stations for multiple statistics
#' using least squares (LM), Mann-Kendall (MK), robust least squares (LMrob) with automatic HAC corrections for autocorrelation.

#' @param  data Data frame with STATION_NUMBER.var, stat, X and Y variables
#' @template  STATION_NUMBER.var
#' @template  Statistic.var
#' @template  Parameter.var
#' @template  Year.var
#' @template  Value.var
#' @template  methods
#' @template  log.Y
#' @template offset
#' @param  nsim.bbmks Number of bootstrapped simulations for Mann-Kendall block bootstrap
#' @template  trend.min.n
#'
#' @return  List for each station with the following elements
#' \itemize{
#'    \item{data.aug}{Data frame augmented with the LM, MK,LMrob predictions as requested in the *methods* argument}
#'    \item{estimates}{Data frame with estimates and standard errors computed using the methods in the *methods* argument
#'    and the following columns
#'    x, y, STATION_NUMBER, Statistic, log.Y, estimate (intercept or Y variable),
#'    std error (ignoring autocorrelation), p.value (ignoring autocorrelation), estimated autocorrelation,
#'    std.error adjusted for HAC, p.value adjusted for HAC}
#' }
#'
#' @export
#'
#'

stfl_trend_kstn_kstat<- function(data, log.Y=FALSE,
       STATION_NUMBER.var='STATION_NUMBER', Statistic.var="Statistic", Parameter.var="Parameter",
       Year.var="Year", Value.var="Value",
       methods=c("LM","LMrob","MK"),
       offset=NULL, nsim.bbmks=2000, trend.min.n=stfl_options()$trend.min.n){

# checks on the data values
  if( !is.data.frame(data))stop("data must be a data.frame")
  if( nrow(data) < trend.min.n) stop("data frame must have at least ", trend.min.n, " observations")

  if( !STATION_NUMBER.var   %in% names(data))stop("data frame does not contain station number variable", STATION_NUMBER.var)
  if( !Statistic.var %in% names(data))stop("data frame does not contain statistic def variable", Statistic.var)
  if( !Parameter.var %in% names(data))stop("data frame does not contain parameter def variable", Parameter.var )
  values <- c(Year.var, Value.var)
  if( !all(values %in% names(data)))stop("data frame does not contain the variables of ", paste(values, collapse=", "))
  if( sum(is.na(data[, Year.var]))>0)stop(Year.var," variable cannot contain any missing values ")

  if( !is.numeric(unlist(data[,Year.var ])))stop(Year.var,  " variable must be numeric")
  if( !is.numeric(unlist(data[,Value.var])))stop(Value.var, " variable must be numeric")

  values <- c("pred.LM",'pred.MK', 'pred.LMrob')
  if( any(values %in% names(data)))stop('data frame contains at least one variable named ', paste(values, collapse=", ")," which conflicts with results")

  if( !is.logical(log.Y))stop('log.Y must be a logical values')
  if( !is.null(offset))check_numeric(offset, min.value=0)

  check_numeric(nsim.bbmks,  min.value=200)
  check_numeric(trend.min.n, min.value=4  )

  # check methods
  methods=as.vector(methods)
  if(length(methods)==0)stop("You must specify at least one method of analysis")
  if(!all(methods %in% c("LM","LMrob","MK")))stop("Available method must be one or more of LM, LMrob, ML. You have: ",
                                                  paste(methods, collapse=", "))
  methods <- unique(methods)

  res <- plyr::dlply(data, c(STATION_NUMBER.var, Statistic.var, Parameter.var), function(xx){
      res <- stfl_trend_1stn_1stat(xx, log.Y=log.Y,
                              Year.var=Year.var, Value.var=Value.var,
                              methods=methods,
                              offset=offset,
                              nsim.bbmks=nsim.bbmks, trend.min.n=trend.min.n)
      res$estimates$STATION_NUMBER = xx[1, STATION_NUMBER.var]
      res$estimates$Statistic      = xx[1, Statistic.var]
      res$estimates$Parameter      = xx[1, Parameter.var]
      res
  })
  res
}


#station.id <- "08NM116"
#data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN","P10","P90"))
#temp <- stfl_trend_kstn_kstat(data, log.Y=TRUE)
#temp

#station.id <- c("08NM053","08NM116")
#data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN","P10","P90"))
#temp <- stfl_trend_kstn_kstat(data, log.Y=TRUE)
#temp
#  stfl_trend_kstn_kstat(data, log.Y=TRUE, methods=c("LM","MK"))


