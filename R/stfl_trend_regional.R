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

#' Regional analysis on trends

#' Computes a regional analysis using least squares (LM), Mann-Kendall (MK), or robust least squares (LMrob)
#'     with HAC corrections for autocorrelation.
#' This computes the individual slopes for each station within each region, and then weights the
#' individual slopes using the weights.


#' @param  data - data frame with REGION STATION NUMBER, Parameter, Statistic, Year (X) and Value (Y) variables
#' @param  station.weights - data frame with STATION_NUMBER, and WEIGHT
#' @template REGION.var
#' @template  STATION_NUMBER.var
#' @template Parameter.var
#' @template Statistic.var
#' @template  Year.var
#' @template  Value.var
#' @template  WEIGHT.var
#' @template  log.Y
#' @template offset
#' @param  nsim.bbmks - Number of bootstrapped simulations for Mann-Kendall block boot strap
#' @param  trend.min.n - Minimum number of data points for trend computation
#'
#' @return  List with a list for every station containing the station trends  and additional entries for region with the following elements
#' \itemize{
#'    \item{data.aug}{Data frame augmented with the LM, MK, LMrob station trend predictions}
#'    \item{estimates}{Data frame with estimates and standard errors and the following columns for each station
#'    x, y, log.Y, estimate (intercept or Y variable), std error (ignoring autocorrelation),
#'    p.value (ignoring autocorrelation), std.error adjusted for HAC}
#'    \item{regional.estimate}{Data frame with similar variables but with a weighted average of
#'     trends trends from the stations}
#'    \item{regional.pred}{Data frame augmented with the LM, MK, LMrob regional trend predictions}
#' }
#'
#' @import plyr
#' @importFrom stats pnorm
#'
#' @examples
#' \dontrun{
#' # needs the HYDAT package installed
#' station.id <- c("08NM053","08NM116")
#' mydata <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#' mydata$REGION <- "AnyOldRegion"
#' mystation.weights <- data.frame(STATION_NUMBER=station.id, WEIGHT=c(2,1))
#' stfl_trend_regional(mydata, mystation.weights, log.Y=TRUE)
#' }
#'
#' @export

stfl_trend_regional<- function(data, station.weights, log.Y=FALSE,
                               REGION.var="REGION", STATION_NUMBER.var="STATION_NUMBER",  WEIGHT.var="WEIGHT",
                               Year.var="Year", Statistic.var="Statistic", Value.var="Value",Parameter.var="Parameter",
                               offset=NULL, nsim.bbmks=2000, trend.min.n=stfl_options()$trend.min.n){


# checks on the data values
  data <- as.data.frame(data)
  station.weights <- as.data.frame(station.weights)

  if( !is.data.frame(data))stop("data must be a data.frame")
  values <- c(Year.var, Value.var, STATION_NUMBER.var, Statistic.var, Parameter.var, REGION.var)
  if( !all(values %in% names(data)))stop("data frame does not contain all the variables: ", paste(values,collapse=", "))
  if( sum(is.na(data[, Year.var        ]))>0)stop("X variable ",          Year.var,      " cannot contain any missing values in data")
  if( sum(is.na(data[, STATION_NUMBER.var  ]))>0)stop("station variable ",    STATION_NUMBER.var," cannot contain any missing values in data")
  if( sum(is.na(data[, REGION.var   ]))>0)stop("region variable  ",    REGION.var, " cannot contain any missing values in data")
  if( sum(is.na(data[, Parameter.var]))>0)stop("Parameter variable  ", Parameter.var," cannot contain any missing values in data")
  if( sum(is.na(data[, Statistic.var]))>0)stop("Statistic variable  ", Statistic.var," cannot contain any missing values in data")
  if( !is.numeric(unlist(data[,Year.var])))stop("X variable must be numeric")
  if( !is.numeric(unlist(data[,Value.var])))stop("Y variable must be numeric")

  if( !is.data.frame(station.weights))stop("station weights must be a data.frame")
  values <- c(STATION_NUMBER.var, WEIGHT.var)
  if( !all(values %in% names(station.weights)))stop("station.weights data frame does not contain all the variables: ", paste(values,collapse=", "))
  if( sum(is.na(station.weights[, STATION_NUMBER.var]))>0)stop("station.weights station variable ", STATION_NUMBER.var," cannot contain any missing values in data")
  if( sum(is.na(station.weights[, WEIGHT.var ]))>0)stop("station.weights weight variable  ", WEIGHT.var, " cannot contain any missing values in data")
  # all stations in data must exists in the station.weights data but not the reverse
  missing.weight <- setdiff(data[,STATION_NUMBER.var,drop=TRUE], station.weights[, STATION_NUMBER.var,drop=TRUE])
  if( length(missing.weight)>0)stop("all stations in data must have a weight in the station.weights data frame: ", paste(missing.weight, collapse=", "))

  values <- c("pred.lm",'pred.mk', 'pred,lmrob')
  if( any(values %in% names(data)))stop('data frame contains at least one variable named ', paste(values, collapse=", ")," which conflicts with results")


  if( !is.logical(log.Y))stop('log.Y must be a logical values')
  if( !is.null(offset))check_numeric(offset, min.value=0)

  check_numeric(nsim.bbmks,  min.value=200)
  check_numeric(trend.min.n, min.value=4  )

  mydata   <- as.data.frame(data)  # copy over the mydata for convenience
  # count number of observation in each series
  mydata.sum <- plyr::ddply(mydata, STATION_NUMBER.var, function(xx){
                      n.year = sum(!is.na(xx[,Value.var]))
                      dup.year = any(duplicated(xx[,Year.var]))
                      data.frame(n.year, dup.year)})
  if(any(mydata.sum$n.year < trend.min.n))stop("some stations did not have minimum series length of ", trend.min.n)
  if(any(mydata.sum$dup.year))stop("some stations had duplicated yearly values which need to be averaged")

  # copy over data
  mydata$Year    <- unlist(data[, Year.var])
  mydata$Value   <- unlist(data[, Value.var])
  mydata$REGION  <- unlist(data[,REGION.var])
  mydata$STATION_NUMBER <- unlist(data[, STATION_NUMBER.var])
  mydata$Statistic<-unlist(data[, Statistic.var])
  mydata$Parameter<-unlist(data[, Parameter.var])

  mydata <- mydata[ order(mydata$STATION_NUMBER, mydata$Year),]  # sort in ascending year within each STATION_NUMBER
  mydata <- mydata[ !is.na(mydata$Value),] # remove any missing values in y

  # now for every REGION, find the slopes of each STATION_NUMBER and then do an average using the weights
  # provided to get a regional weighted average slope and intercept.
  # we ignore spatial autocorrelation issues
  res <- plyr::dlply(mydata, c("REGION","Parameter","Statistic"), function(x){
     # get the estimates of the slopes for every STATION_NUMBER in this REGION
     fits <- stfl_trend_kstn_kstat(
             data=x,
             STATION_NUMBER.var=STATION_NUMBER.var,
             Statistic.var=Statistic.var,
             Parameter.var=Parameter.var,
             Year.var="Year", Value.var="Value",
             log.Y=log.Y, offset=offset, nsim.bbmks=nsim.bbmks, trend.min.n=trend.min.n)
     # add the region variable to the individual fits (estimates and predicted values)
     fits <- plyr::llply(fits, function(xx){
         xx$estimates[,REGION.var] <- x$REGION[1]
         xx
     })

     # we extract the intercept and slopes, pull in the weights and estimate the weighted average
     extract.coef <-plyr::ldply(fits, function(x){
        est <- x$estimates
        est
     })
     # get the weights
     extract.coef <- merge(extract.coef, station.weights, by=STATION_NUMBER.var, all.x=TRUE )
     #browser()
     # find the mean intercept and mean slope (assuming that all stations are independent)
     mean.coef <- plyr::ddply(extract.coef, c("Statistic","Parameter","Year","Value","log.Y","method","coefficient"), function(x){
        # normalize the weights to sum to 1
        x[,WEIGHT.var] <- x[, WEIGHT.var] /sum(x[,WEIGHT.var])
        estimate = sum(x$estimate  *x[,WEIGHT.var])
        se       = sqrt(sum(x$se^2     * x[,WEIGHT.var]^2))
        p.value  = 2*stats::pnorm( abs(estimate/se), lower.tail=FALSE)
        rho      = sum(x$rho * x[,WEIGHT.var])
        se.adj   = sqrt(sum(x$se.adj^2 * x[,WEIGHT.var]^2))
        p.value.adj  = 2*stats::pnorm( abs(estimate/se.adj), lower.tail=FALSE)
        data.frame(estimate, se, p.value, rho, se.adj, p.value.adj)
     })
     regional.estimates <- mean.coef
     regional.estimates[, REGION.var] <- x[1,REGION.var]

     # now to get predictions of the regional level
     #browser()
     regional.pred <- data.frame(Year=min(mydata$Year):max(mydata$Year))  # values for prediction
     regional.pred[,REGION.var] <- x[1,REGION.var]

     regional.pred$log.Y   <- log.Y
     regional.pred$pred.LM <- regional.estimates$estimate[regional.estimates$coefficient=="intercept" & regional.estimates$method=="LM"]+
                              regional.estimates$estimate[regional.estimates$coefficient=="slope"     & regional.estimates$method=="LM"]*regional.pred$Year
     regional.pred$pred.MK <- regional.estimates$estimate[regional.estimates$coefficient=="intercept" & regional.estimates$method=="MK"]+
                              regional.estimates$estimate[regional.estimates$coefficient=="slope"     & regional.estimates$method=="MK"]*regional.pred$Year
     regional.pred$pred.LMrob <- regional.estimates$estimate[regional.estimates$coefficient=="intercept" & regional.estimates$method=="LMrob"]+
                                 regional.estimates$estimate[regional.estimates$coefficient=="slope"     & regional.estimates$method=="LMrob"]*regional.pred$Year
     list(fits=fits, regional.estimates=regional.estimates, regional.pred=regional.pred)
     })
  res
}

#station.id <- c("08NM053","08NM116")
#mydata <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#mydata$REGION <- "AnyOldRegion"
#mystation.weights <- data.frame(STATION_NUMBER=station.id, WEIGHT=c(2,1))
#stfl_trend_regional(mydata, mystation.weights, log.Y=TRUE)

