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

#' Estimate trend for 1 station and 1 statistic
#'
#' Estimate trend for 1 station for 1 statistics
#' using least squares (LM), Mann-Kendall (MK), or robust least squares (LMrob) with
#' automatic HAC corrections.

#' @param  data Data frame with X and Y variables
#' @template  log.Y
#' @template Year.var
#' @template Value.var
#' @template Parameter.var
#' @template Statistic.var
#' @template STATION_NUMBER.var
#' @template offset
#' @template methods
#' @param  nsim.bbmks Number of bootstrapped simulations for Mann-Kendall block boot strap
#' @template  trend.min.n

#' @return  List with the following elements
#' \itemize{
#'    \item{data.aug}{Data frame augmented with the LM, MK, and LMrob predictions as specified in the *methods* argument}
#'    \item{estimates}{Data frame with estimates and standard errors from methods specified in *methods* argument
#'     and the following columns
#'    Parameter, Statistic, Year, Value, log.Y, estimate (intercept or Y variable), std error (ignoring autocorrelation),
#'    p.value ignoring autocorrelation,
#'    estimated autocorrelation, std.error adjusted for HAC, p.value adjusted or HAC}
#' }
#'
#' @import broom
#' @importFrom car durbinWatsonTest
#' @importFrom EnvStats kendallTrendTest
#' @import lmtest
#' @importFrom MASS rlm
#' @importFrom modifiedmk mmky1lag
#' @import robustbase
#' @import sandwich
#' @importFrom stats lm predict var
#'
#' @examples
#' \dontrun{
#' # needs the HYDAT package installed
#' station.id <- "08NM116"
#' data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#' temp <- stfl_trend_1stn_1stat(data, log.Y=TRUE)
#' temp
#' }
#'
#' @export

stfl_trend_1stn_1stat<- function(data, log.Y=FALSE,
                                 Year.var="Year",
                                 Value.var="Value",
                                 Parameter.var="Parameter",
                                 Statistic.var="Statistic",
                                 STATION_NUMBER.var="STATION_NUMBER",
                                 methods=c("LMrob","LM","MK"),
                                 offset=NULL, nsim.bbmks=2000, trend.min.n=stfl_options()$trend.min.n){

# checks on the data values
  if(sum(!is.na(data[,Value.var]))<trend.min.n)browser()
  if( !is.data.frame(data))stop("data must be a data.frame")
  if( nrow(data) < trend.min.n) stop("data frame must have at least ", trend.min.n, " observations")
  values <- c(Year.var, Value.var, Parameter.var, Statistic.var)
  if( !all(values %in% names(data)))stop("data frame does not contain the variables of ", paste(values, collapse=", "))
  if( sum(is.na(data[, Year.var]))>0)stop(Year.var, "variable cannot contain any missing values ")

  if( !is.numeric(unlist(data[,Year.var])))stop(Year.var," variable must be numeric")
  if( !is.numeric(unlist(data[,Value.var])))stop(Value.var, " variable must be numeric")

  values <- c("pred.LM",'pred.MK', 'pred.LMrob')
  if( any(values %in% names(data)))stop('data frame contains at least one variable named ', paste(values, collapse=", ")," which conflicts with results")


  if( !is.logical(log.Y))stop('log.Y must be a logical values')
  if( !is.null(offset))check_numeric(offset, min.value=0)

  # check that log.Y is allowed. If not, the print out a warning and reset the log.Y values
  #browser()
  avail_stat <- stfl_get_avail_stat()
  avail_stat <- avail_stat[ tolower(avail_stat$Statistic)==tolower(data[1,Statistic.var]),] # restrict to statistic of interest
  if(log.Y & !avail_stat$logYallowed){
     log.Y <- FALSE
     cat("*** Warning **** You cannot analyze log(Y) for the following statistic ", data[1,Statistic.var], " - Ignored \n")
  }

  check_numeric(nsim.bbmks,  min.value=200)
  check_numeric(trend.min.n, min.value=4  )

  # check methods
  methods=as.vector(methods)
  if(length(methods)==0)stop("You must specify at least one method of analysis")
  if(!all(methods %in% c("LM","LMrob","MK")))stop("Available method must be one or more of LM, LMrob, ML. You have: ",
                                                  paste(methods, collapse=", "))
  methods <- unique(methods)

  mydata   <- as.data.frame(data)  # copy over the mydata for convenience
  mydata$Year <- unlist(data[, Year.var])
  if(any(duplicated(mydata$Year)))stop("Only one value per unique time point allowed")
  mydata$Value <- unlist(data[, Value.var])
  mydata <- mydata[ order(mydata$Year),]  # sort in ascending year
  mydata <- mydata[ !is.na(mydata$Value),] # remove any missing values in y
  #browser()
  # find the offset if taking log values if not specified if any of the Y values are zero
  # if there are no positive values, and offset not specified, use an offset of 0.1
  if(max(mydata$Value[ is.finite(mydata$Value)], na.rm=TRUE)==0){
     if(is.null(offset))offset <- .1
     cat("***** WARNING *****",mydata$STATION_NUMBER[1],mydata$Parameter[1], mydata$Statistic[1],
         " has all zero values and offset (if log(Y) used) is set to ", offset,"\n")
  }
  if(max(mydata$Value[ is.finite(mydata$Value)], na.rm=TRUE)>0){
     if(is.null(offset)) offset <- min(mydata$Value[ mydata$Value > 0 & is.finite(mydata$Value)], na.rm=TRUE)*0.5
  }
  any.zero <- any(mydata$Value==0)
  if( any.zero & log.Y)mydata$Value <- log(mydata$Value+offset)
  if(!any.zero & log.Y)mydata$Value <- log(mydata$Value)
  mydata$log.Y <- log.Y
  #if(sum(!is.na(mydata$y))<10)browser()

  # add the units to the augmented data
  Y.units <- avail_stat[, "Level.Y.label"]
  if(data[1,Parameter.var]=="Flow")Y.units <- avail_stat[,"Flow.Y.label"]
  mydata$Y.units <- Y.units

  # Now for the various methods used to fit the data
  if("LM" %in% methods){
     # fit a robust lm() with HAC corrections for standard error
     # estimate the std error and p-value after adjustments for autocorrelation
     # unless the variance in the mydata$Value is zero, in which case it is pointless
     if(var(mydata$Value, na.rm=TRUE)>0){
        lm.fit <- stats::lm(Value ~ Year, data=mydata)
        mydata$pred.LM <- predict(lm.fit, newdata=mydata)
        lm.est    <- broom::tidy(lm.fit)

        # estimate the autocorrelation
        ar1 <- car::durbinWatsonTest(lm.fit) # from the car package
        lm.est.adj <- broom::tidy(lmtest::coeftest(lm.fit, vcov=sandwich::NeweyWest(lm.fit,verbose=FALSE)))
     }
     if(var(mydata$Value, na.rm=TRUE)==0){
       mydata$pred.LM <- mean(mydata$Value, na.rm=TRUE)
       lm.est <- data.frame(estimate=c(mean(mydata$Value, na.rm=TRUE),0),
                            std.error=c(0,0),
                            p.value  =c(0,1))
       ar1 <- list(r=0)
       lm.est.adj <- data.frame(std.error=lm.est$std.error,
                                p.value  =lm.est$p.value)
     }

     lm.res <- data.frame(STATION_NUMBER=data[1,STATION_NUMBER.var],
                       Year=Year.var, Value=Value.var, Parameter=data[1, Parameter.var], Statistic=data[1, Statistic.var],
                       log.Y=log.Y, method="LM",
                       coefficient= c("intercept","slope"),
                       estimate   = lm.est$estimate,
                       se         = lm.est$std.error,
                       p.value    = lm.est$p.value,
                       rho        = ar1$r,
                       se.adj     = lm.est.adj$std.error,
                       p.value.adj= lm.est.adj$p.value)
  }

  # See if the MK method wanted
  if("MK" %in% methods){
     if(var(mydata$Value, na.rm=TRUE)>0){
        mk.fit <- EnvStats::kendallTrendTest(Value~Year, data=mydata)
        mydata$pred.MK <- mk.fit$estimate["intercept"]+
                          mk.fit$estimate["slope"]    *mydata$Year
        mk.fit.x.se <- sum(mk.fit$interval$limits*c(-1,1))/2/1.96

        mk.adj.fit <- modifiedmk::mmky1lag(mydata$Value)
     }
     if(var(mydata$Value, na.rm=TRUE)==0){
        mydata$pred.MK <- mean(mydata$Value, na.rm=TRUE)
        mk.fit <- list(estimate=c("intercept"=mean(mydata$Value, na.rm=TRUE),"slope"=0),
                       p.value = 1)
        mk.fit.x.se <- 0
        mk.adj.fit <- c("new P-value"=mk.fit$p.value)
     }

     mk.res <- data.frame(STATION_NUMBER=data[1,STATION_NUMBER.var],
                       Year=Year.var, Value=Value.var, Parameter=data[1, Parameter.var], Statistic=data[1, Statistic.var],
                       log.Y=log.Y, method="MK",
                       coefficient= c("intercept","slope"),
                       estimate   = mk.fit$estimate[c("intercept","slope")],
                       se         = c(NA,  mk.fit.x.se),
                       p.value    = c(NA, mk.fit$p.value),
                       rho        = NA,
                       se.adj     = NA,
                       p.value.adj= c(NA, mk.adj.fit["new P-value"]))
  }

  # See if robust LM wanted
  if("LMrob" %in% methods){
     # fit a robust lm() with HAC corrections for standard error
     # estimate the std error and p-value after adjustments for autocorrelation
     # unless the variance in the mydata$Value is zero, in which case it is pointless
     if(var(mydata$Value, na.rm=TRUE)>0){
        lmrob.fit <- robustbase::lmrob(Value ~ Year, data=mydata)
        mydata$pred.LMrob <- predict(lmrob.fit, newdata=mydata)
        lmrob.est <- broom::tidy(lmrob.fit)

        # estimate the autocorrelation
        ar2 <- 1- (car::durbinWatsonTest(stats::resid(lmrob.fit)))/2 # from the car package

        # do the HAC corrections
        # we use the MASS rlm which is basically equivalent to the lmrob() function
        lmrob.est.adj <- broom::tidy(lmtest::coeftest(lmrob.fit,
                                            vcov=sandwich::NeweyWest(MASS::rlm(Value~Year, data=mydata),verbose=FALSE, lag=2)))
     }
     if(var(mydata$Value, na.rm=TRUE)==0){ # data is constant and so no trend or autocorrelation over time
       mydata$pred.LMrob <- mean(mydata$Value, na.rm=TRUE)
       lmrob.est <- data.frame(estimate=c(mean(mydata$Value, na.rm=TRUE),0),
                               std.error=c(0,0),
                               p.value  =c(0,1))
       ar2 <- 0
       lmrob.est.adj <- data.frame(std.error=lmrob.est$std.error,
                                   p.value  =lmrob.est$p.value)
     }
     #browser()

     lmrob.res <- data.frame(STATION_NUMBER=data[1,STATION_NUMBER.var],
                       Year=Year.var, Value=Value.var,
                       Parameter=data[1, Parameter.var], Statistic=data[1, Statistic.var],
                       log.Y=log.Y, method="LMrob",
                       coefficient= c("intercept","slope"),
                       estimate   = lmrob.est$estimate,
                       se         = lmrob.est$std.error,
                       p.value    = lmrob.est$p.value,
                       rho        = ar2,
                       se.adj     = lmrob.est.adj$std.error,
                       p.value.adj= lmrob.est.adj$p.value)
  }

  # combine the individual results
  results <- NULL
  if("LM"    %in% methods)results <- c(results, list(lm.res))
  if("LMrob" %in% methods)results <- c(results, list(lmrob.res))
  if("MK"    %in% methods)results <- c(results, list(mk.res))

  res <- do.call(rbind, results)
  #browser()
  rownames(res)<- NULL

  # reorder the output to make it easier to read on narrow tables
  res <- res[,c("STATION_NUMBER","Parameter","Statistic","log.Y","method","coefficient","estimate","se","p.value","rho","se.adj","p.value.adj","Year","Value")]

  # add the predicted values to the data frame that was sent in
  predict.vars <- paste0("pred.",methods)
  data$Value <- NULL
  #browser()
  data.aug <- merge(data, mydata[,c("log.Y","Y.units","Year","Value",predict.vars)], by.x="Year", by.y="Year")
  list(estimates=res, data.aug=data.aug)

}

# needs the HYDAT package installed
#station.id <- "08NM116"
#data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#data$Value <- 3
#temp <- stfl_trend_1stn_1stat(data, log.Y=TRUE)
#temp2 <- stfl_trend_1stn_1stat(data, log.Y=FALSE)
#temp

#data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#temp <- stfl_trend_1stn_1stat(data, log.Y=TRUE, methods="ABC") # give an error
#temp <- stfl_trend_1stn_1stat(data, log.Y=TRUE, methods=NULL) # give an error
#temp <- stfl_trend_1stn_1stat(data, log.Y=TRUE, methods=2343) # give an error
#temp <- stfl_trend_1stn_1stat(data, log.Y=TRUE, methods=c("LM","LMrob"))
#temp

#data <- stfl_get_annual_stat(station.id, Statistic=c("Min_7_Day_DoY"))
#temp <- stfl_trend_1stn_1stat(data, log.Y=TRUE)
#temp

