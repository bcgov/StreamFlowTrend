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

#' Compute the annualized statistics from daily data
#'
#' Compute the annualized statistics from daily data using the \code{fasstr} routines.
#'
#' @template daily.data
#' @template Statistic
#' @template water_year_start
#' @template roll_days
#' @template min_roll_days
#' @template ignore_missing
#' @template STATION_NUMBER.var
#' @template Date.var
#' @template Value.var
#' @template Parameter.var

#' @return Returns a data.frame with the following variables (sort order cannot be assumed)
#' \itemize{
#'   \item{STATION_NUMBER}{The station number from the daily.data}
#'   \item{Parameter}{The parameter from the daily data}
#'   \item{Statistic}{The annualized statistics requested}
#'   \item{Year}{The year the annualized statistic is computed}
#'   \item{Value}{The value of the annualized statistic}
#' }
#'
#' @import fasstr
#' @import lubridate
#' @import plyr
#' @import utils
#'
#' @examples
#' \dontrun{
#' # needs the HYDAT package installed
#' station.id <- "08NM116"
#'
#' daily <- hy_daily(station_number=station.id)
#' head(daily)
#'
#' res <- stfl_calc_annual_stats(daily, Statistic=c("Mean","P90","Min"),
#'         water_year_start=1, months_in_year=2, ignore_missing=TRUE)
#' head(res1)
#' tail(res1)
#' }

#' @export

stfl_calc_annual_stats <- function(daily.data, Statistic="Mean",
                              water_year_start=1, months_in_year=12,
                              roll_days=1, min_roll_days=c(1,3,7,30), roll_align="right",
                              ignore_missing=FALSE,
                              STATION_NUMBER.var="STATION_NUMBER", Date.var="Date", Value.var="Value",
                              Parameter.var="Parameter"){


  if(!is.data.frame(daily.data))stop("input data must be a data frame")
  values <- c(STATION_NUMBER.var, Date.var, Value.var)
  if(!all(values %in% names(daily.data)))stop("data frame missing at least one of ", paste(values, compress=", "))

  if(!lubridate::is.Date(daily.data[,Date.var,drop=TRUE]))stop("date variable must be a Date variable ", Date.var)
  if(!is.numeric(daily.data[,Value.var,drop=TRUE]))stop("value variable must be numeric")

  water_year_start <- as.numeric(water_year_start)
  months_in_year   <- as.numeric(months_in_year)
  check_numeric(water_year_start, min.value=1, max.value=12, check.whole=TRUE, req.length=1)
  check_numeric(months_in_year  , min.value=1, max.value=12, check.whole=TRUE, req.length=1)
  check_numeric(roll_days,        min.value=1, max.value=30, check.whole=TRUE, req.length=1)
  values <- c("left","right","center")
  if(!is.character(roll_align))stop("roll_align must be character class")
  if(!is.vector(roll_align))   stop("roll_align must be vector of length 1")
  if(length(roll_align) !=1)   stop("roll_align must be vector of length 1")
  if(!tolower(roll_align) %in% values)stop("roll align must be once of 'left', 'right', or 'center'")

  Statistic <- tolower(Statistic)
  if(!all(Statistic %in% tolower(stfl_get_avail_stat()[,1])))stop("one of requested statistics is not available ", paste(Statistic, compress=", "))

  if(exists("stfl_options") && stfl_options()$debug){
    cat("stfl_calc_annual_stats ", nrow(daily.data), water_year_start,months_in_year, " \n")
    print(utils::head(daily.data))
  }

  daily.data$Month <- lubridate::month(daily.data[, Date.var,drop=TRUE])
  good.months <- (water_year_start + (0:(months_in_year-1)))
  good.months <- ifelse(good.months>12, good.months-12, good.months)

  #browser()
  out.df <- NULL
  # now for the various statistics
  if(any(Statistic %in% c("mean","median","max","min","p10","p90"))){
     res <- plyr::ddply(daily.data, c(STATION_NUMBER.var,Parameter.var), function(x){
        fasstr::calc_annual_stats(data=x, water_year_start=water_year_start, months=good.months,
                                  roll_days=roll_days, roll_align=roll_align, ignore_missing=ignore_missing)
     })
     res1 <- res[,c(STATION_NUMBER.var,"Year",Parameter.var)]
     if("mean"   %in% Statistic){res1$Statistic<- "Mean";   res1$Value<- res$Mean;    out.df <- plyr::rbind.fill(out.df,res1)}
     if("median" %in% Statistic){res1$Statistic<- "Median"; res1$Value<- res$Median;  out.df <- plyr::rbind.fill(out.df,res1)}
     if("max"    %in% Statistic){res1$Statistic<- "Max";    res1$Value<- res$Maximum; out.df <- plyr::rbind.fill(out.df,res1)}
     if("min"    %in% Statistic){res1$Statistic<- "Min";    res1$Value<- res$Minimum; out.df <- plyr::rbind.fill(out.df,res1)}
     if("p10"    %in% Statistic){res1$Statistic<- "P10";    res1$Value<- res$P10;     out.df <- plyr::rbind.fill(out.df,res1)}
     if("p90"    %in% Statistic){res1$Statistic<- "P90";    res1$Value<- res$P90;     out.df <- plyr::rbind.fill(out.df,res1)}
  }

  # Minimum statistics
  if(any(Statistic %in% tolower(c( paste0("min_",c(1,3,7,30),"_day"), paste0("min_",c(1,3,7,30),"_day_doy"))))){
     res <- plyr::ddply(daily.data, c(STATION_NUMBER.var,Parameter.var), function(x){
        fasstr::calc_annual_lowflows(data=x, water_year_start=water_year_start, months=good.months,
                                  roll_days=min_roll_days, roll_align=roll_align, ignore_missing=ignore_missing)
     })
     res1 <- res[,c(STATION_NUMBER.var,"Year",Parameter.var)]
     #browser()
     for(d in c(1,3,7,30)){
       stat <- paste0("Min_",d,"_Day")
       if(tolower(stat) %in% Statistic){res1$Statistic <- stat; res1$Value<- res[,stat]; out.df <- plyr::rbind.fill(out.df, res1)}
       stat <- paste0("Min_",d,"_Day_DoY")
       if(tolower(stat) %in% Statistic){res1$Statistic <- stat; res1$Value<- res[,stat]; out.df <- plyr::rbind.fill(out.df, res1)}
     }
  }

  # Run timing statistics
  if(any(Statistic %in% tolower(c( paste0("DoY_",c("25","33.3","50","75"),"pct_TotalQ"))))){
     res <- plyr::ddply(daily.data, c(STATION_NUMBER.var,Parameter.var), function(x){
        fasstr::calc_annual_flow_timing(data=x, water_year_start=water_year_start)
     })
     res1 <- res[,c(STATION_NUMBER.var,"Year",Parameter.var)]
     #browser()
     for(d in c("25","33.3","50","75")){
       stat <- paste0("DoY_",d,"pct_TotalQ")
       if(tolower(stat) %in% Statistic){res1$Statistic <- stat; res1$Value<- res[,stat]; out.df <- plyr::rbind.fill(out.df, res1)}
     }
  }

  # Days below normal
  if(any(Statistic %in% tolower("Days_Below_Normal"))){
     daily.data2 <- daily.data[ !is.na(daily.data$Value),]
     res <- plyr::ddply(daily.data2, c(STATION_NUMBER.var,Parameter.var), function(x){
        fasstr::calc_annual_outside_normal(data=x, water_year_start=water_year_start, months=good.months,
                                  roll_days=roll_days, roll_align=roll_align) #, ignore_missing=ignore_missing) # not supported
     })
     res1 <- res[,c(STATION_NUMBER.var,"Year",Parameter.var)]
     if("days_below_normal"   %in% Statistic){res1$Statistic<- "Days_Below_Normal";   res1$Value<- res$Days_Below_Normal;    out.df <- plyr::rbind.fill(out.df,res1)}
  }

  return(out.df)

}

#station.id <- "08NM116"
#
#daily <- hy_daily(station_number=station.id)
#head(daily)
#
#res <- stfl_calc_annual_stats(daily, Statistic=c("Mean","P90","Min"), water_year_start=1, months_in_year=2, ignore_missing=TRUE)
#head(res1)
#tail(res1)
#res <- stfl_calc_annual_stats(daily, Statistic=c("Days_Below_Normal"), water_year_start=1, months_in_year=2, ignore_missing=TRUE)


#station.id <- "08HF015"
#
#daily <- hy_daily(station_number=station.id)
#head(daily)
#
#res <- stfl_calc_annual_stats(daily, Statistic=c("Mean","P90","Min"))
#head(res1)
#tail(res1)
