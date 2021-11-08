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



#' Extract the station level estimates from a trend fit, both single-station-single-statistic and multi-station-multi-stat cases.
#'
#' @param  fit Results from \code{stfl_trend_1stn_1stat()} or \code{stfl_trend_kstn_kstat()} fit.
#' @param coefficient Which coefficients to extract
#' @template methods
#' @param transpose If set to TRUE, then converts from long-format to wide-format with the statistics from the methods all on one line.
#'
#' @return Returns a data.frame with the extracted estimates, standard errors, p.values (and adjusted for serial autocorrelation) in a long format
#' and in wide format if \code{transpose=TRUE}.
#'

#' @examples
#' \dontrun{
#' # needs the HYDAT package installed
#' station.id <- "08NM116"
#' data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#' res <- stfl_trend_1stn_1stat(data, log.Y=TRUE)
#' estimates <- stfl_extract_station_trends(res)
#' }
#'#'
#' @importFrom purrr map_dfr
#' @importFrom tidyr pivot_wider
#'
#' @export

stfl_extract_station_trends <- function(fit, coefficient=c("slope","intercept"), methods=c("LM","MK","LMrob"), transpose=FALSE){
   if(!is.character(coefficient))stop("coefficient argument must be a character")
   if(!is.vector   (coefficient))stop("coefficient argument must be a vector")
   if(length(coefficient)<1)     stop("coefficient argument must be vector of at least length 1")

   if(is.null(methods))methods=""
   if(!is.character(methods     ))stop("method argument must be a character")
   if(!is.vector   (methods     ))stop("method argument must be a vector   ")
   if(length(methods)<1)          stop("method argument must be vector of at least length 1")

   values = c('slope',"intercept")
   if(!all(coefficient %in% values))stop("Can only extract 'slope' and 'intercept' estimates. You specified: ", coefficient)

   values = c("LM","MK","LMrob","")
   if(!all(methods %in% values))stop("Can only extract estimates from 'LM', 'MK', or 'LMrob' methods. You specified: ", methods)

   if(!is.logical(transpose))stop("transpose argument must be logical")
   if(length(transpose) !=1) stop("transpose argument must be vector of length 1")

   # see https://stackoverflow.com/questions/48250997/extracting-information-from-multi-level-nested-lists
   get_estimates <- function(x) {
     if (is.null(x) | is.data.frame(x)) return(NULL)
     #if (!is.null(x$estimates)) { # if there's an estimates df, we extract it
     if ('estimates' %in% names(x)){# if there's an estimates df, we extract it
        estimates <- x$estimates
     }
     else {
       purrr::map_dfr(x, get_estimates)
     }
   }
   estimates <- get_estimates(fit)

   # subset depending on which coefficients and methods results wanted
   estimates <- estimates[ estimates$coefficient %in% coefficient,]
   estimates <- estimates[ estimates$method      %in% methods,    ]

   if(!transpose)return(estimates)
   if(length(methods)==1 && methods == "")return(NULL)

   # convert from long to wide format with columns such as
   #   LM.intercept.estimate LM.intercept.se LM.intercept.p.value LM.intercept.se.adj LM.intercept.p.value.adj LM.slope.estimate ..... MK.slope ....MK.slope.p.value.adj etc
   #browser()
   t.est1 <- tidyr::pivot_wider(estimates,
                               id_cols=c("STATION_NUMBER","Year","Value","Parameter","Statistic","log.Y"),
                               names_from=c("method","coefficient"),
                               names_glue="{method}.{coefficient}.{.value}",
                               names_sort=TRUE,
                               values_from=c("estimate","se","p.value","se.adj","p.value.adj"))
   # the rho values are duplicated in the slope and intercept row, so we just pick one.
   t.est2 <- tidyr::pivot_wider(estimates[estimates$coefficient=="intercept",],
                               id_cols=c("STATION_NUMBER","Year","Value","Parameter","Statistic","log.Y"),
                               names_from=c("method"),
                               names_glue="{method}.{.value}",
                               names_sort=TRUE,
                               values_from=c("rho"))
   t.est <- merge(t.est1, t.est2, all.x=TRUE)
   t.est
}

#station.id <- "08NM116"
#data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#res <- stfl_trend_1stn_1stat(data, log.Y=TRUE)
#stfl_extract_station_trends(res)
#stfl_extract_station_trends(res, method=NULL)
#stfl_extract_station_trends(res, method=NULL, transpose=TRUE)
#stfl_extract_station_trends(res, transpose=TRUE)
#stfl_extract_station_trends(res, transpose=TRUE, methods="LM")

# multiple stations
#daily<- tidyhydat::hy_daily_flows(c("08NM116","08HB048"), start_date="1973-01-01")
#temp <- stfl_calc_annual_stats(daily, Statistic="Mean")
#trend <- stfl_trend_kstn_kstat(temp)
#stfl_extract_station_trends(trend)
#stfl_extract_station_trends(trend, transpose=TRUE)
