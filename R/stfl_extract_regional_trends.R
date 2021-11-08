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



#' Extract the regional trend estimates
#'
#' @param  fit Results from \code{stfl_trend_regional()} fit.
#' @param coefficient Which coefficients to extract
#' @param method Which fitting method estimates to be extracted
#' @param transpose When \code{transpose=TRUE}, statistics are converted from long to wide format.
#' @return Returns a data.frame with the extracted estimates, standard errors, p.values (and adjusted for serial autocorrelation)
#'

#' @examples
#' \dontrun{
#' # needs the HYDAT package installed
#' station.id <- c("08NM053","08NM116")
#' mydata <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#' mydata$REGION <- "AnyOldRegion"
#' mystation.weights <- data.frame(STATION_NUMBER=station.id, WEIGHT=c(2,1))
#' fit <- stfl_trend_regional(mydata, mystation.weights, log.Y=TRUE)
#' stfl_extract_regional_trends(fit)
#' }
#'#'
#' @importFrom purrr map_dfr
#' @importFrom tidyr pivot_wider
#' @export

stfl_extract_regional_trends <- function(fit, coefficient=c("slope","intercept"), method=c("LM","MK","LMrob"), transpose=FALSE){

   if(!is.character(coefficient))stop("coefficient argument must be a character")
   if(!is.vector   (coefficient))stop("coefficient argument must be a vector")
   if(length(coefficient)<1)     stop("coefficient argument must be vector of at least length 1")
   if(!is.character(method     ))stop("method argument must be a character")
   if(!is.vector   (method     ))stop("method argument must be a vector   ")
   if(length(method)<1)          stop("method argument must be vector of at least length 1")
   values = c('slope',"intercept")
   if(!all(coefficient %in% values))stop("Can only extract 'slope' and 'intercept' estimates. You specified: ", coefficient)

   values = c("LM","MK","LMrob")
   if(!all(method %in% values))stop("Can only extract estimates from 'LM', 'MK', or 'LMrob' methods. You specified: ", method)

   if(!is.logical(transpose))stop("transpose argument must be logical")
   if(length(transpose) !=1) stop("transpose argument must be vector of length 1")

   # see https://stackoverflow.com/questions/48250997/extracting-information-from-multi-level-nested-lists
   get_estimates <- function(x) {
     if (is.null(x) | is.data.frame(x)) return(NULL)
     if ('regional.estimates' %in% names(x)){# if there's an estimates df, we extract it
        estimates <- x$regional.estimates
     }
     else {
       purrr::map_dfr(x, get_estimates)
     }
   }
   estimates <- get_estimates(fit)

   # subset depending on which coefficients and methods results wanted
   estimates <- estimates[ estimates$coefficient %in% coefficient,]
   estimates <- estimates[ estimates$method      %in% method,     ]

   if(!transpose)return(estimates)

   # convert from long to wide format with columns such as
   #   LM.intercept.estaimte LM.intercept.se LM.intercept.p.value LM.intercept.se.adj LM.intercept.p.value.adj LM.slope.estimate ..... MK.slope ....MK.slope.p.value.adj etc
   #browser()
   t.est1 <- tidyr::pivot_wider(estimates,
                               id_cols=c("REGION","Year","Value","Parameter","Statistic","log.Y"),
                               names_from=c(method,coefficient),
                               names_glue="{method}.{coefficient}.{.value}",
                               names_sort=TRUE,
                               values_from=c("estimate","se","p.value","se.adj","p.value.adj"))
   t.est2 <- tidyr::pivot_wider(estimates[!duplicated(estimates$method),],
                               id_cols=c("REGION","Year","Value","Parameter","Statistic","log.Y"),
                               names_from=c(method),
                               names_glue="{method}.{.value}",
                               names_sort=TRUE,
                               values_from=c("rho"))
   t.est <- merge(t.est1, t.est2)
   t.est
}


# needs the HYDAT package installed
#station.id <- c("08NM053","08NM116")
#mydata <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#mydata$REGION <- "AnyOldRegion"
#\mystation.weights <- data.frame(STATION_NUMBER=station.id, WEIGHT=c(2,1))
#fit <- stfl_trend_regional(mydata, mystation.weights, log.Y=TRUE)
#stfl_extract_regional_trends(fit)
#stfl_extract_regional_trends(fit, transpose=TRUE)


