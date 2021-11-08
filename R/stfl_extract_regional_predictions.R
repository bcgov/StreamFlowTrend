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



#' Extract the regional predictions from a regional fit
#' @param  fit Results from \code{stfl_trend_regional()} fit
#' @template methods
#' @param transpose If set to TRUE, then converts from wide-format to long-format.
#'
#' @return Returns a data.frame with the extracted predictions in a wide format
#' and in long format if \code{transpose=TRUE}.
#'

#' @examples
#' \dontrun{
#' # needs the HYDAT package installed
#' station.id <- c("08NM053","08NM116")
#' mydata <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#' mydata$REGION <- "AnyOldRegion"
#' mystation.weights <- data.frame(STATION_NUMBER=station.id, WEIGHT=c(2,1))
#' fit <- stfl_trend_regional(mydata, mystation.weights, log.Y=TRUE)
#' stfl_extract_regional_predictions(fit)
#' }
#'#'
#' @importFrom tidyr pivot_wider
#'
#' @export

stfl_extract_regional_predictions <- function(fit, methods=c("LM","MK","LMrob"), transpose=FALSE){

   if(!is.character(methods     ))stop("method argument must be a character")
   if(!is.vector   (methods     ))stop("method argument must be a vector   ")
   if(length(methods)<1)          stop("method argument must be vector of at least length 1")

   values = c("LM","MK","LMrob")
   if(!all(methods %in% values))stop("Can only extract estimates from 'LM', 'MK', or 'LMrob' methods. You specified: ", methods)

   if(!is.logical(transpose))stop("transpose argument must be logical")
   if(length(transpose) !=1) stop("transpose argument must be vector of length 1")

   # see https://stackoverflow.com/questions/48250997/extracting-information-from-multi-level-nested-lists
   get_predictions <- function(x) {
     if (is.null(x) | is.data.frame(x)) return(NULL)
     if ('regional.pred' %in% names(x)){# if there's prediction, we extract it
        predictions <- x$regional.pred
     }
     else {
       purrr::map_dfr(x, get_predictions)
     }
   }

   extract.pred <- get_predictions(fit)
   #browser()
   # which predictions from which methods to be returned.
   predict.methods <- paste0("pred.", methods)
   predict.vars    <- names(extract.pred)[ grepl("pred.", names(extract.pred), fixed=TRUE)]
   if(!all(predict.methods %in% predict.vars))cat("**** WARNING **** Not all method requested have predictions. You asked for predictions from ",
                                               paste(predict.methods,  collapse=", ")," but predictions only available for ",
                                               paste(predict.vars,     collapse=", "), "\n")

   drop.preds <- setdiff(predict.vars, predict.methods)
   extract.pred <- extract.pred[, !names(extract.pred) %in% drop.preds]

   if(!transpose)return(extract.pred)

   # convert from wide to long format
   predict.vars <- c(names(extract.pred)[ grepl("pred.", names(extract.pred), fixed=TRUE)])
   extract.pred.long <-tidyr::pivot_longer(extract.pred,
                                           cols=predict.vars,
                                           names_to="method",
                                           values_to="pred")
   #extract.pred.long$method=substring(extract.pred.long$method, 6)
   return(extract.pred.long)
}

#' station.id <- c("08NM053","08NM116")
#' mydata <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#' mydata$REGION <- "AnyOldRegion"
#' mystation.weights <- data.frame(STATION_NUMBER=station.id, WEIGHT=c(2,1))
#' fit <- stfl_trend_regional(mydata, mystation.weights, log.Y=TRUE)
#' stfl_extract_regional_predictions(fit)
#' stfl_extract_regional_predictions(fit, transpose=TRUE)
#'
