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



#' Return a data.frame with available statistics that can be computed
#'
#' Return a data.frame showing the available statistics that can be computed
#' in the \code{stfl_get_annual_stat} and \code{stfl_calc_annual_stat} functions.
#'
#' @return Data.frame with columns \code{Statistic},\code{Defintion},\code{logYallowed},
#' and the Y-axis labels for Flow and Level statistics
#' The \code{logYallowed} value indicates if a trend analysis on the (natural) logarithmic scale should be allowed.
#'
#' @examples
#' stfl_get_avail_stat()
#'
#' @import utils
#'
#' @export

stfl_get_avail_stat <- function(){
avail.stat.csv <- textConnection("
Statistic,        Definition,              logYallowed,   Flow.Y.label,      Level.Y.label
Mean,             Mean daily value,               TRUE,   Discharge (cms),   Level (m)
Median,           Median daily value,             TRUE,   Discharge (cms),   Level (m)
Min,              Minimum daily value,            TRUE,   Discharge (cms),   Level (m)
Max,              Maximum daily value,            TRUE,   Discharge (cms),   Level (m)
P10,              10th percentile of daily value, TRUE,   Discharge (cms),   Level (m)
P90,              90th percentile of daily value, TRUE,   Discharge (cms),   Level (m)
Min_1_Day,        Minimum 1-day daily value,      TRUE,   Discharge (cms),   Level (m)
Min_1_Day_DoY,    Minimum 1-day daily DoY,        FALSE,  Day of Year,               Day of Year
Min_3_Day,        Minimum 3-day daily value,      TRUE,   Discharge (cms),   Level (m)
Min_3_Day_DoY,    Minimum 3-day daily DoY,        FALSE,  Day of Year,               Day of Year
Min_7_Day,        Minimum 7-day daily value,      TRUE,   Discharge (cms),   Level (m)
Min_7_Day_DoY,    Minimum 7-day daily DoY,        FALSE,  Day of Year,               Day of Year
Min_30_Day,       Minimum 30-day daily value,     TRUE,   Discharge (cms),   Level (m)
Min_30_Day_DoY,   Minimum 30-day daily DoY,       FALSE,  Day of Year,               Day of Year
DoY_25pct_TotalQ, DoY of 25% Total Q,             FALSE,  Day of Year,               Day of Year
DoY_33.3pct_TotalQ, DoY of 33.3% Total Q,         FALSE,  Day of Year,               Day of Year
DoY_50pct_TotalQ, DoY of 50% Total Q,             FALSE,  Day of Year,               Day of Year
DoY_75pct_TotalQ, DoY of 75% Total Q,             FALSE,  Day of Year,               Day of Year
Days_Below_Normal,Days below 25th pcntl,          TRUE,   Days below normal, Days below normal")

  avail.stat <- utils::read.csv(avail.stat.csv, as.is=TRUE, strip.white=TRUE, header=TRUE)
  avail.stat
}

#
