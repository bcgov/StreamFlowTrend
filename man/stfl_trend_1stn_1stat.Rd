% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stfl_trend_1stn_1stat.R
\name{stfl_trend_1stn_1stat}
\alias{stfl_trend_1stn_1stat}
\title{Estimate trend for 1 station and 1 statistic}
\usage{
stfl_trend_1stn_1stat(
  data,
  log.Y = FALSE,
  Year.var = "Year",
  Value.var = "Value",
  Parameter.var = "Parameter",
  Statistic.var = "Statistic",
  STATION_NUMBER.var = "STATION_NUMBER",
  methods = c("LMrob", "LM", "MK"),
  offset = NULL,
  nsim.bbmks = 2000,
  trend.min.n = stfl_options()$trend.min.n
)
}
\arguments{
\item{data}{Data frame with X and Y variables}

\item{log.Y}{Should the analysis be done on the log(Y+offset) scale where log() referes to natural logarithms.
If log(y) makes no sense (e.g., if statistic is DoY), the log() is ignored and set to FALSE automatically.
Refer to the \emph{offset} argument for more details.

In most cases, the analysis on the \emph{log(Y)} scale is preferred because of the simple interpretation of the trend.
For example, an slope of .02 on the \emph{log(Y)} corresponds to a 2\% increase/year regardless of the underlying units.

In some cases, a \emph{log(Y)} transform makes no sense. For example, the Day_of_Year (DoY) when a minimum occurs is arbitrary
and depends on when the start of the year begins (e.g. a water year starts on 1 October).

The \emph{stfl_get_avail_stat()} function has a list of available statistics and if a \emph{log(Y)} transform is allowable.}

\item{Year.var}{Name of the Year variable (usually the year the data was collected)}

\item{Value.var}{Character string with the name of the variable containing the value to be analyzed.}

\item{Parameter.var}{Character string with the name of the variable containing the name of the Parameter in the daily data
and in the output results.}

\item{Statistic.var}{Character string with the name of the variable containing the statistic that was computed}

\item{STATION_NUMBER.var}{Character string with the name of the variable containing the station number in the daily data and
in the output results..}

\item{methods}{Which methods should be used to estimate trend lines. Possible values are one or more of
\itemize{
\item{LM}{: Ordinary regression using the lm() function}
\item{MK}{: Mann-Kendall estimates using Sen estimator}
\item{LMrob}{: Robust linear regression fit using the lmrob() function from the robustbase package}

}}

\item{offset}{What is the value of the offset used when taking log(Y+offset).
If unspecified, then the offset is 1/2 of the smallest positive values.

The \emph{offset} is needed to avoid taking log(0).}

\item{nsim.bbmks}{Number of bootstrapped simulations for Mann-Kendall block boot strap}

\item{trend.min.n}{Minimum number of data points for trend computation}
}
\value{
List with the following elements
\itemize{
\item{data.aug}{Data frame augmented with the LM, MK, and LMrob predictions as specified in the \emph{methods} argument}
\item{estimates}{Data frame with estimates and standard errors from methods specified in \emph{methods} argument
and the following columns
Parameter, Statistic, Year, Value, log.Y, estimate (intercept or Y variable), std error (ignoring autocorrelation),
p.value ignoring autocorrelation,
estimated autocorrelation, std.error adjusted for HAC, p.value adjusted or HAC}
}
}
\description{
Estimate trend for 1 station for 1 statistics
using least squares (LM), Mann-Kendall (MK), or robust least squares (LMrob) with
automatic HAC corrections.
}
\examples{
\dontrun{
# needs the HYDAT package installed
station.id <- "08NM116"
data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
temp <- stfl_trend_1stn_1stat(data, log.Y=TRUE)
temp
}

}
