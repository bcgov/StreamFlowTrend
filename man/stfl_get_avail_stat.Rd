% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stfl_get_avail_stat.R
\name{stfl_get_avail_stat}
\alias{stfl_get_avail_stat}
\title{Return a data.frame with available statistics that can be computed}
\usage{
stfl_get_avail_stat()
}
\value{
Data.frame with columns \code{Statistic},\code{Defintion},\code{logYallowed},
and the Y-axis labels for Flow and Level statistics
The \code{logYallowed} value indicates if a trend analysis on the (natural) logarithmic scale should be allowed.
}
\description{
Return a data.frame showing the available statistics that can be computed
in the \code{stfl_get_annual_stat} and \code{stfl_calc_annual_stat} functions.
}
\examples{
stfl_get_avail_stat()

}
