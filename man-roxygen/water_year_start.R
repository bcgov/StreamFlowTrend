#' @param water_year_start Start month for the water year.
#' @param months_in_year How many months, starting from the water_year_start should be
#'   included in the water year. These two parameters allows you, for example, to select
#'   June - Sept with \code{water_year_start=6} and \code{months_in_year=4}. If you specify
#'   \code{water_year_start=10} and \code{months_in_year=4}, statistics will be computed
#'   using October, November, December, and January (of the next year). The year in which the
#'   water year starts is used as the label for the annualized value -- this differs from ther
#'   \code{fasstr} package.
