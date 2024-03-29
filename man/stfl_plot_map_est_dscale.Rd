% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stfl_plot_map_est_dscale.R
\name{stfl_plot_map_est_dscale}
\alias{stfl_plot_map_est_dscale}
\title{Create a leaflet map with a diverging color scale (e.g. slopes with diverging color scale for positive/negative)
Takes a data frame of values and creates a leaflet map with dot at the co-ordinates of the plot
and a diverging color scale}
\usage{
stfl_plot_map_est_dscale(
  data,
  label = NULL,
  circle_radius = 20,
  scale_range = c(-0.05, 0.05),
  scale_name = "Scale name",
  Value.var = "estimate",
  LONGITUDE.var = "LONGITUDE",
  LATITUDE.var = "LATITUDE"
)
}
\arguments{
\item{data}{Data frame with values, latitude, longitude to plot, The variable names in the data.frame
containing these variables are in \code{Value.var}, \code{LONGITUDE.var}, and \code{LATITUDE.var} respectively}

\item{label}{Label to be displayed for each point. If this is NULL, then a default label of the station number
(extracted from the data data.frame) will be created. To stop labels, set this argument to a null string, i.e. label=''.}

\item{circle_radius}{Size of the circle to be plotted. You need to set the radius using the
output metric of interest. For example, if you want to plot the slopes computed using the LMrob method, the
\code{circle_radius} variable will be created by you based on the extracted values of the slopes.}

\item{scale_range}{Endpoints of the diverging color scale. The centre of the scale will be set to the average of the end points.
Values outside the \code{scale_range} will be colors according to the endpoint colors of the scale_range before plotting.
For example, if you specify
that the scale_range = c(-5, 5), the centre of the diverging scale will be set to 0 (average of -5 and 5).
Value of trends that are < -5, will be colored as if their trend was -5. Values of trends that are >5, will
be colored as if their trend was 5. So all points are plotted, but extreme trends will be colored
according to the colors at the ends of the \code{scale_range}.}

\item{scale_name}{Name of the scale legend}

\item{Value.var}{Name of the Y variable to plot}

\item{LONGITUDE.var}{Name of variable in data that is longitude}

\item{LATITUDE.var}{Name of variabel in data that is latitude}
}
\value{
leaflet map.
}
\description{
Create a leaflet map with a diverging color scale (e.g. slopes with diverging color scale for positive/negative)
Takes a data frame of values and creates a leaflet map with dot at the co-ordinates of the plot
and a diverging color scale
}
