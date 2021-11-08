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


#' @title Create a leaflet map with a diverging color scale (e.g. slopes with diverging color scale for positive/negative)

#' Takes a data frame of values and creates a leaflet map with dot at the co-ordinates of the plot
#' and a diverging color scale
#'
#' @param data Data frame with values, latitude, longitude to plot, The variable names in the data.frame
#'   containing these variables are in \code{Value.var}, \code{LONGITUDE.var}, and \code{LATITUDE.var} respectively
#' @param label  Label to be displayed for each point. If this is NULL, then a default label of the station number
#' (extracted from the data data.frame) will be created. To stop labels, set this argument to a null string, i.e. label=''.
#' @param circle_radius Size of the circle to be plotted. You need to set the radius using the
#' output metric of interest. For example, if you want to plot the slopes computed using the LMrob method, the
#' \code{circle_radius} variable will be created by you based on the extracted values of the slopes.
#' @param scale_range Endpoints of the diverging color scale. The centre of the scale will be set to the average of the end points.
#' Values outside the \code{scale_range} will be colors according to the endpoint colors of the scale_range before plotting.
#' For example, if you specify
#' that the scale_range = c(-5, 5), the centre of the diverging scale will be set to 0 (average of -5 and 5).
#' Value of trends that are < -5, will be colored as if their trend was -5. Values of trends that are >5, will
#' be colored as if their trend was 5. So all points are plotted, but extreme trends will be colored
#' according to the colors at the ends of the \code{scale_range}.
#' @param scale_name Name of the scale legend
#' @param Value.var Name of the Y variable to plot
#' @param LONGITUDE.var Name of variable in data that is longitude
#' @param LATITUDE.var  Name of variabel in data that is latitude

#' @return  leaflet map.
#'
#' @import leaflet
#'
#' @export

stfl_plot_map_est_dscale <- function (data, label=NULL,
                                 circle_radius=20,
                                 scale_range=c(-.05,.05),
                                 scale_name = "Scale name",
                                 Value.var="estimate",
                                 LONGITUDE.var="LONGITUDE",
                                 LATITUDE.var ="LATITUDE"
                                 ){

#
# Check the inputs
  if( !is.data.frame(data))stop("plot_map_est_dscale: data must be a data.frame")

  if( !Value.var %in% names(data))stop("plot_map_est_dscale: data frame does not contain the ", Value.var," variable to plot")

  if( !is.numeric(unlist(data[,Value.var])))stop("plot_map_est_dscale: ", Value.var, "variable must be numeric")

  if( !is.numeric(scale_range))stop("plot_map_est_dscale: scale_range must be numeric")
  check_numeric(scale_range, min.value=-Inf, max.value=Inf, req.length=2, check.whole=FALSE)

  if( !LATITUDE.var  %in% names(data))stop("plot_map_est_dscale: data frame does not contain the latitude variable", LATITUDE.var)
  if( !LONGITUDE.var %in% names(data))stop("plot_map_est_dscale: data frame does not contain the longitude variable", LONGITUDE.var )
  if( !is.numeric(unlist(data[,LATITUDE.var ])))stop("plot_map_est_dscale: latitude variable must be numeric")
  if( !is.numeric(unlist(data[,LONGITUDE.var])))stop("plot_map_est_dscale: longitude variable must be numeric")

  if(!is.null(label)){
     if(length(label)!=1 & length(label) != nrow(data))stop("plot_map_est_dscale: need label value for each row in data frame")
  }
  if(is.null(label)){  # create the default label
     values <- c("STATION_NUMBER")
     if(!all(values %in% names(data)))stop("plot_map_est_dscale: missing station number in data when label is set to NULL")
     label <- data[,"STATION_NUMBER"]
  }

  if( !is.numeric(unlist(circle_radius)))stop("plot_map_est_dscale: circle radius must be numeric")
  if( !is.vector (circle_radius))stop("plot_map_est_dscale: circle radius must be a vector")
  if(length(circle_radius)!=1 & length(circle_radius) != nrow(data))stop("plot_map_est_dscale: need circle radius for each row in data frame")

  plotdata <- as.data.frame(data)
  plotdata$...estimate      <- plotdata[,Value.var]
  plotdata$...estimatetrunc <- pmax(min(scale_range), pmin(max(scale_range),plotdata$...estimate)) # truncate to scale range
  plotdata$...circle_radius <- circle_radius
  plotdata$...label         <- label

  # see https://medium.com/inside-machine-learning/center-diverging-colors-on-leaflet-map-515e69d7f81f
  #     https://community.rstudio.com/t/center-diverging-continuous-color-palette-around-0/17399/2
  #browser()
  # Add markers to the plot so you can find the stream
mymap <- leaflet(plotdata) %>% addTiles()%>%
       addMarkers(lng = plotdata[,LONGITUDE.var], lat = plotdata[,LATITUDE.var], label=~...label) %>%
       addCircleMarkers(data= plotdata, lng = plotdata[,LONGITUDE.var], lat = plotdata[,LATITUDE.var],
                       layerId = paste0("Layer :",formatC(1:nrow(plotdata), width=5, format="f", digits=0, flag="0")),
                       color = ~colorBin('RdBu',domain = scale_range)(...estimatetrunc),
                       radius = ~...circle_radius,
                       fillOpacity = 1, stroke = TRUE, weight=10,
                       label  = ~...label) %>%
       addLegend("topright", colorBin('RdBu', domain = scale_range),
                values = scale_range,
                opacity = 0.9,
                title=scale_name)
mymap

}




