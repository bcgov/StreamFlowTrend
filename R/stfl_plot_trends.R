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

#' @title Plot the results from station level fits to various statistics and various stations

#' @param  fit.res Fit result created by \code{tfl_trend_kstn_kstat()} which is a list and each element of the list has
#'  the \code{estimates} data.frame and the \code{data.aug} data.frame with original data with the predicted values from a
#'  LM, MK, and LMrob fit.
#' @param connect.points Should the annualized values be connected with a line.
#' @template  plot.fit
#' @param force.Y.scale.log Should the Y axis be plotted on the log-scale regardless if the
#' fits are done on the log scale. Note that if the original fit was NOT done on the log(Y) scale,
#' then setting \code{force.Y.scale.log=TRUE} will NOT change the fit - only the ysalce
#' @param plotly Should a plot_ly() graph be returned (default), or a ggplot graph

#' @return plot_ly graph of the results
#'
#' @importFrom plotly ggplotly add_trace add_lines
#' @import ggplot2
#' @import reshape2
#' @import plyr
#' @importFrom magrittr %>%
#' @importFrom scales pretty_breaks
#'
#' @examples
#' \dontrun{
#' # needs the HYDAT package installed
#' station.id <- "08NM116"
#' data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN","P10","P90"))
#' temp <- stfl_trend_kstn_kstat(data, log.Y=TRUE)
#' stfl_plot_trends(temp, plot.fit=c("MK","LM","LMrob"))
#' stfl_plot_trends(temp, plot.fit=c("LMrob"), plotly=TRUE)
#' }
#' @export

stfl_plot_trends <- function(fit.res, plot.fit=c("LMrob","LM","MK")[1], connect.points=TRUE,
                             force.Y.scale.log=FALSE, plotly=TRUE){
#
  #browser()
  if(exists("stfl_options") && stfl_options()$debug){
     cat("stfl_plot_trends", "\n")
  }
  if(is.null(plot.fit))plot.fit=""
  values <-c("LMrob","LM","MK","")
  if(!all(plot.fit %in% values))stop("plot_trends - illegal value of plot.fit")
  plot.fit <- unique(plot.fit)

  if(!is.logical(connect.points))stop("connect.points must be logical of length 1")
  if(!length(connect.points)==1) stop("connect.points must be logical of length 1")

  # Extract the actual values values from the first fit
  first.predictions <- stfl_extract_station_predictions(fit.res, methods=NULL)
  # the first element of the list is checked for the Parameter type, statistics, and if logged
  # this is used to set up the Y axis label

  #estimates <- stfl_extract_station_trends(fit.res, methods=plot.fit)
  log.Y <- first.predictions$log.Y[1]

  # get the Y axis label based on the first set of estimates
  first.Parameter = first.predictions$Parameter[1]
  first.Statistic = first.predictions$Statistic[1]
  stat.list <- stfl_get_avail_stat()
  stat.index <- tolower(stat.list$Statistic) == tolower(first.Statistic)
  Y.label <- ifelse(first.Parameter=="Flow", stat.list$Flow.Y.label[ stat.index], stat.list$Level.Y.label[ stat.index])
  #browser()

    # extract all of the augmented data sets from the list of fits
  plotdata.wide <- stfl_extract_station_predictions(fit.res, methods=plot.fit)
  #browser()
  predict.vars <- names(plotdata.wide)[ grepl("^pred\\.",names(plotdata.wide))]

  # check that requested plot variables in predict vars
  if(!(length(plot.fit)==1 && plot.fit=="")){
     if(!all(paste0("pred.",plot.fit) %in% predict.vars))cat("***** WARNING -- Attempt to plot results from a method that wasn't fit. You requested ",
                                          paste(plot.fit, collapse=", ")," but only ", paste(predict.vars, collapse=",")," are available\n")
     plot.fit <-plot.fit[ plot.fit %in% substring(predict.vars,6)]
  }
  plotdata <- reshape2::melt(plotdata.wide,
                             id.vars=c("STATION_NUMBER","Parameter","Statistic","Year","log.Y","Y.units"),
                             measure.vars=c("Value",predict.vars),
                             variable.name="Source",
                             value.name="Value")
  plotdata$Source <- as.character(plotdata$Source)
  plotdata <- plotdata[ plotdata$Source %in% c("Value",paste0("pred.",plot.fit)),]
  plotdata$trace <- paste0(plotdata$STATION_NUMBER," ",plotdata$Statistic, " ",plotdata$Source)
  plotdata$Station.Stat <- paste0(plotdata$STATION_NUMBER," ",plotdata$Statistic)

  # if analysis was done on the log scale for each statistic we must unlog it
  plotdata$Value<- ifelse(plotdata$log.Y,exp(plotdata$Value), plotdata$Value) # if predicted values are on the log(scale),  unlog them
  plotdata$Value  <- signif(plotdata$Value,4+ceiling(max((log10(plotdata$Value)),na.rm=TRUE)))

    # see https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  myplot <- ggplot(data=plotdata, aes_(x=~Year, y=~Value, group=~trace))+
     geom_point(data=plotdata[plotdata$Source=="Value",],  aes(color=trace))+
     xlab("Year")+ylab("")+
     scale_color_discrete(name="")+
     facet_grid(Y.units~.,switch="both", scales="free_y")
  #browser()
  if(!(length(plot.fit)==1 && plot.fit==""))myplot <- myplot +
     geom_line(data=plotdata[plotdata$Source != "Value",], aes(color=trace))
  if(connect.points)myplot <- myplot + geom_line(data=plotdata[plotdata$Source == "Value",], aes(color=trace))

  # if log(Y) scale, then we need to modify the axis
  if(log.Y | force.Y.scale.log) myplot <- myplot + scale_y_continuous(trans = 'log', breaks=scales::pretty_breaks(n=5))

  # if have multiple y units, then facet by them
  #browser()
  #if(length(unique(plotdata$Y.units))>1)myplot <- myplot + facet_wrap(~Y.units, ncol=1)
    #browser()
  if(plotly){
     myplot <- plotly::ggplotly(myplot)
  }
  return(myplot)
}


# needs the HYDAT package installed
#station.id <- "08NM035"
#data <- stfl_get_annual_stat(station.id, Statistic=c("Min_3_Day"))
#temp <- stfl_trend_kstn_kstat(data, log.Y=TRUE)
#stfl_plot_trends(temp, plot.fit=c("MK","LM","LMrob"))
#stfl_plot_trends(temp, plot.fit=NULL)
#stfl_plot_trends(temp, plot.fit=NULL, plotly=TRUE)
#stfl_plot_trends(temp, plot.fit=c("MK","LM","LMrob"), )
#stfl_plot_trends(temp, plot.fit=c("LMrob"), plotly=TRUE)

#temp <- stfl_trend_kstn_kstat(data, log.Y=FALSE, methods="LM")
#stfl_plot_trends(temp, plot.fit=c("MK","LM","LMrob"))

# test forcing Y axis to be log-scale even if fit not log()
#station.id <- "08NM035"
#data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#temp1 <- stfl_trend_kstn_kstat(data, log.Y=TRUE)
#stfl_extract_station_predictions(temp1)
#stfl_plot_trends(temp1, plot.fit=c("MK","LM","LMrob"))
#stfl_plot_trends(temp1, plot.fit=c("MK","LM","LMrob"), force.Y.scale.log=TRUE)

#
#temp <- stfl_trend_kstn_kstat(data, log.Y=FALSE)
#stfl_extract_station_predictions(temp)
#stfl_plot_trends(temp, plot.fit=c("MK","LM","LMrob"))
#stfl_plot_trends(temp, plot.fit=c("MK","LM","LMrob"), force.Y.scale.log=TRUE)

# Mulltiple statistics of commensurate and incommensurate Y units
#data<- stfl_get_annual_stat(station.id, Statistic=c("Mean","Min_3_Day_DoY"))
#temp <- stfl_trend_kstn_kstat(data, log.Y=TRUE)
#stfl_plot_trends(temp, plot.fit=c("MK","LM","LMrob"))


# multiple stations
#daily<- tidyhydat::hy_daily_flows(c("08NM116","08HB048"), start_date="1973-01-01")
#temp <- stfl_calc_annual_stats(daily, Statistic="Mean")
#trend <- stfl_trend_kstn_kstat(temp)
#stfl_plot_trends(trend)
#stfl_plot_trends(trend, plot.fit=NULL)
#stfl_plot_trends(trend, connect.points=FALSE)

