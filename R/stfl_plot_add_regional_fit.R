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

#' Add the regional fit to an existing trend plot
#'
#' @param plot An existing plotly objects to which the regional fit will be added
#' @template regional.fit
#' @template plot.fit
#'
#' @return Plotly object is returned with the regional fit(s) added to the plot.

#' @importFrom plotly add_lines
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # needs the HYDAT package installed
#' station.id <- c("08NM053","08NM116")
#' data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#' temp <- stfl_trend_kstn_kstat(data, log.Y=TRUE)
#' myplot <- stfl_plot_trends(temp, plot.fit=c("MK","LM","LMrob"))
#' myplot

#' data$REGION <- "AnyOldRegion"
#' mystation.weights <- data.frame(STATION_NUMBER=station.id, WEIGHT=c(2,1))
#' regional.fit <- stfl_trend_regional(data, mystation.weights, log.Y=TRUE)

#' stfl_plot_add_regional_fit(myplot,  regional.fit[[1]])
#' }
#' @export


stfl_plot_add_regional_fit <- function(plot,  regional.fit, plot.fit=c("LMrob","LM","MK")[1]){
  # take an existing plot and add the regional fit

  # the first element of the list is checked for the parameter type and if logged
  # this is used to set up the Y axis label
  log.Y <- regional.fit$regional.estimates$log.Y[1]
  #browser()
  if(!is.null(plot.fit)){
      if("LM" %in% plot.fit){
         #if(log.Y)regional.fit$regional.pred$pred.LM <- exp(regional.fit$regional.pred$pred.LM) # need to adjust before plot
         plot <- plot %>% add_lines(data=regional.fit$regional.pred,
                                  x = ~Year,
                                  y = ~pred.LM,
                                  mode = "lines", name="LM-reg", legendgroup="LM", line=list(color='grey', width=4),
                                  showlegend=TRUE,
                                  hoverinfo= 'text', hoverlabel=list(bgcolor="white"),
                                  text=paste("LM Reg Slope: ",
                                   formatC(regional.fit$estimates$estimate[regional.fit$estimates$coefficient=="slope" &
                                                                    regional.fit$estimates$method   =="LM"],
                                           digits=3, format="f")))

      }
      if("LMrob" %in% plot.fit){
         #if(log.Y)regional.fit$regional.pred$pred.LMrob <- exp(regional.fit$regional.pred$pred.LMrob) # need to adjust before plot
         plot <- plot %>% add_lines(data=regional.fit$regional.pred,
                                  x = ~Year,
                                  y = ~pred.LMrob,
                                  mode = "lines", name="LMrob-reg",legendgroup="LMrob", line=list(color='blue', width=4),
                                  showlegend=TRUE,
                                  hoverinfo= 'text', hoverlabel=list(bgcolor="white"),
                                  text=paste("LMrob Reg Slope: ",
                                   formatC(regional.fit$estimates$estimate[regional.fit$estimates$coefficient=="slope" &
                                                                    regional.fit$estimates$method   =="LMrob"],
                                           digits=3, format="f")))
      }
      if("MK" %in% plot.fit){
         #if(log.Y)regional.fit$regional.pred$pred.MK <- exp(regional.fit$regional.pred$pred.MK) # need to adjust before plot
         plot <- plot %>% add_lines(data=regional.fit$regional.pred,
                                  x = ~Year,
                                  y = ~pred.MK,
                                  mode = "lines", name="MK-reg", legendgroup="MK", line=list(color='green', width=4),
                                  showlegend=TRUE,
                                  hoverinfo= 'text', hoverlabel=list(bgcolor="white"),
                                  text=paste("MK Reg Slope: ",
                                   formatC(regional.fit$estimates$estimate[regional.fit$estimates$coefficient=="slope" &
                                                                    regional.fit$estimates$method     =="MK"],
                                           digits=3, format="f")))
      }
  }
  plot
}


#station.id <- c("08NM053","08NM116")
#data <- stfl_get_annual_stat(station.id, Statistic=c("MEAN"))
#temp <- stfl_trend_kstn_kstat(data, log.Y=TRUE)
#myplot <- stfl_plot_trends(temp, plot.fit=c("MK","LM","LMrob"))
#myplot

#data$REGION <- "AnyOldRegion"
#mystation.weights <- data.frame(STATION_NUMBER=station.id, WEIGHT=c(2,1))
#regional.fit <- stfl_trend_regional(data, mystation.weights, log.Y=TRUE)

#stfl_plot_add_regional_fit(myplot,  regional.fit[[1]])
