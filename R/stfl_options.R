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

#' Options for this package.
#'
#' Options for this package. Use the settings::options_manager() to create an override function
#' to change the options here
#'
#' @return Returns a list with options
#'
#' @export
#'
stfl_options <- function(){
   list( debug=FALSE, trend.min.n=10)
}
