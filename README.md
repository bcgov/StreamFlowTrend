
<!--
Copyright 2021 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->

# StreamFlowTrend

<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![R-CMD-check](https://github.com/bcgov/StreamFlowTrend/workflows/R-CMD-check/badge.svg)](https://github.com/bcgov/StreamFlowTrend/actions)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

This is a suite of R functions and vignettes used to explore the
statistical underpinnings of trending annual streamflow metrics using
three different methods:

1.  Mann-Kendall test
2.  regular linear regression
3.  robust linear regression

The package contains the following function and vignettes.

*stfl_function*: *R* functions to:

-   compute station specific trends using the regular regression
    (denoted as the *LM* method), the Mann-Kendall test for trend
    (denoted as the *MK* method), or a robust linear regression (denoted
    at the *LMrob* method).
-   compute station specific plots of the results from above
-   compute regional trends for a set of stations and give weights using
    all three methods
-   add the regional trend to the previous plots
-   create a “dot map” of the trends computed

*v01-individual-station-analysis*: Coming soon.

*v02-collection-stations-analysis*: Coming soon.

*v03-regional-analysis*: Coming soon.

*v04-regional-analysis-AnnualStats*: Coming soon.

### Installation

Use the `remotes` package to install package from GitHub:

``` r
install.packages("remotes") # If not already installed
remotes::install_github("bcgov/StreamFlowTrend") 
```

### Project Status

This project is under development and works as is for the intended
purpose, to explore different statistical methods to trend various
streamflow metrics.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/StreamFlowTrend/issues/).

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2021 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

This repository is maintained by the [Water Protection and
Sustainability
Branch](https://www2.gov.bc.ca/gov/content/environment/air-land-water/water)
of the British Columbia Ministry of Environment and Climate Change
Strategy.
