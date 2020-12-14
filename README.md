  <!-- badges: start -->
  [![Travis build status](https://travis-ci.com/NSAtchison/Stat302Project2.svg?branch=master)](https://travis-ci.com/NSAtchison/Stat302Project2)
  [![Codecov test coverage](https://codecov.io/gh/NSAtchison/Stat302Project2/branch/master/graph/badge.svg)](https://codecov.io/gh/NSAtchison/Stat302Project2?branch=master)
  <!-- badges: end -->

## Use

The vignette demonstrates example usage of all main functions. Please [file an issue](https://github.com/bryandmartin/corncob/issues) if you have a request for a tutorial that is not currently included. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):


``` r
# install.packages("devtools")
devtools::install_github("NSAtchison/Stat302Project2", build_vignette = TRUE, build_opts = c())
library(Stat302Project2)
# Use this to view the vignette in the corncob HTML help
help(package = "Stat302Project2", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "Stat302Project2")
```
