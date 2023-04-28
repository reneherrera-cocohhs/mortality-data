# run this once; after project setup it isn't necessary to run it again
# checks if needed package libraries are installed; if not it will install them

# rené dario herrera
# 13 May 2022
# rherrera at coconino dot az dot gov

# Setup ####
# List of packages needed ####
packages_needed_list <- c(
  "here", # https://github.com/r-lib/here
  "tidyverse", # https://github.com/tidyverse/tidyverse
  "pins", # https://github.com/rstudio/pins
  "lubridate", # https://github.com/tidyverse/lubridate
  "haven", # https://github.com/tidyverse/haven
  "janitor", # https://github.com/sfirke/janitor
  "readxl", # https://github.com/tidyverse/readxl
  "curl", # https://github.com/jeroen/curl
  "purrr", # https://github.com/tidyverse/purrr
  "scales", # https://github.com/r-lib/scales
  "tidycensus", # https://github.com/walkerke/tidycensus
  "zipcodeR", # https://github.com/gavinrozzi/zipcodeR/
  "tigris", # https://github.com/walkerke/tigris
  "sf", # https://github.com/r-spatial/sf/
  "cowplot", # https://github.com/wilkelab/cowplot
  "tidygeocoder", # https://jessecambon.github.io/tidygeocoder/index.html
  "slider", # https://davisvaughan.github.io/slider/
  "pacman", # https://github.com/trinker/pacman
  "tidytext", # https://juliasilge.github.io/tidytext/
  "stopwords", # https://github.com/quanteda/stopwords
  "visdat", # https://docs.ropensci.org/visdat/
  "styler", # https://styler.r-lib.org/
  "renv", # https://rstudio.github.io/renv/
  "lintr"
)

# function #### source: https://gist.github.com/stevenworthington/3178163
# check to see if packages are installed. Install them if they are not
ipak <- function(pkg) {
  new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] # check to see if packages are installed
  if (length(new_pkg)) {
    install.packages(new_pkg, dependencies = TRUE)
  } # Install them if they are not
}

# call function ####
ipak(packages_needed_list)

# R environment status
renv::status()

# add packages to lockfile
renv::snapshot()

# lint and style ####
library(here)
library(lintr)
library(styler)

lint_dir(path = "../mortality-data/")
style_dir(path = "../mortality-data/")
