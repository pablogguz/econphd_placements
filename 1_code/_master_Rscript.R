
################################################################
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo García-Guzmán

# This script: master script
###############################################################

source("_packages_paths.R")
setwd(paste0(code))

# Run scripts  

for (i in c("columbia", "harvard", "princeton", "stanford", "duke", "minnesota",
            "yale", "mit", "brown", "berkeley", "nyu", "michigan", "cornell",
            "maryland", "ucl", "upf", "bonn", "warwick")) {
  source(paste0("read_", i, ".R"))
}

for (i in c("bostonu", "lse", "ucla", "upenn")) {
  source(paste0("scraping_", i, ".R"))
}
