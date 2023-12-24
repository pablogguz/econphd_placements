
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Princeton
#--------------------------------------------------------------#

#------------------------- 0. Load packages, set paths ------------------------#

# Load packages ----
packages_to_load <- c("rvest", 
                      "stringr",  
                      "dplyr", 
                      "haven", 
                      "writexl", 
                      "tidyr",  
                      "readxl",
                      "schoolmath",
                      "wordcloud2",
                      "ggplot2",
                      "scales",
                      "webshot",
                      "htmlwidgets",
                      "httr",
                      "purrr") 

package.check <- lapply(
  packages_to_load,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

lapply(packages_to_load, require, character=T)

# Set working directory 

dir <- getwd()
dir <- substr(dir,1,nchar(dir)-6)

# Paths

data <- paste0(dir, "0_data/")
code <- paste0(dir, "1_code/")
fig <- paste0(dir, "2_figures/")

#---------------------------- 1. Script starts --------------------------------#

# Load data ---- 
url <- "https://economics.princeton.edu/graduate-program/job-market-and-placements/statistics-on-past-placements/"

web <- read_html(url)

# Extract the table
table <- web %>% html_table()

final_data <- table[[1]] %>%
  mutate(year = substr(Year,6,9),
         placement = ifelse(`Position/Title` != "", paste0(`Position/Title`, ", ", Institution), Institution),
         field = Field) %>%
  select(year, placement, field) %>%
  filter(year != "") 
  
# Save ----
write_xlsx(final_data, paste0(data, "/us/raw/princeton_raw.xlsx"))

