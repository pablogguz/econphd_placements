
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Boston University
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
                      "httr") 

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

# Get names, placement and year

web <- read_html("https://www.bu.edu/econ/academics/recent-phd-placements/")
dataweb <- web %>% html_nodes("h3,td") %>% html_text()
dataweb <- as.data.frame(dataweb)
dataweb <- dataweb %>% filter(row_number() > 1) # removed unused lines
dataweb$dataweb <- str_trim(dataweb$dataweb ) # trim
dataweb$year <- str_sub(dataweb$dataweb , start = -4) # extract year

vec_years <- c(2004:2023)
dataweb$year <- ifelse(grepl(paste(vec_years, collapse="|"), dataweb$year),
                       dataweb$year,
                     NA) # generate year column

for (i in 1:nrow(dataweb)) { # complete year column
  j <- i - 1
  if (j > 0 & is.na(dataweb$year[i])) {
    dataweb$year[i] <- dataweb$year[j]
  }
}
dataweb <- dataweb %>% filter(!grepl(paste(vec_years, collapse="|"), dataweb)) # remove rows with years within name variable

dataweb <- dataweb %>% mutate(n = row_number())

placement <- dataweb %>% filter(is.odd(n)) # placement is on odd rows
placement <- placement %>% mutate(n = row_number())
placement <- placement %>% rename(placement = dataweb)

name <- dataweb %>% filter(is.even(n)) # name is on even rows
name <- name %>% mutate(n = row_number())
name <- name %>% rename(name = dataweb)

data_final <- left_join(placement, name)
  
# Only keep first placement if the candidate goes twice on the market

data_final <- data_final %>%
  group_by(name) %>%
  slice(which.min(year)) %>%
  select(-n) %>%
  filter(year != 2006) # bug

# Save ----
write_xlsx(data_final, paste0(data, "/us/raw/bostonu_raw.xlsx"))



