
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Cornell
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

# Load data ---- 
url <- "https://economics.cornell.edu/historical-placement-phd-students"
web <- read_html(url)

# Extract the table
table <- web %>% html_table()

placement_data <- table[[1]]

print(placement_data)

names(placement_data) <- tolower(names(placement_data))

placement_data <- placement_data %>%
  rename(placement = `position or affiliation`,
         field = `research area`) %>%
  select(-program) %>%
  filter(placement != "")

# Remove duplicated values in 'placement' column (bug), since they should be empty if field is missing
placement_data <- placement_data %>%
  mutate(field = ifelse(field == placement, NA, field)) %>%
  mutate(field = ifelse(field == "", NA, field))

# Save ----
write_xlsx(placement_data, paste0(data, "/us/raw/cornell_raw.xlsx"))

  
