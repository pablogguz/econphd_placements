
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for UCSD
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
url <- "https://economics.ucsd.edu/graduate-program/jobmarket-tab/placement-history.html"
web <- read_html(url)

# Extract the table
tables <- web %>% html_nodes("table")

# Initialize an empty data frame for the final output
final_data <- data.frame(year = character(), name = character(), placement = character(), stringsAsFactors = FALSE)

# Iterate through each year and corresponding table
for (i in seq_along(tables)) {
  
  table <- tables[i] %>% html_table(fill = TRUE)
  
  if (!is.null(table) && nrow(table[[1]]) > 1) {
    table_data <- table[[1]][, 2:4] 
    
    z = 2024 - i
    
    # Rename the first two columns
    names(table_data)[1:3] <- c("name", "field", "placement")
    
    # Add the year to the table and combine with the final data
    table_data <- table_data %>% mutate(year = z)
    final_data <- rbind(final_data, table_data)
  }
}

final_data <- final_data %>%
  filter(placement != "")

# Save ----
write_xlsx(final_data, paste0(data, "/us/raw/ucsd_raw.xlsx"))

