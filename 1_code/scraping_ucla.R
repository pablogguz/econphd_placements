
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for UCLA
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
url <- "https://economics.ucla.edu/graduate/graduate-profiles/graduate-placement-history/"
web <- read_html(url)

# Extract h4 elements (years) and the tables that follow them
years <- web %>% html_nodes("h4")
table <- web %>% html_nodes("table")

# Initialize an empty data frame for the final output
final_data <- data.frame(year = character(), name = character(), placement = character(), stringsAsFactors = FALSE)

# Iterate through each year and corresponding table
for (i in seq_along(years)) {
  # Extract the year, assuming it's in a four-digit format
  year_text <- years[i] %>% html_text() %>% str_trim()
  year <- str_extract(year_text, "\\d{4}")
  
  table <- tables[i] %>% html_table(fill = TRUE)
  
  # Check if the table is not empty and has more than one row (header and data)
  if (!is.null(table) && nrow(table[[1]]) > 1) {
    table_data <- table[[1]]

    # Add the year to the table and combine with the final data
    table_data <- table_data %>% mutate(year = year)
    final_data <- rbind(final_data, table_data)
  }
}

final_data <- final_data %>%
  rename(placement = X2,
         name = X1)
# Save ----
write_xlsx(final_data, paste0(data, "/us/raw/ucla_raw.xlsx"))
