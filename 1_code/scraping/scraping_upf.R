
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for UPF
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
url <- "https://www.upf.edu/web/econ/alumni-and-placement"
web <- read_html(url)

tables <- web %>% html_nodes("tbody")

# Initialize an empty list to store each table's data frame
list_of_dfs <- list()

# Starting year for the first table
starting_year <- 2023

# Loop through each table and extract data
for (i in seq_along(tables)) {
  # Calculate the year based on the table index
  year <- as.character(starting_year - (i - 1))
  
  # Extract names and placements from the table
  names <- tables[[i]] %>% html_nodes("tr td:first-child") %>% html_text() %>% trimws()
  names <- names[-1]
  placements <- tables[[i]] %>% html_nodes("tr td:nth-child(2)") %>% html_text() %>% trimws()
  
  years <- rep(year, length(names))
  
  # Create a DataFrame
  df <- data.frame(name = names, year = year, placement = placements, stringsAsFactors = FALSE)
  
  # Append the data frame to the list
  list_of_dfs[[i]] <- df
}

# Combine all data frames into one
final_data <- bind_rows(list_of_dfs)

final_data <- final_data %>%
  mutate(placement = gsub("<!--td {border: 1px solid #cccccc;}br {mso-data-placement:same-cell;}-->", "", placement, fixed = TRUE),
         name = gsub("<!--td {border: 1px solid #cccccc;}br {mso-data-placement:same-cell;}-->", "", name, fixed = TRUE)) %>%
  mutate(name = str_trim(name),
         placement = str_trim(placement)) %>%
  filter(placement != "") 

# Save ----
write_xlsx(final_data, paste0(data, "/eu/raw/upf_raw.xlsx"))



