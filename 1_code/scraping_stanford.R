
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Stanford
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
url <- "https://economics.stanford.edu/student-placement"
starting_year <- 2023 
total_pages <- 11 

# Function to extract tables from a given URL
extract_tables <- function(url, year) {
  web <- read_html(url)
  tables <- web %>% html_nodes("table") %>% html_table()
  
  tables <- lapply(tables, function(table) {
    table$Year <- year
    return(table)
  })
  
  return(tables)
}

# Initialize an empty list to store tables
all_tables <- list()

# Loop through each page and extract tables
for (page in 0:total_pages) {
  current_year <- starting_year - (page)
  page_url <- paste0(url, "?page=%2C%2C", page) # Update this line based on the actual URL structure
  page_tables <- extract_tables(page_url, current_year)
  all_tables <- c(all_tables, page_tables)
}

# Combine all tables into a single data frame
combined_tables <- bind_rows(all_tables)

# View the combined data
print(combined_tables)

final_data <- combined_tables %>%
  rename(name = Name,
         field = `Fields of Study`,
         placement = Placement,
         year = Year) %>%
  filter(placement != "") %>%
  mutate(field = mapply(function(field, name) gsub(name, "", field), field, name))

# Save ----
write_xlsx(final_data, paste0(data, "/us/raw/stanford_raw.xlsx"))


