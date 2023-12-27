
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Duke
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
url <- "https://econ.duke.edu/phd-program/prospective-students/placements"
web <- read_html(url)

# Extract 'a' elements of class 'normal' which contain the years
year_links <- web %>% html_nodes("a.normal") %>% html_text()

# Process the extracted years if necessary (e.g., extract year numbers)
years <- str_extract_all(year_links, "\\d{4}") %>% unlist()

# Extract tables with class 'tablesaw tablesaw-stack'
placement_tables <- web %>% html_nodes("table.tablesaw.tablesaw-stack")

# Initialize an empty data frame for the final output
final_data <- data.frame()

# Iterate through each year and corresponding table
for (i in seq_along(years)) {
  year <- years[i]
  table <- placement_tables[i] %>% html_table(fill = TRUE)
  
  # Check if the table is not empty
  if (!is.null(table) && length(table) > 0 && nrow(table[[1]]) > 0) {
    # Add the year to the table and combine with the final data
    table_data <- table[[1]] %>% mutate(year = year)
    final_data <- rbind(final_data, table_data)
  }
}

# View the final data frame
print(final_data)

colnames(final_data) <- tolower(colnames(final_data))

final_data <- final_data %>% 
  mutate(placement = paste0(position, ", ", institution)) %>%
  select(-position, -institution)

# Save ----
write_xlsx(final_data, paste0(data, "/us/raw/duke_raw.xlsx"))
