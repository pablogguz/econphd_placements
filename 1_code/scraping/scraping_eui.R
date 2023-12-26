
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for EUI
#--------------------------------------------------------------#

#------------------------------ 0. Load packages, set paths ------------------------------#

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
url <- "https://www.eui.eu/en/academic-units/department-of-economics/job-market/results"
web <- read_html(url)

# Extract placement data and years
placement_data <- web %>% html_nodes(".square-bullet") 
years <- web %>% html_nodes("h2") %>% html_text()

# Remove the first element from the list (not actual placement data)
placement_data <- placement_data[-1]

# Initialize an empty data frame for the final output
final_data <- data.frame(year = character(), institution = character(), title = character(), name = character(), stringsAsFactors = FALSE)

# Iterate through each placement data item and corresponding year
for (i in seq_along(placement_data)) {
  # Extract the latest year (e.g., 2023 from "2022/2023")
  year <- str_extract(years[i], "\\d{4}/\\d{2,4}") %>% str_extract("\\d{2,4}$")
  
  placements <- placement_data[[i]] %>% html_nodes("li")
  
  # Iterate through each individual placement
  for (p in placements) {
    # Extract the full text and identify the last hyphen
    full_text <- p %>% html_text() %>% str_trim()
    last_hyphen_index <- max(str_locate_all(full_text, " - ")[[1]][,2])
    
    if (!is.na(last_hyphen_index)) {
      # Split text at the last hyphen
      placement <- substr(full_text, 1, last_hyphen_index - 1)
      name <- str_trim(substr(full_text, last_hyphen_index + 1, nchar(full_text)))
    } else {
      # Handle cases with no hyphen
      placement <- full_text
      name <- NA
    }
    
    # Add to final data
    final_data <- rbind(final_data, data.frame(year, placement, name, stringsAsFactors = FALSE))
  }
}

# Remove the last hyphen from the "placement" column
final_data <- final_data %>%
  mutate(placement = str_replace(placement, "[-]([^-]*)$", "\\1"))

# Save ----
write_xlsx(final_data, paste0(data, "/eu/raw/eui_raw.xlsx"))
