
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Bonn
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
url <- "https://www.econ.uni-bonn.de/bgse/en/job-market/recent-placements-2"
web <- read_html(url)

placement_data <- web %>%
  html_nodes(".tile-content") %>% # Use the correct class selector
  html_text()

# Initialize an empty data frame
final_data <- data.frame(year = character(), name = character(), placement = character(), stringsAsFactors = FALSE)

# Select each 'tiles-wrapper' and iterate
tiles_wrappers <- web %>% html_nodes(".tiles-wrapper")

for(wrapper in tiles_wrappers) {
  
  # For each wrapper, find 'tile-content' elements
  tile_contents <- wrapper %>% html_nodes(".tile-content") %>% html_text()
  
  # Process each tile content
  for(content in tile_contents) {
    
    # Clean and split the content to extract name, and placement
    content <- str_trim(str_replace_all(content, "\n", " "))
    
    name <- content[[1]][1]
    placement <- content[[1]][2]
    
    # Append to the final data frame
    final_data <- rbind(final_data, data.frame(name, placement, stringsAsFactors = FALSE))
  }
}

# Clean ----
final_data <- final_data %>%
  mutate(name = str_trim(name)) %>%
  filter(name != "" & !str_detect(name, fixed("..."))) %>%
  filter(row_number() > 3)

# Initialize variables for year and cleaned data frame
current_year <- NA
cleaned_data <- data.frame(year = integer(), name = character(), placement = character(), stringsAsFactors = FALSE)

# Iterate through the data, treating each pair of rows as a name and its placement
for (i in 1:nrow(final_data)) {
  # Check if the row is a year
  if (grepl("^\\d{4}$", final_data$name[i])) {
    current_year <- as.integer(final_data$name[i])
  } else {
    # Assume current row is a name and next row is its placement
    if (i < nrow(final_data)) { # Ensure we do not go out of bounds
      name <- final_data$name[i]
      placement <- final_data$name[i + 1]
      
      # Add to the cleaned data frame
      cleaned_data <- rbind(cleaned_data, data.frame(year = current_year, name, placement, stringsAsFactors = FALSE))
      
      # Skip the next row as it's already been treated as a placement
      i <- i + 1
    }
  }
}

# View the cleaned data frame
print(cleaned_data)

# Final cleaning ----
cleaned_data <- cleaned_data %>%
  filter(!str_detect(name, fixed("academic"))) %>%
  mutate(row_index = row_number()) %>%
  filter(row_index %% 2 != 0) %>% # filter out even rows
  select(-row_index) %>%
  group_by(name, year) %>%
  filter(row_number()==1)

# Save ----
write_xlsx(cleaned_data, paste0(data, "/eu/raw/bonn_raw.xlsx"))
  