
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for NU
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
url <- "https://economics.northwestern.edu/graduate/prospective/placement.html"
web <- read_html(url)

# Initialize a list to store placement data
placement_data <- list()

# Loop through each group (assuming there are 14 groups)
for(i in 1:14) {
  # Construct the CSS selector for each group
  group_selector <- paste0("#group-", i)
  
  # Find the h3 element for the group
  group_header <- html_node(web, group_selector)
  
  # Find the sibling div containing the placement info
  placement_info <- html_node(group_header, xpath = "following-sibling::div[contains(@class, 'expander')]")
  
  # Extract the list items containing the placement data
  placements <- html_text(html_nodes(placement_info, "li"))
  
  # Store the extracted data
  placement_data[[i]] <- placements
}

# Initialize a data frame to store the final results
final_data <- data.frame(placement = character(), year = integer(), stringsAsFactors = FALSE)

# Loop through each group
for(i in 1:14) {
  # Extract year from the group header (assuming the year is the last four characters)
  group_header <- html_text(html_node(web, paste0("#group-", i)))
  year <- as.integer(substring(group_header, nchar(group_header) - 3))
  
  # Extract the placement data for the group
  placement_info <- placement_data[[i]]
  
  # Create a temporary data frame for the current group
  temp_data <- data.frame(placement = placement_info, year = year, stringsAsFactors = FALSE)
  
  # Append the temporary data frame to the final data frame
  final_data <- rbind(final_data, temp_data)
}

# Clean 
final_data <- final_data %>% 
  mutate(placement = str_trim(placement)) %>%
  # Split the placement column and extract the repeat factor
  mutate(
    repeat_factor = ifelse(grepl("\\(x[0-9]+\\)", placement), 
                           as.numeric(sub(".*\\(x([0-9]+)\\).*", "\\1", placement)), 
                           1),
    placement = gsub(" \\(x[0-9]+\\)", "", placement)
  ) %>%
  mutate(placement = gsub("(x2)", "", x = placement, fixed = TRUE)) %>%
  mutate(placement = str_trim(placement))

# Expand the data frame based on the repeat factor
final_data <- final_data[rep(row.names(final_data), final_data$repeat_factor), ]
row.names(final_data) <- NULL

final_data$repeat_factor <- NULL

# Save ----
write_xlsx(final_data, paste0(data, "/us/raw/northwestern_raw.xlsx"))
