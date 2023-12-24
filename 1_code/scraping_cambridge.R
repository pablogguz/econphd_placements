
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Cambridge
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
url <- "https://www.econ.cam.ac.uk/postgraduate-studies/phd-economics/phd-placements"
web <- read_html(url)

# Extract placement data ----
# Identify the divs containing the placement data
placement_divs <- html_nodes(web, "div .container-fluid")

# Initialize an empty dataframe
final_df <- data.frame(name = character(), placement = character(), year = numeric(), stringsAsFactors = FALSE)

initial_year <- 2022 

# Loop through each placement div and extract names and placements
for(div in placement_divs) {
  # Extract p elements - each containing a placement record
  placement_records <- html_nodes(div, "p")
  
  for(record in placement_records) {
    # Extract name - it's in <strong> tags
    name <- html_node(record, "strong") %>% html_text() %>% str_trim()
    
    # Extract full text then remove the name to get placement
    full_text <- html_text(record) %>% str_trim()
    placement <- str_replace(full_text, paste0("^", name), "") %>% str_trim()
    
    # Create a temporary dataframe for this record
    temp_df <- data.frame(name = name, placement = placement, year = initial_year, stringsAsFactors = FALSE)
    
    # Append the temporary dataframe to the final dataframe
    final_df <- rbind(final_df, temp_df)
  }
  
  # Decrease the year for the next iteration (adjust this if the year changes in a different manner)
  initial_year <- initial_year - 1
}

# Print or return the final data
print(final_df)

# Clean and finalize the dataframe
final_data <- final_df %>%
  filter(name != "" & placement != "") %>%
  mutate(placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
         placement = str_trim(placement)) %>%
  filter(year >= 2012) # very few obs. in 2011 and 2010, remove

# Save ----
write_xlsx(final_data, paste0(data, "/eu/raw/cambridge_raw.xlsx"))

