
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Brown
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
url <- "https://economics.brown.edu/academics/graduate/job-placement-results"
web <- read_html(url)

# Select each 'accordion_item' and extract data
accordion_items <- web %>% html_nodes(".accordion_item")

# Initialize an empty data frame for the final output
final_data <- data.frame(year = character(), details = character(), stringsAsFactors = FALSE)

# Iterate through each accordion item
for (item in accordion_items) {
  # Extract the year from the item's title or a specific part
  year <- item %>% html_node(".accordion_trigger") %>% html_text()
  
  # Extract all placement details (each 'li' element) from the item
  placement_details <- item %>% html_nodes("li")
  
  # Iterate through each placement detail and add to the final data frame
  for (detail in placement_details) {
    full_text <- detail %>% html_text() %>% str_trim()
    strong_elements <- detail %>% html_nodes("strong") %>% html_text()
    placement <- paste(strong_elements, collapse = " ") %>% str_trim()
    name <- str_replace(full_text, fixed(placement), "") %>% str_trim()
    
    # Add to the final data frame
    final_data <- rbind(final_data, data.frame(year, name, placement, stringsAsFactors = FALSE))
  }
}

# View the final data frame
print(final_data)

final_data <- final_data %>%
  mutate(name = str_replace(name, "[-]([^-]*)$", "\\1"))

# Save ----
write_xlsx(final_data, paste0(data, "/us/raw/brown_raw.xlsx"))


