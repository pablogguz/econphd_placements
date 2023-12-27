
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Berkeley
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
url <- "https://www.econ.berkeley.edu/grad/program/placement-outcomes"
web <- read_html(url)

placement_data <- web %>%
  html_nodes("div.field-item.even") %>%
  html_children() %>%
  {split(., cumsum(html_name(.) == "h3"))}

# Get data for 2012 onwards ----
# Initialize an empty data frame to store the results
final_data <- data.frame(year = character(), placement = character(), stringsAsFactors = FALSE)

# Iterate over each year's data
for (year_data in placement_data) {
  # Extract the year
  
  year_text <- html_text(year_data, "h3")
  
  # Filter for text containing "Jobs Starting Fall..."
  year_text <- year_text[grepl("Jobs Starting Fall", year_text)]
  
  # If year_text is not empty, proceed
  if (length(year_text) > 0) {
    # Use a regular expression to extract the last four-digit number
    year <- str_extract(year_text, "\\d{4}(?!.*\\d)")
    
    # Check if there are placement items in the group
    if (length(html_nodes(year_data, "li")) > 0) {
      # Extract the placements
      placements <- html_text(html_nodes(year_data, "li"))
      
      # Create a temporary data frame for this year
      temp_data <- data.frame(year = rep(year, length(placements)), placement = placements, stringsAsFactors = FALSE)
      
      # Combine with the final data frame
      final_data <- rbind(final_data, temp_data) %>% 
        mutate(year = ifelse(year <= 2012, 2012, year)) 
      
      # Years before 2012 are nested within 2012, which yields error. Use the line above to assign 2012 to all those years,
      # which will be corrected after reading the nested data separately
    }
  }
}

# View the final data frame
print(final_data)

# Get data before 2012 ----
data_before2012 <- placement_data[[13]]

data_before2012 <- data_before2012 %>%
  html_children() %>%
  {split(., cumsum(html_name(.) == "h3"))} 

data_before2012[[1]] <- NULL # remove year 2012 

# Initialize an empty data frame to store the results
final_data2012 <- data.frame(year = character(), placement = character(), stringsAsFactors = FALSE)

# Iterate over each year's data
for (year_data in data_before2012) {
  # Extract the year
  
  year_text <- html_text(year_data, "h3")
  
  # Filter for text containing "Jobs Starting Fall..."
  year_text <- year_text[grepl("Jobs Starting Fall", year_text)]
  
  # If year_text is not empty, proceed
  if (length(year_text) > 0) {
    # Use a regular expression to extract the last four-digit number
    year <- str_extract(year_text, "\\d{4}(?!.*\\d)")
    
    # Check if there are placement items in the group
    if (length(html_nodes(year_data, "li")) > 0) {
      # Extract the placements
      placements <- html_text(html_nodes(year_data, "li"))
      
      # Create a temporary data frame for this year
      temp_data <- data.frame(year = rep(year, length(placements)), placement = placements, stringsAsFactors = FALSE)
      
      # Combine with the final data frame
      final_data2012 <- rbind(final_data2012, temp_data) 
    }
  }
}

## Append final data ----
final <- bind_rows(final_data, final_data2012)

# Clean 
final_data <- final %>% 
  mutate(placement = str_trim(placement)) %>%
  # Split the placement column and extract the repeat factor
  mutate(
    repeat_factor = ifelse(grepl("\\([0-9]+\\)", placement), 
                           as.numeric(sub(".*\\(([0-9]+)\\).*", "\\1", placement)), 
                           1),
    placement = gsub(" \\([0-9]+\\)", "", placement)
  ) %>%
  mutate(placement = str_trim(placement))

# Expand the data frame based on the repeat factor
final_data <- final_data[rep(row.names(final_data), final_data$repeat_factor), ]
row.names(final_data) <- NULL

final_data$repeat_factor <- NULL

## Save ----
write_xlsx(final_data, paste0(data, "/us/raw/berkeley_raw.xlsx"))

