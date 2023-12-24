
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for NYU
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
url <- "https://as.nyu.edu/departments/econ/job-market/placements.html"
web <- read_html(url)

# Extract h4 elements (years) and the tables that follow them
years <- web %>% html_nodes("summary")
tables <- web %>% html_nodes("details")

# Keep only data from 2012 onwards (data for previous periods is problematic)
years <- years[1:11]
tables <- tables[1:11]

# Initialize an empty data frame for the final output
final_data <- data.frame(year = character(), placement = character(), stringsAsFactors = FALSE)

# Iterate through each year and corresponding table
for (i in seq_along(years)) {
  # Extract the year, assuming it's in a four-digit format
  year_text <- years[i] %>% html_text() %>% str_trim()
  year <- as.numeric(str_extract(year_text, "\\d{4}"))
  year <- year + 1
  
  full_text <- tables[[i]] %>% html_nodes("p") %>% html_text() %>% str_trim()
  full_text <- full_text[[2]]
  placement <- strsplit(full_text, "\n")[[1]]

  # Add to final data
  final_data <- rbind(final_data, data.frame(year, placement, stringsAsFactors = FALSE))
  
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
write_xlsx(final_data, paste0(data, "/us/raw/nyu_raw.xlsx"))




