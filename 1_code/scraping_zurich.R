
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Zurich
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
url <- "https://www.econ.uzh.ch/en/study/phd/zurichgse/recentplacements.html"
web <- read_html(url)

# Extract years and corresponding blocks of placements
years_nodes <- html_nodes(web, 'h2.TextImage--title.richtext')
placement_blocks <- html_nodes(web, '.TextImage--content.richtext')

# Initialize an empty dataframe to store the results
placement_df <- data.frame(year=character(), name=character(), placement=character(), stringsAsFactors=FALSE)

for (i in seq_along(years_nodes)) {
  year <- html_text(years_nodes[i])
  
  z <- i + 1
  
  # For each year, find the corresponding placement block and iterate through candidates
  candidates <- html_nodes(placement_blocks[z], 'strong')
  placements <- html_nodes(placement_blocks[z], xpath = ".//p[contains(@style, 'margin-left:40px')]")
  
  if(length(candidates) == length(placements)) {
    for (j in 1:length(candidates)) {
      name <- html_text(candidates[j])
      placement <- html_text(placements[j])
      
      # Append to the dataframe
      placement_df <- rbind(placement_df, data.frame(year, name, placement, stringsAsFactors=FALSE))
    }
  } else {
    warning(paste("Mismatch in numbers of candidates and placements for year:", year))
  }
  
}

# View the results
print(placement_df)

# Save ----
write_xlsx(placement_df, paste0(data, "/eu/raw/zurich_raw.xlsx"))
