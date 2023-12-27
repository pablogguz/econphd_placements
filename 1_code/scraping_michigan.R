
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Michigan
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
                      "purrr",
                      "xml2") 

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
url <- "https://lsa.umich.edu/econ/doctoral-program/past-job-market-placements.html"
web <- read_html(url)

# Initialize a list to store placement data
placement_data <- list()

# Find all h3 elements (assuming they contain the years)
years <- html_text(html_nodes(web, 'h3'))
years <- years[6:15]

# Find all divs containing the placement information
placement_divs <- html_nodes(web, 'div.accordion-body.text')

# Ensure that the number of years and divs are the same
if(length(years) != length(placement_divs)) {
  stop("Mismatch between number of years and number of placement divs")
}

# Loop through each placement div
for(i in 1:length(placement_divs)) {
  
  # Extract raw HTML for each placement info
  placement_nodes  <- html_nodes(placement_divs[i], 'p') 
  
  # Process each placement
  placements_split <- lapply(placement_nodes, function(node) {
    # Extracting the name (text within the <b> tag)
    name <- html_text(xml_find_first(node, "b"))
    
    # Extracting placement info by removing the name and <br> tag from the node's HTML
    node_html <- as.character(node)
    name_html <- as.character(xml_find_first(node, "b"))
    placement_html <- gsub(paste0(name_html, "<br>"), "", node_html)
    placement <- html_text(read_html(placement_html))
    
    return(c(name, placement))
  })
  
  # Create a data frame for the current year
  year_data <- data.frame(
    name = sapply(placements_split, `[`, 1),
    placement = sapply(placements_split, `[`, 2),
    stringsAsFactors = FALSE
  )
  
  # Extract and format the year
  extracted_year <- as.numeric(sub(".*?(\\d{4})-\\d{4}.*", "\\1", years[i]))
  year_formatted <- if(!is.na(extracted_year)) extracted_year + 1 else NA
  
  # Add year column
  year_data$year <- year_formatted
  
  # Append the data frame to the list
  placement_data[[i]] <- year_data
}

# Combine all years into a single data frame without row names
final_data <- do.call(rbind, placement_data)
rownames(final_data) <- NULL

# View the final data
print(final_data)

# Clean 
final_data <- final_data %>%
  filter(year >= 2015) %>%
  mutate(name = str_trim(name),
         placement = str_trim(placement)) %>%
  filter(name != "") %>%
  rowwise() %>%  # Operate on each row individually
  mutate(placement = str_replace(placement, name, "")) %>%
  mutate(placement = str_trim(placement)) 

# Save ----
write_xlsx(final_data, paste0(data, "/us/raw/michigan_raw.xlsx"))
