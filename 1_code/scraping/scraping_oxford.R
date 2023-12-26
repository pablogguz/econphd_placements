
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Oxford
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
url <- "https://www.economics.ox.ac.uk/job-market-candidates/job-placements"
web <- read_html(url)

# Extract placement data ----
# Identify the divs containing the placement data
placement_divs <- html_nodes(web, ".panel-body")

# Initialize an empty dataframe
final_df <- data.frame(name = character(), placement = character(), year = numeric(), stringsAsFactors = FALSE)

initial_year <- 2023 

# Loop through each placement div and extract names and placements
for(i in 1:length(placement_divs)) {
  div <- placement_divs[i]
  # Extract both p and h4 elements
  all_text <- html_nodes(div, "p, h4") %>% html_text()
  
  # Convert all text to a dataframe, trim whitespace, and filter out empty strings and "DPHIL STUDENTS"
  df <- as.data.frame(all_text) %>%
    mutate(all_text = str_trim(all_text)) %>%
    filter(all_text != "") %>%
    filter(all_text != "DPHIL STUDENTS")
  
  print(df)
  
  # Correct bug
  if(i == 4) {
    # Replace the specific name with "George Charlson"
    df$all_text[3] <- "George Charlson"  # Assuming 3rd row needs the name replacement
    
    # Create a new row for the placement after the replaced name
    new_row <- data.frame(all_text = "Cambridge, INET (Post-doc)", stringsAsFactors = FALSE)
    
    df <- rbind(df[1:3, , drop = FALSE], new_row, df[4:nrow(df), , drop = FALSE])  # Rebinding rows without replacing the 4th
  }
  
  # Correct bug
  if(i >= 5) {
    # Split each text entry at "\n" and extract names and placements
    split_text <- strsplit(df$all_text, "\\n")
    names <- sapply(split_text, function(x) x[1])
    placements <- sapply(split_text, function(x) if(length(x) > 1) x[2] else "")
    
    # Update df to have separate name and placement columns
    df <- data.frame(name = names, placement = placements, stringsAsFactors = FALSE)
    
    postdoc_index <- which(df$name == "POSTDOCS")
    
    if(length(postdoc_index) > 0) {
      df <- df[1:(postdoc_index - 1), , drop = FALSE]  # keep it as a dataframe
    }
    
    temp_df <- df %>%
      mutate(year = initial_year)
    
    # Append the processed data to the placements_df
    final_df <- rbind(final_df, temp_df)
    
  } else {
    
    # Identify the row with "POSTDOC" and remove all subsequent rows
    postdoc_index <- which(df$all_text == "POSTDOCS")
    
    if(length(postdoc_index) > 0) {
      df <- df[1:(postdoc_index - 1), , drop = FALSE]  # keep it as a dataframe
    }
    
    # Assuming odd rows are names and even rows are placements
    names_df <- df[seq(1, nrow(df), by = 2), , drop = FALSE]
    placements_df <- df[seq(2, nrow(df), by = 2), , drop = FALSE]
    
    # Ensure that both extracted parts have equal length before combining
    if (nrow(names_df) == nrow(placements_df)) {
      temp_df <- data.frame(
        name = names_df$all_text,
        placement = placements_df$all_text,
        year = initial_year,
        stringsAsFactors = FALSE
      )
      
      # Append the processed data to the placements_df
      final_df <- rbind(final_df, temp_df)
    } 
    
  }
  
  # Decrease the year for the next iteration
  initial_year = initial_year - 1
}

# Print or return the final data
print(final_df)

# Clean and finalize the dataframe
final_data <- final_df %>%
  filter(name != "" & placement != "") %>%
  distinct() %>%
  mutate(placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
         placement = str_trim(placement))

# Save ----
write_xlsx(final_data, paste0(data, "/eu/raw/oxford_raw.xlsx"))

