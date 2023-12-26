
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for Harvard
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
url <- "https://economics.harvard.edu/placement"
web <- read_html(url)

# Extract the table
table <- web %>% html_table()

current_year <- 2023
modified_tables <- list()

for (i in 1:length(table)) {
  year <- current_year - (i - 1)
  df <- mutate(table[[i]], year = year)
  
  # Rename columns for years 2018 and earlier
  if (year <= 2018) {
    df <- df %>%
      rename(name = X1, field = X2, placement = X3) %>%
      select(-starts_with("X"))
  } else {
    df <- df %>%
      rename(name = Name, field = `Fields of Study`, placement = Placement)
  }
  
  modified_tables[[i]] <- df
}

final_table <- bind_rows(modified_tables)

final_table <- final_table %>%
  filter(placement != "Placement")

# Save ----
write_xlsx(final_table, paste0(data, "/us/raw/harvard_raw.xlsx"))


