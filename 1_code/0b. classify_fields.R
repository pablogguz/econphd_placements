
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Classify fields
#--------------------------------------------------------------#

#---------------------- 0. Load packages, set paths ---------------------------#

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

#------------------------------ 1. Script starts ------------------------------#

# Load data ---- 
data_raw <- read_dta(paste0(data, "all/scraped_data_raw.dta"))

data_field <- data_raw %>%
  filter(field != "")

#*******************************************************************************
#* We will focus on the primary field, which will be defined as the first field 
#* mentioned in the field string (i.e., the first field before the first comma)
#* *****************************************************************************

data_field <- data_field %>% 
  separate(field, into = c("primary_field", "secondary_field"), ",")

# Correct: Harvard placements before 2014 (included) do not have actual information on placements 
data_field <- data_field %>% 
  filter(!(inst == "harvard" & year <= 2014))

# Field classification starts ----

classify_field <- function(df) {
  
  # Define keywords for each category
  fields_keywords <- list(
    theory = c("theory", "contracts", "market design", "mechanism design", "market microstructure"),
    appliedmicro = c("applied micro", "applied microeconomics", "applied economics", "gender economics", "applied mircoeconomics"),
    macro = c("macroeconomics", "monetary", "macro"),
    public = c("public"),
    finance = c("finance", "financial", "asset pricing"),
    trade = c("trade"),
    development = c("development", "developmental"),
    behavioral = c("behavioral", "behavioural", "experimental", "psychology", "behavorial"),
    labor = c("labor", "labour"),
    metrics = c("econometrics", "metrics", "econometric methods"),
    history = c("history"),
    international = c("international"),
    education = c("education"),
    io = c("industrial", "io"),
    urban = c("urban", "geography"),
    political = c("political"),
    health = c("health"),
    environment = c("environment", "resource", "environmental"),
    food = c("food", "agricultural"),
    micro = c("microeconomics") # pick-up unmatched, re-classify to theory
  )
  
  # Corrected match_keyword function
  match_keyword <- function(stringtosearch) {
    for (field in names(fields_keywords)) {
      keywords <- fields_keywords[[field]]
      pattern = paste0("\\b(", paste(keywords, collapse="|"), ")\\b")
      if (grepl(pattern, stringtosearch, ignore.case = TRUE)) {
        return(field)
      }
    }
    return("other")
  }
  
  # Apply function to dataframe
  df$field_type <- sapply(df$primary_field, match_keyword)
  
  return(df)
}

# Apply the function 
data_field_classified <- classify_field(data_field)

all_data_classified <- data_raw %>%
  mutate(field = ifelse(inst == "harvard" & year <= 2014, "", field)) %>% # No info on fields for Harvard before 2014 (it's actually the name of the program, i.e. PEG, Business Econ, etc.)
  filter(field == "") %>%
  bind_rows(data_field_classified) %>%
  select(-field) %>%
  mutate(primary_field = str_trim(primary_field),
         secondary_field = str_trim(secondary_field))

# Save ----
write_dta(all_data_classified, paste0(data, "all/scraped_data_raw_wfield.dta"))


