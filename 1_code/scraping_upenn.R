
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for UPenn
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
web <- "https://economics.sas.upenn.edu/graduate/prospective-students/placement-information"
web <- read_html(web)

names <- web %>% html_nodes("a,p") %>% html_text()
names <- as.data.frame(names)
names <- names %>% filter(row_number() > 44) # removed unused lines
names <- names %>% filter(row_number() < 290) # removed unused lines

df <- names %>%
  rename(name = names)

# Identify and carry forward the year
df <- df %>%
  mutate(year = ifelse(grepl("PLACEMENT [0-9]{4}-[0-9]{4}", name), gsub("PLACEMENT ([0-9]{4})-[0-9]{4}", "\\1", name), NA)) %>%
  fill(year, .direction = "down") %>%
  filter(!is.na(year) & !grepl("PLACEMENT [0-9]{4}-[0-9]{4}", name)) %>%
  mutate(year = as.numeric(year) + 1) 

# Separate the name into 'name' and 'placement', handling both "-" and "–"
df <- df %>%
  separate(name, into = c("name", "placement"), sep = "-|–", extra = "merge") %>%
  drop_na() %>%
  mutate(placement = if_else(str_detect(placement, "^\\b(\\w+|\\w+ \\w+) -"), 
                             str_replace(placement, "^\\b(\\w+|\\w+ \\w+) - ", ""), 
                             placement))

# Correct bugs
df$name[156] <- paste(df$name[156], "young Shim ")
df$placement[156] <- paste0("University of California, San Diego (Post-Doc)")

# Save ----
write_xlsx(df, paste0(data, "/us/raw/upenn_raw.xlsx"))

