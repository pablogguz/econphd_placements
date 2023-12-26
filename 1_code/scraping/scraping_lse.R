
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Scrapes data for LSE
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

# Function to extract the name considering compound surnames with hyphens
extract_name <- function(s) {
  # Find all capital letters
  caps <- gregexpr("[A-Z]", s)[[1]]
  
  # Determine the cut-off point for the name extraction
  # Default is the position of the third capital letter
  cutoff <- if (length(caps) >= 3) caps[3] - 1 else nchar(s)
  
  # Extract substring up to the cutoff point
  name_substring <- substr(s, 1, cutoff)
  
  # Check if there is a hyphen in the extracted substring and at least 4 capital letters
  if (grepl("-", name_substring) && length(caps) >= 4) {
    # If there is a hyphen, consider the fourth capital letter for the cutoff
    cutoff <- caps[4] - 1
  }
  
  # Extract the final name using the determined cutoff point
  return(substr(s, 1, cutoff))
}


# 2023 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2022-2023")
names <- web %>% html_nodes("p") %>% html_text()
names <- names[3:12]

# Process the vector
df2023 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, ".*(?=Research interests)"),
    field = str_extract(strings, "(?<=Research interests:).*"),
    year = 2023
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

# 2022 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2021-2022")
names <- web %>% html_nodes("p") %>% html_text()
names <- names[3:13]

# Process the vector
df2022 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, ".*(?=Research interests)"),
    field = str_extract(strings, "(?<=Research interests:).*"),
    year = 2022
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2022 

# 2021 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2020-2021")
names <- web %>% html_nodes("p") %>% html_text()
names <- names[4:14]

# Process the vector
df2021 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, ".*(?=Research interests)"),
    field = str_extract(strings, "(?<=Research interests:).*"),
    year = 2021
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2021

# Correct bugs
df2021$placement[8] <- paste("Post-doc, ", df2021$placement[8])
df2021$placement[8] <- gsub("2021-22Assistant", "2021-22 Assistant", df2021$placement[8])

# Remove the trailing 'Post-doc,' from the name in row 8
df2021$name[8] <- gsub("Post-doc,", "", df2021$name[8])
df2021$name[8] <- str_trim(df2021$name[8]) # Trim to remove any leading/trailing spaces

df2021$placement[9] <- gsub("Thysen", "", df2021$placement[9])

df2021

# 2020 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2019-2020")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:15]

# Process the vector
df2020 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, ".*(?=Research interests)"),
    field = str_extract(strings, "(?<=Research interests:).*"),
    year = 2020
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2020

df2020$placement[8] <- gsub("Lee", "", df2020$placement[8])

# 2019 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2018-2019")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[4:24]

# Process the vector
df2019 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, ".*(?=Research interests)"),
    field = str_extract(strings, "(?<=Research interests:).*"),
    year = 2019
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2019

df2019$placement[10] <- gsub("Walter", "", df2019$placement[10])

# 2018 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2017-2018")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[4:24]

# Process the vector
df2018 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, ".*(?=Primary research interest[s]?)"),
    field = str_extract(strings, "(?<=Primary research interest[s]?:).*"),
    year = 2018
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2018

# 2017 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2016-2017")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:15]

# Process the vector
df2017 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2017
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2017

# 2016 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2015-2016")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[4:13]

# Process the vector
df2016 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2016
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2016

df2016$placement[4] <- gsub("Ferra", "", df2016$placement[4])
df2016$name[4] <- gsub("Sergio De", "Sergio De Ferra", df2016$name[4])

df2016$placement[3] <- gsub("De Campos Pinto", "", df2016$placement[3])
df2016$name[3] <- gsub("Pedro Franco", "Pedro Franco De Campos Pinto", df2016$name[3])

# 2015 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2014-2015")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:23]

# Process the vector
df2015 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2015
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2015

# 2014 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2013-2014")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[4:18]

# Process the vector
df2014 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2014
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2014

# 2013 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2012-2013")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[4:18]

# Process the vector
df2013 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2013
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2013

# Correct bugs
df2013$field[15] <- paste("Macroeconomics (Primary), Applied Econometrics, Development Economics (Secondary)")

# 2012 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2011-2012")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:18]

# Process the vector
df2012 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2012
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2012

df2012$placement[2] <- "Economist, IMF"

df2012$placement[4] <- gsub("Matos", "", df2012$placement[4])
df2012$name[4] <- gsub("Ana Damas de", "Ana Damas de Matos", df2012$name[4])

df2012$placement[8] <- gsub("Miner", "", df2012$placement[8])
df2012$name[8] <- gsub("Luke Ian", "Luke Ian Miner", df2012$name[8])

df2012$placement[15] <- gsub("Eynde", "", df2012$placement[15])
df2012$name[15] <- gsub("Luke Ian", "Luke Ian Miner", df2012$name[15])

df2012$placement[16] <- gsub("CERGE-EIPostdoc", "CERGE-EI, before Postdoc", df2012$placement[16])

# 2011 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2010-2011")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:23]

# Process the vector
df2011 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2011
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2011 <- df2011 %>% drop_na()

df2011$placement[2] <- "Economist, IMF"

df2011$placement[12] <- gsub("Barreda", "", df2011$placement[12])
df2011$name[12] <- gsub("Ines Moreno de", "Ines Moreno de Barreda", df2011$name[12])

df2011$placement[14] <- gsub("Penczynski", "", df2011$placement[14])
df2011$name[14] <- gsub("Stefan P", "Stefan P Penczynski", df2011$name[14])


# 2010 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2009-2010")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[4:11]

# Process the vector
df2010 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2010
  ) %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2010 <- df2010 %>% drop_na()


# 2009 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2008-2009")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:17]

# Process the vector
df2009 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2009
  ) %>%
  drop_na() %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2009 <- df2009 %>% drop_na()

# 2008 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2007-2008")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:10]

# Process the vector
df2008 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2008
  ) %>%
  drop_na() %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2008 <- df2008 %>% drop_na()

df2008$placement[7] <- gsub("Santos", "", df2008$placement[7])
df2008$name[7] <- gsub("Carlos Daniel", "Carlos Daniel Santos", df2008$name[7])


# 2007 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2006-2007")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:14]

# Process the vector
df2007 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2007
  ) %>%
  drop_na() %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2007 <- df2007 %>% drop_na()

df2007$placement[8] <- gsub("Villalba", "", df2007$placement[8])
df2007$name[8] <- gsub("Miguel Sanchez", "Miguel Sanchez Villalba", df2007$name[8])

# 2006 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2005-2006")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:17]

# Process the vector
df2006 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2006
  ) %>%
  drop_na() %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2006 <- df2006 %>% drop_na()

df2006$placement[6] <- gsub("Silva", "", df2006$placement[6])
df2006$name[6] <- gsub("Afonso Goncalves da", "Afonso Goncalves da Silva", df2006$name[6])

# 2005 -------------------------------------------------------------------------

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2004-2005")
names <- web %>% html_nodes("p") %>% html_text()
names
names <- names[3:17]

# Process the vector
df2005 <- data.frame(strings = names) %>%
  mutate(
    name = sapply(strings, extract_name),
    placement = str_extract(strings, "(?i).*?(?=Research Interests)"),
    field = str_extract(strings, "(?i)(?<=Research Interests:).*"),
    year = 2005
  ) %>%
  drop_na() %>%
  mutate(
    placement = mapply(function(placement, name) gsub(name, "", placement), placement, name),
    field = gsub("(Primary)", "(Primary), ", field, fixed = TRUE),
    # Trim all variables
    name = str_trim(name),
    placement = str_trim(placement),
    field = str_trim(field),
    field = ifelse(substr(field, nchar(field), nchar(field)) == ",", substr(field, 1, nchar(field) - 1), field),
  ) %>%
  select(-strings)

df2005 <- df2005 %>% drop_na()

df2005$placement[4] <- gsub("De Laiglesia", "", df2005$placement[4])
df2005$name[4] <- gsub("Juan R", "Juan R De Laiglesia", df2005$name[4])

df2005$placement[5] <- gsub("Paoli", "", df2005$placement[5])
df2005$name[5] <- gsub("Bianca Shelton de", "Bianca Shelton de Paoli", df2005$name[5])

# Append -----
df_names <- paste0("df", 2010:2023)
final_table <- do.call("rbind", lapply(df_names, function(name) get(name)))

# Save ----
write_xlsx(final_table, paste0(data, "/eu/raw/lse_raw.xlsx"))

