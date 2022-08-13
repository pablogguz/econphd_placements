
# Set paths 

source("_packages_paths.R")
setwd(paste0(data, "/us/raw/"))

jobs.list <- c('Professor',
               'Data Scientist',
               'Economist',
               'Post-doc',
               'Postdoc',
               'Postdoctoral',
               'Fellow',
               'Associate',
               'Consultant',
               'Research',
               'Lecturer',
               'Strategist',
               'World Bank',
               'IMF',
               'Scholar',
               'Analysis',
               'Federal Reserve',
               'UC Riverside',
               'Amazon',
               'School of Economics',
               'University',
               'College',
               'CSU Fullerton',
               'School of',
               'Uber',
               'RAND',
               'Business School',
               'Economics',
               'Research',
               'Institute',
               'FRB',
               'Researcher',
               'Consultant',
               'Bank of',
               'Senior',
               'Macroeconomist',
               'Microeconomist',
               'Econometrician',
               'Head',
               'CSU Fullerton',
               'Bank for International Settlements',
               'Bank for')

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

# Get names and year

web <- read_html("https://economics.ucla.edu/graduate/graduate-profiles/graduate-placement-history/")
name <- web %>% html_nodes(".av-special-heading-tag, .name") %>% html_text()
name <- as.data.frame(name)

vec_years <- c(2012:2022)
name$year <- ifelse(grepl(paste(vec_years, collapse="|"), name$name),
                     name$name,
                     NA) # generate year column

for (i in 1:nrow(name)) { # complete year column
  j <- i - 1
  if (j > 0 & is.na(name$year[i])) {
    name$year[i] <- name$year[j]
  }
}
  
name <- name %>% filter(!grepl(paste(vec_years, collapse="|"), name)) # remove rows with years within name variable
name <- name %>% mutate(n = row_number()) # for merging

# Get placement

placement <- web %>% html_nodes("td") %>% html_text()  
placement <- as.data.frame(placement) 
placement <- placement %>% filter(grepl(paste(jobs.list, collapse="|"), placement)) # keep if match with jobs list
placement <- placement %>% mutate(n = row_number()) # for merging

# Merge

data_final <- left_join(name, placement)
limit <- data_final %>% filter(name == "Andres Zambrano")
limit <- limit$n[[1]]
data_final <- data_final %>% filter(n <= limit) # get data only from 2012 onwards

# Only keep first placement if the candidate goes twice on the market

data_final <- data_final %>%
  group_by(name) %>%
  slice(which.min(year))

#------------------------------ 2. Classify placements ------------------------------#

# Post-docs  

postdocs <- c("Post-doc", "Postdoc", "Postdoctoral", "Post-doctoral", "postdoc", "Postdoctoral")
postdocsnot <- c("Assistant Professor","assistant professor")

postdocs <- data_final %>% filter(grepl(paste(postdocs, collapse="|"), placement) & !grepl(paste(postdocsnot, collapse="|"), placement))
postdocs$placement_type1 <- "postdoc"
postdocs <- postdocs %>% select(name, placement_type1)

# Tenure-track  

tenuretrack <- c("Assistant Professor", "Professor", "Lecturer", "School of",
                 "Business School", "CSU Fullerton", "UC Riverside", "Rensselaer", "USC Economics",
                 "Xiamen University", "of Hong Kong", "University of Sydney", "University of Mannheim",
                 "Rensselaer Polytechnic Institute", "Tehran Institute of Advanced Studies, Assistant Professor",
                 "University of International Business and Economics")
tenuretracknot <- c("Assistant Adjunct")

tenuretrack <- data_final %>% filter(grepl(paste(tenuretrack, collapse="|"), placement) & !grepl(paste(tenuretracknot, collapse="|"), placement))
tenuretrack$placement_type2 <- "tenure_track"
tenuretrack <- tenuretrack %>% select(name, placement_type2)

# International organizations  

intl <- c("World Bank", "IMF", "UNU Wider", "International Monetary Fund", "Bank for International Settlements")
intl <- data_final %>% filter(grepl(paste(intl, collapse="|"), placement))
intl$placement_type3 <- "international_inst"
intl <- intl %>% select(name, placement_type3)

# Private sector  

private <- c("Consultant", "Deliveroo", "Uber", "Amazon", "Data Scientist", "Luohan Academy", 
             "Vivid Economics", "Academy", "Cornerstone", "Data Scientist",
             "Bank of America", "Analysis Group", "Company", "Instacart", "Resolution Economics",
             "Citadel", 'Mathematica', 'Brattle', "Deutshe Bank", 'Investment', "Wayfair",
             "Charles River", "J.P. Morgan")
private <- data_final %>% filter(grepl(paste(private, collapse="|"), placement))
private$placement_type4 <- "private"
private <- private %>% select(name, placement_type4)

# Central banks  

centralbanks <- c("Bank of Spain", "Bank of Canada", "Bank of Chile", "Federal Reserve", "FRB",
                  "Central Bank", 'Bank of Korea', "Bank of Mexico")
centralbanks <- data_final %>% filter(grepl(paste(centralbanks, collapse="|"), placement))
centralbanks$placement_type5 <- "central_bank"
centralbanks <- centralbanks %>% select(name, placement_type5)

# Think tanks  

thinktanks <- c("RAND", "RAND, Associate Economist", "Korea Institute")
thinktanks <- data_final %>% filter(grepl(paste(thinktanks, collapse="|"), placement))
thinktanks$placement_type6 <- "thinktank"
thinktanks <- thinktanks %>% select(name, placement_type6)

# Government  

government <- c("U.S.", "Treasury", "U.S. Department of", "US Department of", "Federal Trade Commission",
                "Korea Development", "Legislative", "Korea Development Institute", "Institute for Defense Analyses",
                "Korea Information Socety Development Institute", "Korea Information Society")
government <- data_final %>% filter(grepl(paste(government, collapse="|"), placement))
government$placement_type7 <- "government"
government <- government %>% select(name, placement_type7)

# Merge in type dummies

data_final <- left_join(data_final, postdocs)
data_final <- left_join(data_final, tenuretrack)
data_final <- left_join(data_final, intl)
data_final <- left_join(data_final, private)
data_final <- left_join(data_final, centralbanks)
data_final <- left_join(data_final, thinktanks)
data_final <- left_join(data_final, government)

# Assign type 

data_final$placement_type <- data_final$placement_type1

data_final$placement_type <- ifelse(is.na(data_final$placement_type),
                                    data_final$placement_type2, 
                                    data_final$placement_type)

data_final$placement_type <- ifelse(is.na(data_final$placement_type),
                                    data_final$placement_type3, 
                                    data_final$placement_type)

data_final$placement_type <- ifelse(is.na(data_final$placement_type),
                                    data_final$placement_type4, 
                                    data_final$placement_type)

data_final$placement_type <- ifelse(is.na(data_final$placement_type),
                                    data_final$placement_type5, 
                                    data_final$placement_type)

data_final$placement_type <- ifelse(is.na(data_final$placement_type),
                                    data_final$placement_type6, 
                                    data_final$placement_type)

data_final$placement_type <- ifelse(is.na(data_final$placement_type),
                                    data_final$placement_type7, 
                                    data_final$placement_type)

data_final$placement_type <- ifelse(is.na(data_final$placement_type),
                                    "other", 
                                    data_final$placement_type)

#------------------------------ 3. Final cleaning and save ------------------------------#

data_final[5:9] <- list(NULL) # remove unnecessary dummies
data_final[3] <- list(NULL) # remove unnecessary variables

order <- c("year", "name", "placement", "placement_type") # rearrange columns
data_final <- data_final[,order]

write_xlsx(data_final, "ucla.xls")

# Clean environment

setwd(paste0(code))
rm(list = ls())

  