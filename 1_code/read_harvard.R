

# Set paths 

source("_packages_paths.R")
setwd(paste0(data, "/us/raw/"))

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

data_final <- read_excel("harvard_raw.xls") # for merging

# Only keep first placement if the candidate goes twice on the market

data_final <- data_final %>%
  group_by(name) %>%
  slice(which.min(year))

#------------------------------ 2. Classify placements ------------------------------#

# Post-docs

postdocs <- c("Post-doc", "Postdoc", "Postdoctoral", "Post-doctoral", "postdoc", "Post Doc", "post-doc", "post doc")
postdocsnot <- c("after one-year postdoc", "post-doc 2018-2019", "post doc 2018-2019")

postdocs <- data_final %>% filter(grepl(paste(postdocs, collapse="|"), placement) & !grepl(paste(postdocsnot, collapse="|"), placement))
postdocs$placement_type1 <- "postdoc"
postdocs <- postdocs %>% select(name, placement_type1)

# Tenure-track

tenuretrack <- c("Business School", "University", "Department of Economics", "School of", "Yale SOM", "John Hopkins", 
                 "EIEF Rome", "Northwestern Kellogg", "Brown", "MIT Sloan", "UC Berkeley", "Harvard Kennedy School",
                 "Columbia Econ", "INSEAD", "Sciences Po", "Instituto Tecnológico Autónomo de México", "Harvard Law School",
                 "Dartmouth")
tenuretracknot <- c("Lecturer")

tenuretrack <- data_final %>% filter(grepl(paste(tenuretrack, collapse="|"), placement) & !grepl(paste(tenuretracknot, collapse="|"), placement))
tenuretrack$placement_type2 <- "tenure_track"
tenuretrack <- tenuretrack %>% select(name, placement_type2)

# International organizations

intl <- c("World Bank", "IMF", "UNU Wider", "International Monetary Fund", "Inter American Development Bank", 
          "Inter-American Development Bank", "European Bank for Reconstruction and Development", "OECD")
intl <- data_final %>% filter(grepl(paste(intl, collapse="|"), placement))
intl$placement_type3 <- "international_inst"
intl <- intl %>% select(name, placement_type3)

# Private sector

private <- c("Analysis Group", "Anaylsis Group", "Uber", "Amazon", "Facebook", "Air BnB", 
             "AirBnB", "Consulting", "Cornerstone", "Edgeworth Economics", "Mathematica", "Vanguard",
             "QuantCo", "Spotify", "Goldman Sachs", "Lyft", "Square Capital", "McKinsey", "Square Group",
             "American Road & Transportation Builders Association", "Upwork", "TrueCar", "Coursera",
             "Jane Street", "Farallon Capital", "Bain & Company", "Management Group", "Wayfair",
             "Dodge & Cox", "PrepScholar", "Wilson Perumal & Company")


private <- data_final %>% filter(grepl(paste(private, collapse="|"), placement))
private$placement_type4 <- "private"
private <- private %>% select(name, placement_type4)

# Central banks

centralbanks <- c("Bank of Spain", "Bank of Canada", "Bank of Chile", "Federal Reserve", "FRB",
                  "Central Bank", 'Bank of Korea', "Bank of Mexico", "FED", "Bank of Italy",
                  "Hong Kong Monetary Authority", "Federal Bank Reserve")
centralbanks <- data_final %>% filter(grepl(paste(centralbanks, collapse="|"), placement))
centralbanks$placement_type5 <- "central_bank"
centralbanks <- centralbanks %>% select(name, placement_type5)

# Think tanks

thinktanks <- c("Resources for the Future", "Opportunity Insights", "American Enterprise Institute", "Rand", "Hoover Institution")
thinktanks <- data_final %>% filter(grepl(paste(thinktanks, collapse="|"), placement))
thinktanks$placement_type6 <- "thinktank"
thinktanks <- thinktanks %>% select(name, placement_type6)

# Government

government <- c("Treasury", "Federal Trade Commission", "Congressional Budget Office","Joint Committee on Taxation", 
                "Committee for Public Counsel Services, Boston", "Boston Redevelopment Authority")

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

data_final[5:11] <- list(NULL) # remove unnecessary dummies

order <- c("year", "name", "field", "placement", "placement_type") # rearrange columns
data_final <- data_final[,order]

write_xlsx(data_final, "harvard.xls")

# Clean environment

setwd(paste0(code))
rm(list = ls())
