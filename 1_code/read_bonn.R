

# Set paths 

source("_packages_paths.R")
setwd(paste0(data, "/eu/raw/"))

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

data_final <- read_xlsx("bonn_raw.xlsx")

# Only keep first placement if the candidate goes twice on the market

data_final <- data_final %>%
  group_by(name) %>%
  slice(which.min(year))

#------------------------------ 2. Classify placements ------------------------------#

# Post-docs

postdocs <- c("Post-doc", "Postdoc", "Postdoctoral", "Post-doctoral", "postdoc")
postdocsnot <- c("Assistant Professor")

postdocs <- data_final %>% filter(grepl(paste(postdocs, collapse="|"), placement) & !grepl(paste(postdocsnot, collapse="|"), placement))

postdocs$placement_type1 <- "postdoc"
postdocs <- postdocs %>% select(name, placement_type1)

# Tenure-track

tenuretrack <- c("Assistant Professor", "Junior Professor", "Professor", "Business School")
tenuretracknot <- c("Visiting")

tenuretrack <- data_final %>% filter(grepl(paste(tenuretrack, collapse="|"), placement) & !grepl(paste(tenuretracknot, collapse="|"), placement))
tenuretrack$placement_type2 <- "tenure_track"
tenuretrack <- tenuretrack %>% select(name, placement_type2)
  
# International organizations

intl <- c("World Bank", "IMF", "UNU Wider", "International Monetary Fund", "WorldBank", "OECD")
intl <- data_final %>% filter(grepl(paste(intl, collapse="|"), placement))
intl$placement_type3 <- "international_inst"
intl <- intl %>% select(name, placement_type3)
  
# Private sector

private <- c("Consultant", "Deliveroo", "Uber", "Amazon", "Data Scientist", "Luohan Academy", 
             "Vivid Economics", "Academy", "Cornerstone", "Data Scientist",
             "Bank of America", "Analysis Group", "Company", "Instacart", "Resolution Economics",
             "Citadel", 'Mathematica', 'Brattle', "Deutshe Bank", 'Investment', "Upwork", "Oxera",
             "AlixPartners", "Citigroup", "Deloitte", "Price Waterhouse", "Price", "Gro Intelligence",
             "Consulting", "Analytics", "MSCI", "Facebook", "QuantCo", "Bates White", "DIW Econ",
             "Vanguard", "PIMCO", "Ernst & Young", "Strategic Analyst", "Risk Management",
             "Deutsche Postbank AG", "Quantitative Risk Analyst", "Reinsurance Actuary",
             "Software Developer", "AXA", "PwC", "Data Analyst", "Pricing Manager", "TWS Partners AG")
private <- data_final %>% filter(grepl(paste(private, collapse="|"), placement))
private$placement_type4 <- "private"
private <- private %>% select(name, placement_type4)

# Central banks

centralbanks <- c("Bank of Spain", "Bank of Canada", "Bank of Chile", "Federal Reserve", "FRB",
                  "Central Bank", 'Bank of Korea', "Bank of Mexico", "Bank of Portugal",
                  "Bank of England", "Banco de Portugal", "Board", "Bank of Slovenia", "Nationalbank",
                  "Central bank", "Swedish Riksbank", "Banque de France")

centralbanks <- data_final %>% filter(grepl(paste(centralbanks, collapse="|"), placement))
centralbanks$placement_type5 <- "central_bank"
centralbanks <- centralbanks %>% select(name, placement_type5)

# Think-tanks

thinktanks <- c("RAND", "Korean Advanced Institute", "Insitut der Deutschen Wirtschaft", "Kiel Insitute for the World Economy")
thinktanks <- data_final %>% filter(grepl(paste(thinktanks, collapse="|"), placement))
thinktanks$placement_type6 <- "thinktank"
thinktanks <- thinktanks %>% select(name, placement_type6)

# Government

government <- c("Treasury", "U.S. Department of", "US Department of", "Federal Trade Commission",
                "Korea Development", "Legislative", "Bureau", "Bundesanstalt für", "Bundeskartellamt",
                "Federal Ministry for Ecomomic Affairs", "Sachverständigenrat zur Begutachtung",
                "Statistisches Bundesamt")
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
  
data_final[4:10] <- list(NULL) # remove unnecessary dummies

write_xlsx(data_final, "bonn.xlsx")

# Clean environment

setwd(paste0(code))
rm(list = ls())