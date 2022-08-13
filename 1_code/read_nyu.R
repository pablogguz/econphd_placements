

# Set paths 

source("_packages_paths.R")
setwd(paste0(data, "/us/raw/"))

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

data_final <- read_xls("nyu_raw.xls") 

# No names available: create ID column

data_final <- data_final %>% mutate(name = row_number())

#------------------------------ 2. Classify placements ------------------------------#

# Post-docs

postdocs <- c("Post-doc", "Postdoc", "Postdoctoral", "Post-doctoral", "postdoc")
postdocsnot <- c("after ")

postdocs <- data_final %>% filter(grepl(paste(postdocs, collapse="|"), placement) & !grepl(paste(postdocsnot, collapse="|"), placement))
postdocs$placement_type1 <- "postdoc"
postdocs <- postdocs %>% select(name, placement_type1)
  
# Tenure-track

tenuretrack <- c("Assistant Professor", "Professor", "Lecturer", "School of",
                 "Business School", "CSU Fullerton", "UC Riverside", "Rensselaer", "USC Economics",
                 "University", "Pompeu Fabra", "Virginia Tech", "Chicago Booth", "UCSB", "School of",
                 "Vargas", "Universidad", "MIT", "UPF", "UCLA", "Penn State", "Haifa", "Nova", "Minnesota",
                 "Shanghai U Finance", "Hautes Etudes Commerciales", "Rochester Institute",
                 "Indian Institute of Technology", "Bristol", "Hitotsubashi", "Johns Hopkins", 
                 "Rochester", "Zhejiang", "UIUC", "CEMFI")
tenuretrack <- data_final %>% filter(grepl(paste(tenuretrack, collapse="|"), placement))
tenuretrack$placement_type2 <- "tenure_track"
tenuretrack <- tenuretrack %>% select(name, placement_type2)
  
# International organizations

intl <- c("World Bank", "IMF", "UNU Wider", "International Monetary Fund", "WorldBank", "OECD")
intl <- data_final %>% filter(grepl(paste(intl, collapse="|"), placement))
intl$placement_type3 <- "international_inst"
intl <- intl %>% select(name, placement_type3)
  
# Private sector

private <- c("Consultant", "Deliveroo", "Uber", "Amazon", "Data Scientist", "Luohan Academy", 
             "Vivid Economics", "Academy", "Cornerstone", "Data Scientist", "Associate",
             "Bank of America", "Analysis Group", "Company", "Instacart", "Resolution Economics",
             "Citadel", 'Mathematica', 'Brattle', "Deutshe Bank", 'Investment', "Upwork", "Oxera",
             "AlixPartners", "Citigroup", "Deloitte", "Price Waterhouse", "Price", "Gro Intelligence",
             "Consulting", "Analytics", "MSCI", "Facebook", "QuantCo", "Bates White", "DIW Econ",
             "Vanguard", "PIMCO", "Wayfair", "LinkedIn")

private <- data_final %>% filter(grepl(paste(private, collapse="|"), placement))
private$placement_type4 <- "private"
private <- private %>% select(name, placement_type4)

# Central banks

centralbanks <- c("Bank of Spain", "Bank of Canada", "Bank of Chile", "Federal Reserve", "FRB",
                  "Central Bank", 'Bank of Korea', "Bank of Mexico", "Bank of Portugal",
                  "Bank of England", "Banco de Portugal", "Board")
centralbanks <- data_final %>% filter(grepl(paste(centralbanks, collapse="|"), placement))
centralbanks$placement_type5 <- "central_bank"
centralbanks <- centralbanks %>% select(name, placement_type5)
  
# Think tanks

thinktanks <- c("RAND", "Korean Advanced Institute")
thinktanks <- data_final %>% filter(grepl(paste(thinktanks, collapse="|"), placement))
thinktanks$placement_type6 <- "thinktank"
thinktanks <- thinktanks %>% select(name, placement_type6)
  
# Government

government <- c("Treasury", "U.S. Department of", "US Department of", "Federal Trade Commission",
                "Korea Development", "Legislative", "Bureau")
government <- data_final %>% filter(grepl(paste(government, collapse="|"), placement))
government$placement_type7 <- "government"
government <- government %>% select(name, placement_type7)

# Merge in 
  
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
  
data_final[4:10] <- list(NULL) # clean dataset

order <- c("year", "name", "placement", "placement_type") # rearrange columns
data_final <- data_final[,order]

write_xlsx(data_final, "nyu.xls")

# Clean environment

setwd(paste0(code))
rm(list = ls())


