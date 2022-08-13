

# Set paths 

source("_packages_paths.R")
setwd(paste0(data, "/us/raw/"))

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

data_final <- read_excel("minnesota_raw.xls") # for merging

# Only keep first placement if the candidate goes twice on the market

data_final <- data_final %>%
  group_by(name) %>%
  slice(which.min(year))

#------------------------------ 2. Classify placements ------------------------------#


# Post-docs

postdocs <- c("Post-doc", "Postdoc", "Postdoctoral", "Post-doctoral", "postdoc", "Post Doc",
              "Post")
postdocs <- data_final %>% filter(grepl(paste(postdocs, collapse="|"), placement))
postdocs$placement_type1 <- "postdoc"
postdocs <- postdocs %>% select(name, placement_type1)
  
# Tenure-track

tenuretrack <- c("Assistant Professor", "Professor", "Lecturer", "School of",
                 "Business School", "CSU Fullerton", "UC Riverside", "Rensselaer", "USC Economics", "University",
                 "Cal State Fullerton", "Emory", "Tsinghua", "Universidad", "ITAM", "Virginia Tech", "Autonoma Barcelona",
                 "INSEAD", "College", "Penn State", "International Economics, EPFL, Lausanne", "Department of Economics, William & Mary",
                 "Cornell Economics", "Louisiana State Univeristy")
tenuretracknot <- c("Visiting")

tenuretrack <- data_final %>% filter(grepl(paste(tenuretrack, collapse="|"), placement) & !grepl(paste(tenuretracknot, collapse="|"), placement))
tenuretrack$placement_type2 <- "tenure_track"
tenuretrack <- tenuretrack %>% select(name, placement_type2)

# International organizations

intl <- c("World Bank", "IMF", "UNU Wider", "International Monetary Fund", "Inter American Development Bank", 
          "Inter-American Development Bank", "European Bank for Reconstruction and Development", 
          "Organisation for Economic Co-operation and Development", "Bank for International Settlements", "OECD")
intl <- data_final %>% filter(grepl(paste(intl, collapse="|"), placement))
intl$placement_type3 <- "international_inst"
intl <- intl %>% select(name, placement_type3)
  
# Private sector

private <- c("Consultant", "Deliveroo", "Uber", "Amazon", "Data Scientist", "Luohan Academy", 
             "Vivid Economics", "Academy", "Cornerstone", "MSCI", "PwC", "Charles River",
             "Bank of America", "Analysis Group", "Company", "Instacart", "Resolution Economics",
             "Citadel", 'Mathematica', 'Brattle', "Deutshe Bank", 'Investment', "JD.com", "Capital One",
             "QuantCo", "IMPAQ", "Black Rock", "Afiniti", "Vanguard", "Kaneka Holdings", "WhatsApp", "AQR Capital",
             "CNA", "Pricewaterhouse", "Twitter", "CNA", "Compass Lexecon", "SOLO World Partners, LCC", "Ford Motor",
             "Bates White", "Mercer", "Keystone", "KPMG", "Samsung", "Ernst & Young", "Axioma", "Captial One", "HealthCore",
             "Twin Beech Capital", "Cooper/Smith", "ISO New England", "Microsoft", "AVP Risk Management", "Agrowealth", "Mathmatica",
             "Vitalite Zambia Limited", "Boston Consulting Group", "Siam Commercial Bank", "Wayfair", "Facebook", "AidData",
             "BlackRock", "Citi Bank", "Morningstar", "BBVA Research", "United Parcel Service")
private <- data_final %>% filter(grepl(paste(private, collapse="|"), placement))
private$placement_type4 <- "private"
private <- private %>% select(name, placement_type4)

# Central banks

centralbanks <- c("Bank of Spain", "Bank of Canada", "Bank of Chile", "Federal Reserve", "FRB",
                  "Central Bank", 'Bank of Korea', "Bank of Mexico", "FED", "Bank of Italy", "Banque de France", 
                  "Bank of Thailand", "Bank of Portugal", "Bank of England")
centralbanks <- data_final %>% filter(grepl(paste(centralbanks, collapse="|"), placement))
centralbanks$placement_type5 <- "central_bank"
centralbanks <- centralbanks %>% select(name, placement_type5)
  
# Think tanks

thinktanks <- c("Institute", "RAND", "RAND, Associate Economist", "Tax Policy Center", "IFPRI, Poverty Health and Nutrition Division",
                "Resources for the Future")
thinktanks <- data_final %>% filter(grepl(paste(thinktanks, collapse="|"), placement))
thinktanks$placement_type6 <- "thinktank"
thinktanks <- thinktanks %>% select(name, placement_type6)

# Government

government <- c("U.S.", "Treasury", "U.S. Department of", "US Department of", "Federal Trade Commission",
                "Korea Development", "Legislative", "US Naval Academy", "Ministry", "Congressional Budget Office",
                "United States Census Bureau", "Securities and Exchange Commission", "Michigan Department of Education",
                "Center on Budget", "Joint Committee on Taxation", "Commodity Futures Commission", "Food & Drug Administration",
                "CA State Government", "USAID", "Center for Governmental Research", "Japan International Cooperation Agency",
                "Federal Energy Regulatory Commission", "Census Bureau", "National Renewable Energy Laboratory", 
                "Bureau of Labor Statistics", "Department of Health and Human Services", "Federal Deposit Insurance Corporation",
                "USDA", "Minnesota Department of Commerce", "Bureau of Economic Analysis", "Minister of Finance of Mexico",
                "Government of Japan", "Federal Housing Finance")
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
  
data_final[4:10] <- list(NULL) # remove unnecessary dummies
data_final <- data_final %>% filter(!is.na(placement)) # drop NAs for placement
data_final <- data_final %>% filter(year>=2012) # keep data from 2012 onward

order <- c("year", "name", "placement", "placement_type") # rearrange columns
data_final <- data_final[,order]

write_xlsx(data_final, "minnesota.xls")

# Clean environment

setwd(paste0(code))
rm(list = ls())

