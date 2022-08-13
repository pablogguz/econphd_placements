

# Set paths 

source("_packages_paths.R")
setwd(paste0(data, "/eu/raw/"))

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

data_final <- read_excel("upf_raw.xlsx") # for merging

# Only keep first placement if the candidate goes twice on the market

data_final <- data_final %>%
  group_by(name) %>%
  slice(which.min(year))

#------------------------------ 2. Classify placements ------------------------------#

# Post-docs

postdocs <- c("Post-doc", "Postdoc", "Postdoctoral", "Post-doctoral", "postdoc", "Post Doc", "post-doc", 
              "post doc", "Post-Doc", "PostDoc", "Post", "Port Doctoral")
postdocsnot <- c("after one-year postdoc", "first, one-year ", "after Postdoc", " then ", "Assistant Professor")

postdocs <- data_final %>% filter(grepl(paste(postdocs, collapse="|"), placement) & !grepl(paste(postdocsnot, collapse="|"), placement))
postdocs$placement_type1 <- "postdoc"
postdocs <- postdocs %>% select(name, placement_type1)

# Tenure-track

tenuretrack <- c("Assistant Professor", "Assistant professor", "Lecturer", "University", "School of", "ESADE",
                 "Business School", "Imperial College London", "Universitat", "Université", "Universiteit", "Universidad",
                 "Assitant Professor")

tenuretracknot <- c("Teaching-Track", "Adjunct Professor", "Research Associate", "Port Doctoral", "Visiting")

tenuretrack <- data_final %>% filter(grepl(paste(tenuretrack, collapse="|"), placement) & !grepl(paste(tenuretracknot, collapse="|"), placement))
tenuretrack$placement_type2 <- "tenure_track"
tenuretrack <- tenuretrack %>% select(name, placement_type2)

# International organizations

intl <- c("World Bank", "IMF", "UNU Wider", "International Monetary Fund", "Inter American Development Bank", 
          "Inter-American Development Bank", "European Bank for Reconstruction and Development", "OECD",
          "Bank for International Settlements", "International Growth Centre", "Organization for Economic Co-operation and Development",
          "Latin American Development Bank Research Group", "International Labour Organization")
intl <- data_final %>% filter(grepl(paste(intl, collapse="|"), placement))
intl$placement_type3 <- "international_inst"
intl <- intl %>% select(name, placement_type3)

# Private sector

private <- c("Analysis Group", "Anaylsis Group", "Uber", "Amazon", "Facebook", "Air BnB", 
             "AirBnB", "Consulting", "Cornerstone", "Edgeworth Economics", "Mathematica", "Vanguard",
             "QuantCo", "Spotify", "Goldman Sachs", "Lyft", "Square Capital", "McKinsey", "Square Group",
             "American Road & Transportation Builders Association", "Upwork", "TrueCar", "Coursera",
             "Jane Street", "Farallon Capital", "Bain & Company", "Management Group", "Wayfair",
             "Dodge & Cox", "PrepScholar", "Wilson Perumal & Company", "Deliveroo", "Google",
             "Meta", "Sidley Austin", "LinkedIn", "Safra Bank", "Ping An Bank", "PIMCO", "Investment",
             "REMI", "Quantco", "IDInsight", "IDinsight", "Charles River", "Asset Management", "Instacart", 
             "Asset Management", "Airbnb", "Nuna Health", "NERA", "Zillow", "Capital Management",
             "Ohmconnect", "Netflix", "Keystone Strategy", "SiriusXM/Pandora", "Bates White",
             "Root Insurance", "Revelio Labs", "JP Morgan", "WorldQuant", "Deutsche Bank",
             "D.E. Shaw", "Citigroup", "Wealthfront", "Brattle Group", "Bank of America", "BlackRock",
             "Investimentos", "Capital One", "Mastercard", "Barclays", "Convoy", "Prysm Group",
             "Compass Lexecon", "Diagnostic Robotics","Quant Economics", "Caixa Bank", "Blue Labs",
             "QFR Capital", "Pandora Media", "Deloitte", "Robinhood", "Pandora", "Alvarez and Marsal",
             "Analytics", "True North Managers LLP", "Black Rock", "Bloomberg", "Citadel",
             "Software Engineer", "Data Scientist", "National Economic Research Associates", "Ellington Management",
             "KPMG", "PwC", "PricewaterhouseCoopers", "Dimensional Fund Advisors", "Oxera", "Private Sector",
             "CaixaBank")

private <- data_final %>% filter(grepl(paste(private, collapse="|"), placement))
private$placement_type4 <- "private"
private <- private %>% select(name, placement_type4)

# Central banks

centralbanks <- c("Bank of Spain", "Bank of Canada", "Bank of Chile", "Federal Reserve", "FRB",
                  "Central Bank", 'Bank of Korea', "Bank of Mexico", "FED", "Bank of Italy",
                  "Hong Kong Monetary Authority", "Federal Bank Reserve", "Bank of Norway",
                  "Norges Bank", "GiveWell", "Banco de Mexico", "Bank of England", "Federal Board",
                  "Banque de France", "Boston Fed", "St Louis Fed", "Bank of Lithuania", "Banco Central",
                  "Bundesbank","Banca d'Italia", "Banco de México", "Bank of Australia")
centralbanks <- data_final %>% filter(grepl(paste(centralbanks, collapse="|"), placement))
centralbanks$placement_type5 <- "central_bank"
centralbanks <- centralbanks %>% select(name, placement_type5)

# Think tanks

thinktanks <- c("Resources for the Future", "Opportunity Insights", "American Enterprise Institute", "Rand", 
                "Hoover Institution", "Center for Global Development", "Upjohn Institute", "RAND", "RFF",
                "Korea Institute", "Employ America", "Public Policy Institute of California", "Urban Institute",
                "Korean Institute of Finance", "Data for Progress", "Research Alliance")
thinktanks <- data_final %>% filter(grepl(paste(thinktanks, collapse="|"), placement))
thinktanks$placement_type6 <- "thinktank"
thinktanks <- thinktanks %>% select(name, placement_type6)

# Government

government <- c("Treasury", "Federal Trade Commission", "Congressional Budget Office","Joint Committee on Taxation", 
                "Committee for Public Counsel Services, Boston", "Boston Redevelopment Authority",
                "Federal District Court", "U.S. Census Bureau", "US Naval Academy", "Consumer Financial Protection Bureau",
                "U.S. Department of ", "Federal Deposit Insurance Corporation", "Korea Development Institute",
                "Federal Housing Finance Agency", "U.S. Bureau ", "US Census Bureau", "Government Accountability Office",
                "SEC", "Federal ", "Securities and Exchange Commission", "Ministry of", "Department of Justice",
                "Commodity Futures Trading Commission", "Community Service Society of New York", "U.S. Naval Academy",
                "Office of Management and Budgets of New York", "Veterans Administration", "Researcher National Planning Department")

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

order <- c("year", "name", "placement", "placement_type") # rearrange columns
data_final <- data_final[,order]

write_xlsx(data_final, "upf.xlsx")

# Clean environment

setwd(paste0(code))
rm(list = ls())
