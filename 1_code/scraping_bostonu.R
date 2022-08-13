

# Set paths 

source("_packages_paths.R")
setwd(paste0(data, "/us/raw/"))

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

# Get names, placement and year

web <- read_html("https://www.bu.edu/econ/academics/recent-phd-placements/")
dataweb <- web %>% html_nodes("h3,td") %>% html_text()
dataweb <- as.data.frame(dataweb)
dataweb <- dataweb %>% filter(row_number() > 1) # removed unused lines
dataweb$dataweb <- str_trim(dataweb$dataweb ) # trim
dataweb$year <- str_sub(dataweb$dataweb , start = -4) # extract year

vec_years <- c(2004:2022)
dataweb$year <- ifelse(grepl(paste(vec_years, collapse="|"), dataweb$year),
                       dataweb$year,
                     NA) # generate year column

for (i in 1:nrow(dataweb)) { # complete year column
  j <- i - 1
  if (j > 0 & is.na(dataweb$year[i])) {
    dataweb$year[i] <- dataweb$year[j]
  }
}
dataweb <- dataweb %>% filter(!grepl(paste(vec_years, collapse="|"), dataweb)) # remove rows with years within name variable

dataweb <- dataweb %>% mutate(n = row_number())

placement <- dataweb %>% filter(is.odd(n)) # placement is on odd rows
placement <- placement %>% mutate(n = row_number())
placement <- placement %>% rename(placement = dataweb)

name <- dataweb %>% filter(is.even(n)) # name is on even rows
name <- name %>% mutate(n = row_number())
name <- name %>% rename(name = dataweb)

data_final <- left_join(placement, name)
  
# Only keep first placement if the candidate goes twice on the market

data_final <- data_final %>%
  group_by(name) %>%
  slice(which.min(year))

#------------------------------ 2. Classify placements ------------------------------#
  
# Post-docs

postdocs <- c("Post-doc", "Postdoc", "Postdoctoral", "Post-doctoral", "postdoc", "Post Doc",
              "Post", "post-doc", "(post doc)", "post doc", "(post doc)")
postdocsnot <- c("Assistant Professor","assistant professor")

postdocs <- data_final %>% filter(grepl(paste(postdocs, collapse="|"), placement) & !grepl(paste(postdocsnot, collapse="|"), placement))
postdocs$placement_type1 <- "postdoc"
postdocs <- postdocs %>% select(name, placement_type1)
  
# Tenure-track

tenuretrack <- c("Assistant Professor", "Professor", "ECARES", "(asst. prof.)", "Indian Institute of managements",
                 "lecturer", "University", "Indian Institute of Management", "London School of Economics, UK",
                 "Babson College, Finance Department, MA", "New College of Florida")
tenuretracknot <- c("adjunct professor", "visiting")

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
             "BlackRock", "Citi Bank", "Morningstar", "BBVA Research", "United Parcel Service", "Moody", "consultant",
             "Consultant", "Quantitative Researcher", "Scientist", "Morgan Stanley", 	"JP Morgan",
             "State Street", "Santander", "Alibaba", "Insurance", "The Clearing House", "Fontana Group", "Digonex Technologies")
private <- data_final %>% filter(grepl(paste(private, collapse="|"), placement))
private$placement_type4 <- "private"
private <- private %>% select(name, placement_type4)
  
# Central banks
  
centralbanks <- c("Bank of Spain", "Bank of Canada", "Bank of Chile", "Federal Reserve", "FRB",
                  "Central Bank", 'Bank of Korea', "Bank of Mexico", "FED", "Bank of Italy", "Banque de France", 
                  "Bank of Thailand", "Bank of Portugal", "Bank of England", "Reserve Bank of India")
centralbanks <- data_final %>% filter(grepl(paste(centralbanks, collapse="|"), placement))
centralbanks$placement_type5 <- "central_bank"
centralbanks <- centralbanks %>% select(name, placement_type5)
  
# Think tanks

thinktanks <- c("Institute", "RAND", "RAND, Associate Economist", "Tax Policy Center", "IFPRI, Poverty Health and Nutrition Division",
                "Resources for the Future", "The Economic Policy Research Foundation of Turkey")
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
                "Government of Japan", "Department of Justice, DC", "Government Accountability", "Department Of Justice")
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

data_final[3] <- list(NULL) # remove numeric ID
data_final[4:10] <- list(NULL) # remove unnecessary dummies
data_final <- data_final %>% filter(!is.na(placement)) # drop NAs for placement
data_final <- data_final %>% filter(year>=2012) # keep data from 2012 onward

order <- c("year", "name", "placement", "placement_type") # rearrange columns
data_final <- data_final[,order]

write_xlsx(data_final, "boston.xls")
  
# Clean environment

setwd(paste0(code))
rm(list = ls())
