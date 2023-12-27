
#--------------------------------------------------------------#
# Project: Placement outcomes for PhD graduates in Economics
# Author: Pablo Garcia-Guzman

# This script: 
#   Appends scraped data
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

# Define function to read and process files
process_file <- function(file_path, region) {
  
  # Extract institution name from file name
  inst_name <- tools::file_path_sans_ext(basename(file_path))
  inst_name <- gsub("_raw", "", inst_name)
  
  # Read the file
  df <- readxl::read_excel(file_path)
  
  # Convert year to numeric if it's not already
  df$year <- as.numeric(as.character(df$year))
  
  # Add columns for institution and region
  df <- df %>%
    mutate(inst = inst_name, region = region)
  
  return(df)
}

# Gather all file paths
us_files <- list.files(path = paste0(data, "us/raw/"), full.names = TRUE, pattern = "\\.xlsx$")
eu_files <- list.files(path = paste0(data, "eu/raw/"), full.names = TRUE, pattern = "\\.xlsx$")

# Process each file and append
all_data <- bind_rows(
  lapply(us_files, process_file, region = "us"),
  lapply(eu_files, process_file, region = "eu")
)

# Clean
all_data <- all_data %>%
  mutate(placement = str_trim(placement)) %>%
  filter(!is.na(placement)) %>%
  # Remove leading and trailing comma or period
  mutate(placement = str_replace_all(placement, "^[,.\\-]+|[,.\\-]+$", ""),
         field = str_replace_all(field, "^[,.\\-]+|[,.\\-]+$", "")) %>%
  mutate(placement = str_trim(placement),
         field = str_trim(field))

# Text classification algorithm ----

# Function to classify placements

classify_placement <- function(df, debug = FALSE) {
  
  # Define keywords for each category
  
  postdocs_keywords <- c("Post-doc", "Postdoc", "Postdoctoral", "Post-doctoral", "postdoc", "Post Doc",
                         "Visiting", "Post Doctoral", "Post-doctural", "Max Planck", "(Post-doc)", "PostDoc",
                         "Post-doctorate",  "Research Fellow, University", "teaching fellow",
                         "career development fellowship", "Career development fellow", "max weber fellow")
  postdocsnot_keywords <- c("after ", "before", "then", "assistant professor", "tenure") # account for the fact that some placements involve a post-doc and then the tenure-track position
  
  tenure_track_keywords <- c("Assistant Professor", "Professor", "Lecturer", 
                             "School of", "Business School", "University", 
                             "College", "Department of Economics", 
                             "Economics Department", "Universidad", "HEC", "FGV",
                             "New Economic School", "Yale SOM", "John Hopkins", 
                             "EIEF Rome", "Northwestern Kellogg", "Brown", "MIT Sloan", "UC Berkeley", "Harvard Kennedy School",
                             "Columbia Econ", "INSEAD", "Sciences Po", "Instituto Tecnológico Autónomo de México", "Harvard Law School",
                             "Dartmouth", "LMU Munich", "Vanderbilt", "NYU", "MIT", "CEMFI", "Boston College",
                             "Universitat Pompeu Fabra", "USC", "Chicago Booth", "UC ", "UNLV", "UT Austin", 
                             "LSE", "UCLA", "UBC Sauder", "Georgia Institute of Technology", "Kellogg", "Penn State", 
                             "Carnegie Mellon", "Wharton", "Michigan State", "UGA", "McMaste", "Princeton", "Toronto", "HEC Paris",
                             "ITAM", "Columbia", "CSU Fullerton", "UC Riverside", "Rensselaer", "USC Economics",
                             "Pompeu Fabra", "Virginia Tech", "UCSB", "Vargas", "UPF", "Haifa", "Nova", "Minnesota",
                             "Shanghai U Finance", "Hautes Etudes Commerciales", "Rochester Institute",
                             "Indian Institute of Technology", "Bristol", "Hitotsubashi", "Johns Hopkins", 
                             "Rochester", "Zhejiang", "UIUC", "International Economics, EPFL, Lausanne", "Department of Economics, William & Mary",
                             "Cornell Economics", "Louisiana State University", "SUNY Binghamton", "United States Naval Academy", "UIBE", "Université",
                             "CSEF Naples", "Caltech", "Colegio de Mexico", "Universidade", "Polytechnique", "Universitat", "Notre Dame",
                             "Academia Sinica", "Naval Postgraduate", "HKUST", "CREI", "Assistant Professorship", "CREST", "UNC Chapel Hill",
                             "Cornell ILR", "SUNY Albany", "Collegio Carlo Alberto", "Berkeley", "Stanford", "Einaudi", "Colorado State",
                             "Colorado State", "ohio state", "Middlebury", "SUNY", "Harvard", "Geneva Graduate Institute", "IESE",
                             "institute of technology", "Universiteit", "Cal State", "Automoma Barcelona", "Indian Institute of Management",
                             "Kiel Institute for World Economy", "CERGE-EI", "Tilburg", "Montreal", "Autonoma Barcelona/Move",
                             "Polytechnic Institute", "CCNY", "Texas A&M", "CSU Sacramento", "Inc,", "Convertro", 
                             "IMT Lucca Institute for Advanced Studies", "Georgetown", "Tsinghua SEM", "Wake Forest", "Wesleyan",
                             "Weill Cornell Medicine", "Korean Advanced Institute for Science & Technology", "Indiana Univiversity",
                             "Indian Statistical Institute", "Hong Kong Univ", "ISET in Tbilisi, Georgia", "Georgia Tech",
                             "Northeastern", "West Point", "Yale", "UCSC", "Torcuato di Tella", "HKU", "Clemson", "NUS",
                             "Stockholm Institute for International Economic Studies", "Rutgers")

  intl_org_keywords <- c("World Bank", "IMF", "UNU Wider", "International Monetary Fund", 
                         "Inter American Development Bank", "European Bank for Reconstruction and Development", 
                         "OECD", "Bank for International Settlements", "EBRD", "IDB", "IADB",
                         "Development Impact Evaluation Initiative", "European Commission", "Organization for Economic Co-operation and Development",
                         "United Nations", "WorldBank", "Organisation for Economic Co-operation and Development",
                         "European Stability Mechanism", "European Securities and Markets Authority",
                         "Organization for Economic Cooperation and Development")
  
  private_sector_keywords <- c("Consultant", "Data Scientist", "Associate", "Bank", "Group", "Company", "Investment", "Capital",
                               "Deliveroo", "Uber", "Amazon", "Luohan Academy", "Vivid Economics", "Academy", "Cornerstone",
                               "Bank of America", "Analysis Group", "Instacart", "Resolution Economics", "Citadel", "Mathematica", 
                               "Brattle", "Deutshe Bank", "JD.com", "Capital One", "QuantCo", "IMPAQ", "Black Rock", "Afiniti", 
                               "Vanguard", "Kaneka Holdings", "WhatsApp", "AQR Capital", "CNA", "Pricewaterhouse", "Twitter", 
                               "Compass Lexecon", "SOLO World Partners, LCC", "Ford Motor", "Bates White", "Mercer", "Keystone", 
                               "KPMG", "Samsung", "Facebook", "Air BnB", "AirBnB", "Consulting", "Edgeworth Economics", 
                               "Spotify", "Goldman Sachs", "Lyft", "Square Capital", "McKinsey", "Square Group", 
                               "American Road & Transportation Builders Association", "Upwork", "TrueCar", "Coursera", 
                               "Jane Street", "Farallon Capital", "Bain & Company", "Management Group", "Wayfair", 
                               "Dodge & Cox", "PrepScholar", "Wilson Perumal & Company", "Google", "Meta", "Sidley Austin", 
                               "LinkedIn", "Safra Bank", "Ping An Bank", "PIMCO", "REMI", "Quantco", "IDInsight", "Charles River", 
                               "Asset Management", "Instacart", "Airbnb", "Nuna Health", "NERA", "Zillow", "Capital Management", 
                               "Ohmconnect", "Netflix", "Keystone Strategy", "SiriusXM/Pandora", "Root Insurance", "Revelio Labs", 
                               "JP Morgan", "WorldQuant", "Deutsche Bank", "D.E. Shaw", "Citigroup", "Wealthfront", "Brattle Group", 
                               "Bank of America", "BlackRock", "Investimentos", "Capital One", "Mastercard", "Barclays", "Convoy", 
                               "Prysm Group", "Compass Lexecon", "Diagnostic Robotics", "Quant Economics", "Caixa Bank", "Blue Labs", 
                               "QFR Capital", "Pandora Media", "Deloitte", "UBS", "Data Science", "Bloomber", "Moody's", "Oxera", 
                               "AlixPartners", "Price Waterhouse", "Price", "Gro Intelligence", "Analytics", "MSCI", "DIW Econ", 
                               "Twin Beech Capital", "Cooper/Smith", "ISO New England", "Microsoft", "AVP Risk Management", "Agrowealth",
                               "Vitalite Zambia Limited", "Boston Consulting Group", "Siam Commercial Bank", "AidData", "Citi Bank", 
                               "Morningstar", "PwC", "Barclay's", "Numeric Investors", "Abt", ", LLc", "Freddie Mac", "PricewaterhouseCoopers",
                               "Rimeto", "Datadog", "Captial One", "HealthCore", "Investments", "Axioma", "Ernst & Young", "Deliotte",
                               "Mathmatica", "CEO", "Co-Founder", "Citicards", "Lehman", "NESTA", "State Street", "Syncsort", "Bloomberg",
                               "Ernest & Young", "Thomson Reuters", "MedStat", "Inc.", "Two Sigma", "Morgan Stanley", "Citi", "Fund Advisors",
                               "BNY Mellon", "Give Well", "LLC", "Insight Policy Research", "Power Auctions", "Rothschild", "Ofcom",
                               "Private sector", "LLP", "Pandora", "Arbella Insurance", "Technologies", "Alibaba", "Economists Incorporated",
                               "Nathan Associates", "Integrated Finance Limited", "Intensity", "Intel", "Merck Research", "Strategy Consultants",
                               "Paulson & Co", "CRA", "Cornerston", "Quant Co", "Charles Rivers", "Copenhagen Economics", "Compass", "Credit Swiss", 
                               "Yunta Securities", "Invesco", "BBVA", "Walmart", "Lexecon", "DHL", "Telekom", "Fannie Mae", "Frontier economics",
                               "Swissre", "T-Mobile", "Cubist Systemic Strategies", "Franklin Templeton", "TWS Partners AG", "Raifeisenbank International",
                               "Schonfeld Strategic Advisors", "Investors", "Ellington Management", "Rivada Networks", "Lingoda", "Inverstors",
                               "Robert Bosch", "Talanx AG", "National Economic Research Associates", "Unibanco", "AXA", "Reinsurance Actuary",
                               "node.energy", "Constellation Energy", "Oakland Athletics", "Hanover Research Council", "Economic Analysis Corporation",
                               "Secretariat Economists", "ESMT Competition Analysis", "Korean Advanced Institute of Science and Technology",
                               "Legal Economics", "The Clearing House", "Worthix", "United Parcel Service", "LG Economic Research", 
                               "Economics Analysis, Los Angeles, Economist", "Ashenfelter", "Planbase", "CVS Health", "(ING)", "Happy Elements",
                               "JPMorgan", "Indeed", "Entrepreneur First", "Credit Suisse", "Swiss Re", "Lupa Textiles", "Zalando",
                               "Baloise Insurance", "Marshall Wace", "Unbound Potential", "Wüest Partner", "Swiss Life", "Sanitas",
                               "BKW Energie AG")
  
  central_banks_keywords <- c("Bank of Spain", "Federal Reserve", "FRB", "Central Bank", "Monetary Auth
                              ority",
                              "Bank of Canada", "Bank of Chile", 'Bank of Korea', "Bank of Mexico", "FED", 
                              "Bank of Italy", "Hong Kong Monetary Authority", "Federal Bank Reserve", 
                              "Bank of Norway", "Norges Bank", "Banco de Mexico", "Bank of England", 
                              "Federal Board", "Fed Board", "Banco de", "Banca d'Italia", "Bundesbank", "Banque de France",
                              "Banco central", "Nationalbank", "Swedish Riksbank")
  
  think_tanks_keywords <- c("RAND", "Policy Center", "IFPRI", "Resources for the Future", "Opportunity Insights",
                            "Resources for the Future", "Opportunity Insights", "American Enterprise Institute", "Rand", 
                            "Hoover Institution", "Center for Global Development", "Upjohn Institute", "RAND", "RFF",
                            "Korea Institute", "Brookings", "International Growth Centre", "Iseak", "APPRISE Incorporated",
                            "data for progress", "International Food Policy Research Institute", "Research Triangle Institute",
                            "prosperity now", "RTI", "RIPL", "Public Policy Institute of California", "Employ America", "TEPAV",
                            "American Institutes for Research", "Baker Institute", "CRI Foundation", "MITRE", "Community Service Society of New York",
                            "Fondazione Bruno Kessler", "Korea Information Strategy Development Institute", "Research Triangle Inst", "Motu Economics",
                            "Netzwerk  Steuergerechtigkeit (NGO-Berlin)", "SPHERE Institute", "The Institute for the Study of Labor",
                            "Pew Research", "Recividiz", "Institute for Fiscal Studies", "Institute of Fiscal Studies",
                            "Instituto para la Planeacion del Desarrollo", "GiveWell", "Recidiviz", "NGO-Berlin",
                            "Center on Budget and Policy Priorities", "German Economic Institute")
  
  government_keywords <- c("Treasury", "Department of", "Federal Trade Commission", "Ministry", "Census Bureau", "Securities and Exchange Commission",
                           "Legislative", "Treasury", "Federal Trade Commission", "Congressional Budget Office","Joint Committee on Taxation", 
                           "Committee for Public Counsel Services, Boston", "Boston Redevelopment Authority",
                           "Federal District Court", "U.S. Census Bureau", "US Naval Academy", "Consumer Financial Protection Bureau",
                           "U.S. Department of", "Federal Deposit Insurance Corporation", "Korea Development Institute",
                           "Federal Housing Finance Agency", "U.S. Bureau", "US Government", "Bureau of Economic Analysis",
                           "Government Accountability", "Prime Minister", "FTC", "Bureau of Labor Statistics", "Board of Governors",
                           "U.S. Securities & Exchange", "Korean Development Institute", "SEC", "Security & Exchange", "FDA", 
                           "Office of Management and Budgets of New York", "State Administration of Foreign Exchange", "U.S. Intelligence Community",
                           "Census", "Minister of", "International Trade Commission", "Federal Cartel Office",
                           "Australian Bureau of Statistics", "USDA", "Federal", "U.S. General Accounting", "Security and Exchange Commission",
                           "Japan International Cooperation Agency", "KIEP", "Korea Information Society Development Institute", "KISDI", "FDIC",
                           "Equal Employment Opportunity Commission", "USAID", "Office of Comptroller of Currency", "Korea Labor Institute",
                           "Korean Institute of", "Institute for Defense Analyses", "US Dept.", "Center for Naval Analysis", "China National Petroleum Corporation",
                           "Government of", "U.S. Dept. of", "U.S.Government", "Governmental", "State Government", "Office of the Comptroller of the Currency",
                           "Food & Drug Administration", "Commodity Futures Trading Commission", "National Renewable Energy Laboratory",
                           "Veterans Administration", "Commodity Futures Commission", "Centers for Disease Control and Prevention",
                           "German Council of Economic Experts", "US Dept. of Agriculture", "Agency for Healthcare Research and Quality",
                           "Fraunhofer Institute for Applied Information Technology FIT", "Fraunhofer Center for International Management and Knowledge",
                           "GAO Financial Markets", "Competition and Market Authority", "Defra", "IPEA",
                           "Office of Fair Trading", "Department for International Trade", "Civil Service", "National Education Association",
                           "Agency for Health Care Research and Quality", "Swiss Competition Commission", "Agroscope", "State Secretariat for Education")
  
  # Function to determine placement type
  determine_placement_type <- function(placement) {
    
    match_keyword <- function(keywords) {
      pattern = paste0("\\b(", paste(keywords, collapse="|"), ")\\b")
      return(grepl(pattern, placement, ignore.case = TRUE))
    }
    
    # Function to check for keyword presence considering context for post-docs
    match_keyword_postdoc <- function(keywords, keywords_not) {
      pattern = paste0("\\b(", paste(keywords, collapse="|"), ")\\b")
      pattern_not = paste0("\\b(", paste(keywords_not, collapse="|"), ")\\b")
      
      return(grepl(pattern, placement, ignore.case = TRUE) & !grepl(pattern_not, placement, ignore.case = TRUE))
    }
    
    if (match_keyword_postdoc(postdocs_keywords, postdocsnot_keywords)) {
      return("postdoc")
    }
    if (match_keyword(tenure_track_keywords)) {
      return("tenure_track")
    }
    if (match_keyword(intl_org_keywords)) {
      return("international_org")
    }
    if (match_keyword(central_banks_keywords)) {
      return("central_bank")
    }
    if (match_keyword(private_sector_keywords)) {
      return("private")
    }
    if (match_keyword(think_tanks_keywords)) {
      return("think_tank")
    }
    if (match_keyword(government_keywords)) {
      return("government")
    }
    return("other")
  }
  
  # Apply the function to classify each placement
  df$placement_type <- sapply(df$placement, determine_placement_type)
  
  # Debugging for misclassified entries
  if (debug) {
    print("Entries classified as 'other':")
    print(df[df$placement_type == "other", ])
  }
  
  return(df)
}

# Apply the function 
all_data_classified <- classify_placement(all_data, debug = TRUE)

all_data_classified <- all_data_classified %>%
  select(year, placement, placement_type, everything())

# Bug - some entries are wrongly classified even with keyword match. Amend:
all_data_classified <- all_data_classified %>%
  mutate(placement_type = ifelse(grepl("Development Institute", placement, ignore.case = TRUE), 
                                 "government", 
                                 placement_type))

other <- all_data_classified %>%
  filter(placement_type == "other")

# Save ----
all_data_classified <- all_data_classified %>%
  filter(placement_type != "other")
write_dta(all_data_classified, paste0(data, "all/scraped_data_raw.dta"))
