

# Set paths 

source("_packages_paths.R")
setwd(paste0(data, "/us/raw/"))

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

# Get names, placement and year

web <- read_html("https://economics.sas.upenn.edu/graduate/prospective-students/placement-information")
names <- web %>% html_nodes("a,p") %>% html_text()
names <- as.data.frame(names)
names <- names %>% filter(row_number() > 44) # removed unused lines
names$names <- str_trim(names$names) # trim
names$year <- str_sub(names$names, start = -4) # extract year

vec_years <- c(2004:2022)
names$year <- ifelse(grepl(paste(vec_years, collapse="|"), names$year),
                    names$year,
                    NA) # generate year column

for (i in 1:nrow(names)) { # complete year column
  j <- i - 1
  if (j > 0 & is.na(names$year[i])) {
    names$year[i] <- names$year[j]
  }
}

names <- names %>% filter(!grepl(paste(vec_years, collapse="|"), names)) # remove rows with years within name variable
names <- names %>% mutate(n = row_number()) # for merging

names <- names %>% mutate(n = row_number()) # for merging
names$names <- gsub('\u2013', '-', names$names, perl = T) # harmonize dashes
names$names <- gsub('next year postdoc', '', names$names) # in order not to confound pure post-docs
names$names <- gsub('next year, postdoc', '', names$names) # in order not to confound pure post-docs
names$names <- gsub('(Start-up)', '', names$names) # other adjustments
names$names <- gsub('Post-Doc', 'postdoc', names$names) # other adjustments
names$names <- gsub('Post-doc', 'postdoc', names$names) # other adjustments
names$names <- gsub('2-Year', '2 year', names$names) # other adjustments
names$names <- gsub('- Finland', 'Finland', names$names) # other adjustments
names$names <- gsub('Urbana-Champaign', 'Urbana Champaign', names$names) # other adjustments
names$names <- gsub('- Research Analyst', 'Research Analyst', names$names) # other adjustments
names$names <- gsub('Cornerstone Reseach - Associate', 'Cornerstone Reseach, Associate', names$names) # other adjustments

names$n_hyphens <- str_count(names$names, "-") # number of hyphens
names$names <- ifelse(names$n_hyphens>=2,
                      sub("-", "", names$names, fixed = TRUE),
                      names$names) # if there are more than two hyphens, remove the 1st one (i.e., the one that belongs to the name)

names <- names %>% separate(names, into = c("id", "placement"), sep = "-") # separate
names$n_hyphens <- NULL # remove unused variables

# Merge

data_final <- names
data_final <- subset(data_final, year>=2012) 
data_final <- data_final %>% rename(name = id) # for merging

limit <- data_final %>% filter(name == "Michela Tincani ")
limit <- limit$n[[1]]
data_final <- data_final %>% filter(n <= limit) # get data only from 2012 onwards

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
                 "INSEAD", "College", "Lingnan university", "EIEF")
tenuretrack <- data_final %>% filter(grepl(paste(tenuretrack, collapse="|"), placement))
tenuretrack$placement_type2 <- "tenure_track"
tenuretrack <- tenuretrack %>% select(name, placement_type2)

# International organizations 

intl <- c("World Bank", "IMF", "UNU Wider", "International Monetary Fund", "Inter American Development Bank")
intl <- data_final %>% filter(grepl(paste(intl, collapse="|"), placement))
intl$placement_type3 <- "international_inst"
intl <- intl %>% select(name, placement_type3)

# Private sector 

private <- c("Consultant", "Deliveroo", "Uber", "Amazon", "Data Scientist", "Luohan Academy", 
             "Vivid Economics", "Cornerstone", "Data Scientist", "Associate",
             "Bank of America", "Analysis Group", "Company", "Instacart", "Resolution Economics",
             "Citadel", 'Mathematica', 'Brattle', "Deutshe Bank", 'Investment', "JD.com", "Capital One",
             "QuantCo", "IMPAQ", "Black Rock", "Afiniti", "Vanguard", "Kaneka Holdings", "Facebook",
             "Microsoft", "Susquehanna International Group")
private <- data_final %>% filter(grepl(paste(private, collapse="|"), placement))
private$placement_type4 <- "private"
private <- private %>% select(name, placement_type4)

# Central banks  

centralbanks <- c("Bank of Spain", "Bank of Canada", "Bank of Chile", "Federal Reserve", "FRB",
                  "Central Bank", 'Bank of Korea', "Bank of Mexico", "FED", "Fed", "Bank of Italy")
centralbanks <- data_final %>% filter(grepl(paste(centralbanks, collapse="|"), placement))
centralbanks$placement_type5 <- "central_bank"
centralbanks <- centralbanks %>% select(name, placement_type5)

# Think tanks 

thinktanks <- c("Institute", "RAND", "RAND, Associate Economist")
thinktanks <- data_final %>% filter(grepl(paste(thinktanks, collapse="|"), placement))
thinktanks$placement_type6 <- "thinktank"
thinktanks <- thinktanks %>% select(name, placement_type6)

# Government  

government <- c("U.S.", "Treasury", "U.S. Department of", "US Department of", "Federal Trade Commission",
                "Korea Development", "Legislative", "US Naval Academy", "Ministry")
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

data_final[4:11] <- list(NULL) # remove unnecessary dummies

order <- c("year", "name", "placement", "placement_type") # rearrange columns
data_final <- data_final[,order]
data_final <- data_final %>% filter(year>=2012) # keep data from 2012 onward

write_xlsx(data_final, "upenn.xls")

# Clean environment

setwd(paste0(code))
rm(list = ls())

