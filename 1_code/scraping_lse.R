

# Set paths 

source("_packages_paths.R")
setwd(paste0(data, "/eu/raw/"))

word.list <- c('Professor',
               'Scientist',
               'Economist',
               'Post-doc',
               'Postdoc',
               'Postdoctoral',
               'Fellow',
               'Associate',
               'Consultant',
               'Research',
               'Lecturer',
               'Young Professionals Program', # 2022 bug
               'TBC',
               "Icesi University, Colombia") # 2015 bug

field.list <- c('Micro',
                'Macro',
                'Econometrics',
                'Theory',
                'Economics',
                'economics',
                'Development',
                'Environmental',
                'Labour',
                'Industrial Organization',
                'Urban',
                'Political Economy',
                'Behavioural',
                'Experimental',
                'Psychology',
                'International Trade')

id.list <- c("Mr", "Ms", "Mrs", 'Miss')

############################################################ 2022

# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:22]
names <- str_remove(names, "PhD Candidate in Economics") # remove unused line
names <- str_remove(names, "Primary research interests:") # remove unused line
names <- str_remove(names, "Primary research interest:") # remove unused line
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

# Select relevant lines - names

id <- word(names,1,2,sep=" ") # names of candidates
id <- as.data.frame(id) 
id <- id %>% filter(!grepl(paste(field.list, collapse="|"), id)) # drop if match with field list
id <- id %>% filter(!is.na(id))
id <- id %>% mutate(n = row_number()) # for merging

# Select relevant lines - placement

place <- sub('^\\w+\\s\\w+\\s', '', names) # remove first two words
place <- as.data.frame(place)
place <- place %>% filter(grepl(paste(word.list, collapse="|"), place)) # keep if match with word list
place <- place %>% mutate(n = row_number()) # for merging

# Select relevant lines - fields

fields <- as.data.frame(names)
fields <- fields %>% filter(grepl(paste(field.list, collapse="|"), names)) # keep if match with field list
fields$names <- str_remove(fields$names, "Martina Zanella Assistant Professor, Department of Economics, Trinity College Dublin") # bug
fields$names <- str_remove(fields$names, "Sacha Dray Young Professionals Program, World Bank ") # bug
fields$names <- str_remove(fields$names, "Bhargavi Sakthivel Global Economist, Global Modelling and Analyst Team, Bloomberg ") # bug
fields$names <- str_remove(fields$names, "Amanda Dahlstrand Post-doc, Microsoft, 2022-23;Assistant Professor, University of Zurich, 2023") # bug
fields <- fields %>% filter(!grepl(paste(word.list, collapse="|"), names)) # drop if match with word list
fields <- fields %>% filter(names != "") # remove blank rows
fields <- fields %>% mutate(n = row_number()) # for merging
fields <- fields %>% rename(fields = names) # for merging

# Merge 

df2022 <- left_join(id, place)
df2022 <- left_join(df2022, fields)
df2022$n <- NULL # no longer needed
df2022$year <- 2022

############################################################ 2021

# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2020-2021")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:22]
names <- str_remove(names, "PhD Candidate in Economics") # remove unused line
names <- str_remove(names, "Primary research interests") # remove unused line
names <- str_remove(names, "Primary research interest") # remove unused line
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

# Select relevant lines - names

id <- word(names,1,2,sep=" ") # names of candidates
id <- as.data.frame(id) 
id <- id %>% filter(!grepl(paste(field.list, collapse="|"), id)) # drop if match with field list
id <- id %>% filter(!is.na(id))
id <- id %>% mutate(n = row_number()) # for merging
  
# Select relevant lines - placement
  
place <- sub('^\\w+\\s\\w+\\s', '', names) # remove first two words
place <- as.data.frame(place)
place <- place %>% filter(grepl(paste(word.list, collapse="|"), place)) # keep if match with word list
place <- place %>% mutate(n = row_number()) # for merging

# Select relevant lines - fields
  
fields <- as.data.frame(names)
fields <- fields %>% filter(grepl(paste(field.list, collapse="|"), names)) # keep if match with field list
fields$names <- str_remove(fields$names, "Yusuke Kuroishi Assistant Professor, Hitotsubashi University ") # bug
fields$names <- str_remove(fields$names, "Tsogsag Nyamdavaa Assistant Professor, Hitotsubashi University ") # bug
fields <- fields %>% filter(!grepl(paste(word.list, collapse="|"), names)) # drop if match with word list
fields <- fields %>% mutate(n = row_number()) # for merging
fields <- fields %>% rename(fields = names) # for merging

# Merge 
  
df2021 <- left_join(id, place)
df2021 <- left_join(df2021, fields)
df2021$n <- NULL # no longer needed
df2021$year <- 2021
  
############################################################ 2020
  
# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2019-2020")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:28]
names <- str_remove(names, "PhD Candidate in Economics") # remove unused line
names <- str_remove(names, "Primary research interests") # remove unused line
names <- str_remove(names, "Primary research interest") # remove unused line
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

# Select relevant lines - names

id <- word(names,1,3,sep=" ") # names of candidates
id <- as.data.frame(id) 
id <- id %>% filter(!grepl(paste(field.list, collapse="|"), id)) # drop if match with field list
id <- id %>% filter(!is.na(id))
id <- id %>% mutate(n = row_number()) # for merging

# Select relevant lines - placement

place <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', names) # remove first three words
place <- as.data.frame(place)
place <- place %>% filter(grepl(paste(word.list, collapse="|"), place)) # keep if match with word list
place <- place %>% mutate(n = row_number()) # for merging

# Select relevant lines - fields

fields <- as.data.frame(names)
fields <- fields %>% filter(grepl(paste(field.list, collapse="|"), names)) # keep if match with field list
fields$names <- str_remove(fields$names, "Yusuke Kuroishi Assistant Professor, Hitotsubashi University ") # bug
fields$names <- str_remove(fields$names, "Tsogsag Nyamdavaa Assistant Professor, Hitotsubashi University ") # bug
fields <- fields %>% filter(!grepl(paste(word.list, collapse="|"), names)) # drop if match with word list
fields <- fields %>% mutate(n = row_number()) # for merging
fields <- fields %>% rename(fields = names) # for merging

# Merge 

df2020 <- left_join(id, place)
df2020 <- left_join(df2020, fields)
df2020$n <- NULL # no longer needed
df2020$year <- 2020
  
  
############################################################ 2019
  
missing.placement <- c("Mr Andres", "Mr Torsten")
  
# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2018-2019")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:49]
names <- str_remove(names, "PhD Candidate in Economics") # remove unused line
names <- str_remove(names, "Primary research interests") # remove unused line
names <- str_remove(names, "Primary research interest") # remove unused line
names <- str_remove(names, "Research interests") # remove unused line
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

# Select relevant lines - names

id <- sub("(\\w+\\s+\\w+\\s+\\w+).*", "\\1", names) # first three words
id <- as.data.frame(id) 
id <- id %>% filter(!grepl(paste(field.list, collapse="|"), id)) # drop if match with field list
id <- id %>% filter(!grepl(paste(missing.placement, collapse="|"), id)) # drop if match with missing placement list
id <- id %>% filter(grepl(paste(id.list, collapse="|"), id)) # keep if match with id list
id <- id %>% filter(!is.na(id))
id <- id %>% mutate(n = row_number()) # for merging

# Select relevant lines - placement

place <- str_remove(names, "-") #remove hyphens
place <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', place) # remove first three words
place <- as.data.frame(place)
place <- place %>% filter(grepl(paste(word.list, collapse="|"), place)) # keep if match with word list
place <- place %>% mutate(n = row_number()) # for merging

# Select relevant lines - fields

fields <- as.data.frame(names)
fields <- fields %>% filter(grepl(paste(field.list, collapse="|"), names)) # keep if match with field list
fields <- fields %>% mutate(n = row_number()) # define row numbers for debugging
fields$names <- str_remove(fields$names, "Economics of Education, Labor Economics") # bug
fields <- fields %>% filter(n!=12) # bug
fields <- fields %>% filter(names!="") # bug
fields <- fields %>% filter(!grepl(paste(word.list, collapse="|"), names)) # drop if match with word list
fields$n <- NULL
fields <- fields %>% mutate(n = row_number()) # for merging
fields <- fields %>% rename(fields = names) # for merging

# Merge

df2019 <- left_join(id, place)
df2019 <- left_join(df2019, fields)
df2019$n <- NULL # no longer needed
df2019$year <- 2019

############################################################ 2018

# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2017-2018")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:42]
names <- str_remove(names, "PhD Candidate in Economics") # remove unused line
names <- str_remove(names, "Primary research interests") # remove unused line
names <- str_remove(names, "Primary research interest") # remove unused line
names <- str_remove(names, "Research interests") # remove unused line
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

# Select relevant lines - names

id <- sub("(\\w+\\s+\\w+\\s+\\w+).*", "\\1", names) # first three words
id <- as.data.frame(id) 
id <- id %>% filter(!grepl(paste(field.list, collapse="|"), id)) # drop if match with field list
id <- id %>% filter(grepl(paste(id.list, collapse="|"), id)) # keep if match with id list
id <- id %>% filter(!is.na(id))
id <- id %>% mutate(n = row_number()) # for merging

# Select relevant lines - placement

place <- str_remove(names, "-") #remove hyphens
place <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', place) # remove first three words
place <- as.data.frame(place)
place <- place %>% filter(grepl(paste(word.list, collapse="|"), place)) # keep if match with word list
place <- place %>% mutate(n = row_number()) # for merging

# Select relevant lines - fields

fields <- as.data.frame(names)
fields <- fields %>% filter(grepl(paste(field.list, collapse="|"), names)) # keep if match with field list
fields <- fields %>% filter(!grepl(paste(word.list, collapse="|"), names)) # drop if match with word list
fields <- fields %>% mutate(n = row_number()) # for merging
fields <- fields %>% rename(fields = names) # for merging

# Merge 

df2018 <- left_join(id, place)
df2018 <- left_join(df2018, fields)
df2018$n <- NULL # no longer needed
df2018$year <- 2018

############################################################ 2017

# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2016-2017")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:29]
names <- str_remove(names, "PhD Candidate in Economics") # remove unused line
names <- str_remove(names, "Primary research interests") # remove unused line
names <- str_remove(names, "Primary research interest") # remove unused line
names <- str_remove(names, "Research interests") # remove unused line
names <- str_remove(names, "Research Interests:") # remove unused line
names <- str_remove(names, "Research Intersts:") # remove unused line
names <- str_remove(names, ":") # remove unused line
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

# Select relevant lines - names

id <- sub("(\\w+\\s+\\w+\\s+\\w+).*", "\\1", names) # first three words
id <- as.data.frame(id) 
id <- id %>% filter(!grepl(paste(field.list, collapse="|"), id)) # drop if match with field list
id <- id %>% filter(grepl(paste(id.list, collapse="|"), id)) # keep if match with id list
id <- id %>% filter(!is.na(id))
id <- id %>% mutate(n = row_number()) # for merging

# Select relevant lines - placement

place <- str_remove(names, "-") #remove hyphens
place <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', place) # remove first three words
place <- as.data.frame(place)
place <- place %>% filter(grepl(paste(word.list, collapse="|"), place)) # keep if match with word list
place <- place %>% mutate(n = row_number()) # for merging

# Select relevant lines - fields

fields <- as.data.frame(names)
fields <- fields %>% filter(grepl(paste(field.list, collapse="|"), names)) # keep if match with field list
fields <- fields %>% filter(!grepl(paste(word.list, collapse="|"), names)) # drop if match with word list
fields <- fields %>% mutate(n = row_number()) # for merging
fields <- fields %>% rename(fields = names) # for merging

# Merge 

df2017 <- left_join(id, place)
df2017 <- left_join(df2017, fields)
df2017$n <- NULL # no longer needed
df2017$year <- 2017

############################################################ 2016

missing.placement <- c("Sarah Sandford")

# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2015-2016")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:29]
names <- str_remove(names, "PhD Candidate in Economics") # remove unused line
names <- str_remove(names, "Primary research interests") # remove unused line
names <- str_remove(names, "Primary research interest") # remove unused line
names <- str_remove(names, "Research interests") # remove unused line
names <- str_remove(names, "Research Interests:") # remove unused line
names <- str_remove(names, "Research Intersts:") # remove unused line
names <- str_remove(names, ":") # remove unused line
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

# Select relevant lines - names

id <- sub("(\\w+\\s+\\w+\\s+\\w+).*", "\\1", names) # first three words
id <- as.data.frame(id) 
id <- id %>% filter(!grepl(paste(field.list, collapse="|"), id)) # drop if match with field list
id <- id %>% filter(!grepl(paste(missing.placement, collapse="|"), id)) # drop if match with missing placement list
id <- id %>% filter(grepl(paste(id.list, collapse="|"), id)) # keep if match with id list
id <- id %>% filter(!is.na(id))
id <- id %>% mutate(n = row_number()) # for merging

# Select relevant lines - placement

place <- str_remove(names, "-") #remove hyphens
place <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', place) # remove first three words
place <- as.data.frame(place)
place <- place %>% filter(grepl(paste(word.list, collapse="|"), place)) # keep if match with word list
place <- place %>% mutate(n = row_number()) # for merging

# Select relevant lines - fields

fields <- as.data.frame(names)
fields <- fields %>% filter(grepl(paste(field.list, collapse="|"), names)) # keep if match with field list
fields <- fields %>% filter(!grepl(paste(word.list, collapse="|"), names)) # drop if match with word list
fields <- fields %>% mutate(n = row_number()) # for merging
fields <- fields %>% rename(fields = names) # for merging

# Merge 

df2016 <- left_join(id, place)
df2016 <- left_join(df2016, fields)
df2016$n <- NULL # no longer needed
df2016$year <- 2016


############################################################ 2015

# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2014-2015")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:44]
names <- str_remove(names, "PhD Candidate in Economics") # remove unused line
names <- str_remove(names, "Primary research interests") # remove unused line
names <- str_remove(names, "Primary research interest") # remove unused line
names <- str_remove(names, "Research interests") # remove unused line
names <- str_remove(names, "Research Interests:") # remove unused line
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

# Select relevant lines - names

id <- sub("(\\w+\\s+\\w+\\s+\\w+).*", "\\1", names) # first three words
id <- as.data.frame(id) 
id <- id %>% filter(!grepl(paste(field.list, collapse="|"), id)) # drop if match with field list
id <- id %>% filter(grepl(paste(id.list, collapse="|"), id)) # keep if match with id list
id <- id %>% filter(!is.na(id))
id <- id %>% mutate(n = row_number()) # for merging

# Select relevant lines - placement

place <- str_remove(names, "-") #remove hyphens
place <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', place) # remove first three words
place <- as.data.frame(place)
place <- place %>% filter(grepl(paste(word.list, collapse="|"), place)) # keep if match with word list
place <- place %>% mutate(n = row_number()) # for merging

# Select relevant lines - fields

fields <- as.data.frame(names)
fields <- fields %>% filter(grepl(paste(field.list, collapse="|"), names)) # keep if match with field list
fields <- fields %>% filter(!grepl(paste(word.list, collapse="|"), names)) # drop if match with word list
fields <- fields %>% mutate(n = row_number()) # for merging
fields <- fields %>% rename(fields = names) # for merging

# Merge 

df2015 <- left_join(id, place)
df2015 <- left_join(df2015, fields)
df2015$n <- NULL # no longer needed
df2015$year <- 2015

############################################################ 2014

# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2013-2014")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:17]
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

df <- as.data.frame(names)
df$id <- sub("(\\w+\\s+\\w+\\s+\\w+).*", "\\1", df$names) # first three words
df$names <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', df$names) # remove first three words
df$fields <- gsub(".*Research Interests:","",df$names)
df$names <- gsub("Research Interests:.*","",df$names) # remove interests

df <- df %>% rename(place = names) # for merging

df2014 <- df
df2014$year <- 2014

############################################################ 2013

# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2012-2013")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:17]
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

df <- as.data.frame(names)
df$id <- sub("(\\w+\\s+\\w+\\s+\\w+).*", "\\1", df$names) # first three words
df$names <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', df$names) # remove first three words
df$fields <- gsub(".*Research Interests:","",df$names)
df$names <- gsub("Research Interests:.*","",df$names) # remove interests

df <- df %>% rename(place = names) # for merging

df2013 <- df
df2013$year <- 2013

############################################################ 2012

# Read web

web <- read_html("https://www.lse.ac.uk/economics/phd-job-market/job-market-candidates-2011-2012")
names <- web %>% html_nodes("p") %>% html_text()

names <- names[3:17]
names <- gsub("([a-z])([A-Z])","\\1 \\2",names) #introduce a space between words that start with uppercase letter

df <- as.data.frame(names)
df$id <- sub("(\\w+\\s+\\w+\\s+\\w+).*", "\\1", df$names) # first three words
df$names <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', df$names) # remove first three words
df$fields <- gsub(".*Research Interests:","",df$names)
df$names <- gsub("Research Interests:.*","",df$names) # remove interests

df <- df %>% rename(place = names) # for merging

df2012 <- df
df2012$year <- 2012
  
#------------------------------ 1. Append data and preliminary cleaning ------------------------------#
  
data_final <- rbind(df2012, df2013, df2014, df2015, df2016, df2017, df2018, df2019, df2020, df2021, df2022)
data_final <- data_final %>% rename(name = id) # for merging
data_final <- data_final %>% rename(placement = place) # for merging
data_final <- data_final %>% rename(field = fields) # for merging

#------------------------------ 2. Classify placements ------------------------------#
  
# Post-docs  

postdocs <- c("Post-doc", "Postdoc", "Postdoctoral", "Post-doctoral", "postdoc")
postdocs <- data_final %>% filter(grepl(paste(postdocs, collapse="|"), placement))
postdocs$placement_type1 <- "postdoc"
postdocs <- postdocs %>% select(name, placement_type1)

# Tenure-track  

tenuretrack <- c("Assistant Professor", "Professor", "Lecturer")
tenuretrack <- data_final %>% filter(grepl(paste(tenuretrack, collapse="|"), placement))
tenuretrack$placement_type2 <- "tenure_track"
tenuretrack <- tenuretrack %>% select(name, placement_type2)

# International organizations  

intl <- c("World Bank", "IMF", "UNU Wider", "International Monetary Fund", "European Commission", "UNU WIDER")
intl <- data_final %>% filter(grepl(paste(intl, collapse="|"), placement))
intl$placement_type3 <- "international_inst"
intl <- intl %>% select(name, placement_type3)

# Private sector  

private <- c("Consultant", "Deliveroo", "Uber", "Amazon", "Data Scientist", "Luohan Academy", 
             "Vivid Economics", "Compass Lexecon", "NERA", "Unicredit", "Cornerstone Research", "Bloomberg")
private <- data_final %>% filter(grepl(paste(private, collapse="|"), placement))
private$placement_type4 <- "private"
private <- private %>% select(name, placement_type4)

# Central banks  

centralbanks <- c("Bank of England", "European Central Bank", "Bank of Canada", "Federal Reserve",
                  "Bank of Italy", "Central Bank")
centralbanks <- data_final %>% filter(grepl(paste(centralbanks, collapse="|"), placement))
centralbanks$placement_type5 <- "central_bank"
centralbanks <- centralbanks %>% select(name, placement_type5)

# Merge in type dummies

data_final <- left_join(data_final, postdocs)
data_final <- left_join(data_final, tenuretrack)
data_final <- left_join(data_final, intl)
data_final <- left_join(data_final, private)
data_final <- left_join(data_final, centralbanks)

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
                                    "other", 
                                    data_final$placement_type)

#------------------------------ 3. Final cleaning and save ------------------------------#

data_final[5:9] <- list(NULL) # remove unnecessary dummies
data_final <- data_final %>% filter(placement != "") # drop NAs for placement

# Only keep first placement if the candidate goes twice on the market

data_final <- data_final %>%
  group_by(name) %>%
  slice(which.min(year))

order <- c("year", "name", "field", "placement", "placement_type") # rearrange columns

write_xlsx(data_final, "lse.xlsx")

# Clean environment

setwd(paste0(code))
rm(list = ls())

  