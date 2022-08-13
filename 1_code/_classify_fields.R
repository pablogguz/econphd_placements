

# Set paths 

source("_packages_paths.R")
setwd(paste0(data))

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

# EU

lse <- read_xlsx("eu/raw/lse.xlsx")
lse$uni <- "lse"

warwick <- read_xlsx("eu/raw/warwick.xlsx")
warwick$uni <- "warwick"

# US

for (i in c("columbia", "harvard", "princeton", "stanford", "cornell", "maryland")) {
  name <- i
  i <- read_xlsx(paste0("us/raw/", name, ".xls"))
  i$uni <- paste0(name)
  assign(paste0(name), i)
  rm(i)
}

# Append

data <- rbind(lse, columbia, harvard, princeton, stanford, cornell, maryland, warwick)

#------------------------------ 2. Define primary field ------------------------------#

# Clean field

data$field <- str_trim(data$field)
data$field <- gsub("(primary)", ", ", data$field) 
data$field <- gsub("(Primary)", ", ", data$field) 
data$field <- gsub(", :", "", data$field) 
data$field <- gsub(",  Field:", "", data$field) 
data$field <- gsub(",  Fields:", "", data$field)
data$field <- gsub(",  field:", "", data$field)
data$field <- gsub(",  fields:", "", data$field)
data$field <- sub("(, )", ", ", data$field, fixed=TRUE)
data$field <- sub("Secondary: ", "", data$field, fixed=TRUE)
data$field <- sub("(Secondary) ", "", data$field, fixed=TRUE)
data$field <- sub("(Secondary)", "", data$field, fixed=TRUE)
data$field <- sub("Secondary field: ", "", data$field, fixed=TRUE)
data$field <- sub("Secondary Field: ", "", data$field, fixed=TRUE)
data$field <- sub(";", ",", data$field, fixed=TRUE)
data$field <- sub(" ,", ",", data$field, fixed=TRUE)
data$field <- sub("/", ", ", data$field, fixed=TRUE)
data$field <- sub(" and", ",", data$field, fixed=TRUE)
data$field <- sub("&", ",", data$field, fixed=TRUE)
data$field <- sub("(Environment, Public)", "", data$field, fixed=TRUE)
data$field <- str_trim(data$field)

# Define primary and secondary field

data <- data %>% separate(field, into = c("primary_field", "secondary_field"), ",")
data$primary_field <- str_trim(tolower(data$primary_field))
data$secondary_field <- str_trim(tolower(data$secondary_field))

# Drop NAs

data$primary_field <- ifelse(data$primary_field=="pam", NA, data$primary_field)
data <- data %>% filter(!is.na(primary_field))

# Trim 
data$primary_field <- str_trim(data$primary_field)
data$secondary_field <- str_trim(data$secondary_field)

#------------------------------ 3. Classify fields ------------------------------#

# Theory

vec_theory <- c("theory", "contracts", "market design", "mechanism design", "market microstructure")
data$primary_field_final <- ifelse(grepl(paste(vec_theory, collapse="|"), data$primary_field),
                                   "theory",
                                   NA)
# Applied Micro

vec_appliedmicro <- c("applied micro", "applied microeconomics", "applied economics", "gender economics", "applied mircoeconomics")
data$primary_field_final <- ifelse(grepl(paste(vec_appliedmicro, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "appliedmicro",
                                   data$primary_field_final) 
# Macro

vec_macro <- c("macroeconomics", "monetary")
data$primary_field_final <- ifelse(grepl(paste(vec_macro, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "macroeconomics",
                                   data$primary_field_final) 
# Public

vec_public <- c("public")
data$primary_field_final <- ifelse(grepl(paste(vec_public, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "public",
                                   data$primary_field_final) 
# Finance

vec_finance <- c("finance", "financial", "asset pricing")
data$primary_field_final <- ifelse(grepl(paste(vec_finance, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "finance",
                                   data$primary_field_final) 
# Trade 

vec_trade <- c("trade")
data$primary_field_final <- ifelse(grepl(paste(vec_trade, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "trade",
                                   data$primary_field_final) 
# Development

vec_development <- c("development")
data$primary_field_final <- ifelse(grepl(paste(vec_development, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "development",
                                   data$primary_field_final)

# Behavioral, experimental, psychology

vec_behavioral <- c("behavioral", "behavioural", "experimental", "psychology", "behavorial")
data$primary_field_final <- ifelse(grepl(paste(vec_behavioral, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "behavioral",
                                   data$primary_field_final)
# Labor

vec_labor <- c("labor", "labour")
data$primary_field_final <- ifelse(grepl(paste(vec_labor, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "labor",
                                   data$primary_field_final)
# Metrics

vec_metrics <- c("econometrics", "metrics", "econometric methods")
data$primary_field_final <- ifelse(grepl(paste(vec_metrics, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "econometrics",
                                   data$primary_field_final)
# History

vec_history <- c("history")
data$primary_field_final <- ifelse(grepl(paste(vec_history, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "history",
                                   data$primary_field_final)
# International

vec_inter <- c("international")
data$primary_field_final <- ifelse(grepl(paste(vec_inter, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "international",
                                   data$primary_field_final)
# Education

vec_education <- c("education")
data$primary_field_final <- ifelse(grepl(paste(vec_education, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "education",
                                   data$primary_field_final) 
# IO

vec_io <- c("industrial", "io")
data$primary_field_final <- ifelse(grepl(paste(vec_io, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "io",
                                   data$primary_field_final)
# Urban

vec_urban <- c("urban")
data$primary_field_final <- ifelse(grepl(paste(vec_urban, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "urban",
                                   data$primary_field_final)
# Political economy

vec_political <- c("political")
data$primary_field_final <- ifelse(grepl(paste(vec_political, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "political",
                                   data$primary_field_final)
# Health

vec_health <- c("health")
data$primary_field_final <- ifelse(grepl(paste(vec_health, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "health",
                                   data$primary_field_final)
# Environmental 

vec_environment <- c("environment", "resource")
data$primary_field_final <- ifelse(grepl(paste(vec_environment, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "environmental",
                                   data$primary_field_final) 
# Food & agricultural

vec_food <- c("food", "agricultural")
data$primary_field_final <- ifelse(grepl(paste(vec_food, collapse="|"), data$primary_field) & is.na(data$primary_field_final),
                                   "food",
                                   data$primary_field_final) 
# Other

data$primary_field_final <- ifelse(is.na(data$primary_field_final),
                                   "other",
                                   data$primary_field_final) 
# Reconvert to theory

vec_theory <- c("microeconomics")
data$primary_field_final <- ifelse(grepl(paste(vec_theory, collapse="|"), data$primary_field) & data$primary_field_final == "other",
                                   "theory",
                                   data$primary_field_final) 
  

#------------------------------ 4. Save ------------------------------#

order <- c("year", "name", "uni", "placement", "placement_type", "primary_field_final", "secondary_field") # rearrange columns
data <- data[,order]

write_xlsx(data, "all/data_all_unis_fields.xlsx")

# Clean environment

setwd(paste0(rscripts))
rm(list = ls())
  