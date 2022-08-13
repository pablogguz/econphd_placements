
# Set paths 

source("_packages_paths.R")
setwd(paste0(data))

#------------------------------ 1. Read data and preliminary cleaning ------------------------------#

data <- read_dta("all/data_all_unis_proc.dta")
data <- data %>% filter(private == 1)

# Preliminary cleaning

data$placement <- sub(", Associate", "", data$placement, fixed=TRUE)
data$placement <- sub("Economist", "", data$placement, fixed=TRUE)
data$placement <- sub("Data Scientist", "", data$placement, fixed=TRUE)
data$placement <- sub("Quantitative Researcher", "", data$placement, fixed=TRUE)
data$placement <- sub("Researcher", "", data$placement, fixed=TRUE)
data$placement <- sub("Software Engineer", "", data$placement, fixed=TRUE)
data$placement <- sub("Engineer", "", data$placement, fixed=TRUE)
data$placement <- sub("Senior", "", data$placement, fixed=TRUE)
data$placement <- sub("Analyst", "", data$placement, fixed=TRUE)
data$placement <- sub("Quantitative Trader", "", data$placement, fixed=TRUE)
data$placement <- sub(",", "", data$placement, fixed=TRUE)
data$placement <- str_trim(data$placement)

# Harmonize names

data$placement <- ifelse(grepl(paste("Amazon", collapse="|"), data$placement),
                         "Amazon",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Deloitte", collapse="|"), data$placement),
                         "Deloitte",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Analysis Group", collapse="|"), data$placement),
                         "Analysis Group",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Uber", collapse="|"), data$placement),
                         "Uber",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Cornerstone", collapse="|"), data$placement),
                         "Cornerstone Research",
                         data$placement) 

data$placement <- ifelse(grepl(paste(c("Mathematica","Mathmatica"), collapse="|"), data$placement),
                         "Mathematica",
                         data$placement) 

data$placement <- ifelse(grepl(paste(c("Price","PWC","PwC"), collapse="|"), data$placement),
                         "PwC",
                         data$placement) 

data$placement <- ifelse(grepl(paste(c("Quantco","QuantCo", "Quant Co"), collapse="|"), data$placement),
                         "QuantCo",
                         data$placement) 

data$placement <- ifelse(grepl(paste(c("BnB","Airbnb"), collapse="|"), data$placement),
                         "AirBnB",
                         data$placement) 

data$placement <- ifelse(grepl(paste(c("Black Rock","BlackRock"), collapse="|"), data$placement),
                         "BlackRock",
                         data$placement) 

data$placement <- ifelse(grepl(paste(c("JP Morgan","J.P."), collapse="|"), data$placement),
                         "JP Morgan",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Goldman", collapse="|"), data$placement),
                         "Goldman Sachs",
                         data$placement)

data$placement <- ifelse(grepl(paste("Moody", collapse="|"), data$placement),
                         "Moody's",
                         data$placement)

data$placement <- ifelse(grepl(paste("NERA", collapse="|"), data$placement),
                         "NERA Economic Consulting",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Bates White", collapse="|"), data$placement),
                         "Bates White",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Brattle", collapse="|"), data$placement),
                         "The Brattle Group",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Bank of America", collapse="|"), data$placement),
                         "Bank of America",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Barclay", collapse="|"), data$placement),
                         "Barclays",
                         data$placement) 

data$placement <- ifelse(grepl(paste("McKinsey", collapse="|"), data$placement),
                         "McKinsey & Company",
                         data$placement) 

data$placement <- ifelse(grepl(paste("KPMG", collapse="|"), data$placement),
                         "KPMG",
                         data$placement)

data$placement <- ifelse(grepl(paste("Vanguard", collapse="|"), data$placement),
                         "Vanguard",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Boston Consulting", collapse="|"), data$placement),
                         "Boston Consulting Group",
                         data$placement) 

data$placement <- ifelse(grepl(paste("Charles River", collapse="|"), data$placement),
                         "Charles Rivers Associates",
                         data$placement)

data$placement <- ifelse(grepl(paste("Compass", collapse="|"), data$placement),
                         "Compass Lexecon",
                         data$placement)

data$placement <- ifelse(grepl(paste("Abt", collapse="|"), data$placement),
                         "Abt Associates",
                         data$placement)

data$placement <- ifelse(grepl(paste("Citadel", collapse="|"), data$placement),
                         "Citadel LLC",
                         data$placement)

data$placement <- ifelse(grepl(paste("Citi", collapse="|"), data$placement),
                         "Citigroup",
                         data$placement)

data$placement <- ifelse(grepl(paste("AQR", collapse="|"), data$placement),
                         "AQR Capital Management",
                         data$placement)

data$placement <- ifelse(grepl(paste("Edgeworth", collapse="|"), data$placement),
                         "Edgeworth Economics",
                         data$placement)

data$placement <- ifelse(grepl(paste("Ernst & Young", collapse="|"), data$placement),
                         "Ernst & Young",
                         data$placement)

data$placement <- ifelse(grepl(paste("Keystone", collapse="|"), data$placement),
                         "Keystone Strategy",
                         data$placement)

data$placement <- ifelse(grepl(paste("Microsoft", collapse="|"), data$placement),
                         "Microsoft",
                         data$placement)

data$placement <- ifelse(grepl(paste(c("Facebook","Meta"), collapse="|"), data$placement),
                         "Facebook",
                         data$placement)

#----------------------- 1b. Private sector placements: pie by categories -------------------------#

private <- data

# Tech

vec_tech <- c("Uber","Facebook","WhatsApp","Amazon","Microsoft","AirBnB",
            "Instacart", "Google", "QuantCo", "Wayfair", "TrueCar", "Netflix",
            "Spotify", "Afiniti", "Alibaba", "Blue Labs", "Coursera",
            "LinkedIn", "Pandora", "Opendoor Technologies", "QuantCo",
            "Quora", "Revelio Labs", "Robinhood", "Whatsapp", "Upwork", "Zillow",
            "Datadog", "Lingoda", "Deliveroo", "ZipRecruiter", "Digonex Technologies",
            "Lyft", "JD.com", "Gopuff", "Gro Intelligence", "Twitter")
private$placement <- ifelse(grepl(paste(vec_tech, collapse="|"), private$placement),
                       "Tech",
                       private$placement)

# Finance

vec_finance <- c("Morgan","Citi","Citadel","Abt","Vanguard","BlackRock","Bank of America",
               "Barclays", "Deutsche", "Two Sigma", "Goldman", "Moody", "Capital One",
               "Asset Management", "Equity", "Capital", "Bank", "Invest", "Capital Management",
               "Insurance", "Unicredit", "Barclay", "Investment", "AXA", "BBVA", "Morningstar",
               "BlackRock", "Bloomberg", " Square Capital", "CaixaBank", "Bank", "Fund", "Dodge & Cox",
               "Ellington Management", "Emil van Essen Managed Futures", "Capital", "Freddie Mac",
               "Invesco", "ING", "Jane Street", "Holdings", "Key Square Group", "Caixa",
               "Sidley Austin", "Mastercard", "Agrowealth", "MSCI", "PIMCO", "Risk Management",
               "Swissre", "UBS", "WorldQuant", "Captial One", "Wealthfront", "AIG",
               "Raifeisenbank International", "The Clearing House", "Susquehanna International Group",
               "Spring Venture Group", "State Street")
private$placement <- ifelse(grepl(paste(vec_finance, collapse="|"), private$placement),
                          "Finance & Insurance",
                          private$placement)

# Consulting & Policy research

vec_consulting <- c("NERA","Compass","McKinsey","Consulting","Ernst","Deloitte",
                  "Keystone", "Edgeworth", "Bates White", "PwC", "Mathematica", "Analysis Group",
                  "Cornerstone", "Vivid", "Brattle", "Consultant", "Bain", "AlixPartners",
                  "Alvarez & Marsal", "Alvarez and Marsal", "Bain", "Charles River", "Consultant",
                  "Copenhagen Economics", "DIW Econ", "FTI Consulting", "Keystone Strategy", "KPMG",
                  "Cooper/Smith", "Mercer", "National Economic Research Associates", "Oxera",
                  "Brattle Group", "Wilson Perumal & Company", "Acumen", 
                  "American Road & Transportation Builders Association", "IDinsight", 
                  "Insight", "IMPAQ", "Anaylsis", "CNA", "Resolution Economics",
                  "Dean & Company", "Booz and Company", "Quant Economics", "TWS Partners",
                  "Secretariat")
private$placement <- ifelse(grepl(paste(vec_consulting, collapse="|"), private$placement),
                          "Consulting & Policy research",
                          private$placement)

private$placement <- ifelse(private$placement!="Consulting & Policy research" & private$placement!="Finance & Insurance" & private$placement!="Tech",
                          "Other",
                          private$placement)

# Count placements by category

private <- private %>% group_by(year) %>% count(placement, sort=TRUE)

private <- private %>% 
group_by(placement) %>% 
summarise(n = sum(n))

total <- private %>%
summarise(total = sum(n))

private <- cbind(private, total)

private$fraction <- private$n / private$total

# Export graph

plot <- ggplot(private, aes(x = "", y = n, fill = placement)) +
geom_bar(width = 1, stat="identity") +
scale_fill_brewer(palette="Pastel2") +
coord_polar(theta = "y") + 
geom_text(aes(x=1.1, label=percent(fraction)), 
          position=position_stack(vjust=0.5), size=6) +
theme_void() + theme(
  legend.title=element_blank(),
  legend.text=element_text(size=14)
)

setwd(paste0(fig))

ggsave("pie_chart.png", plot=plot, height=4, width=6, dpi=150)

#------------------------------ 2. Wordcloud ------------------------------#

data <- data %>% count(placement, sort=TRUE)

data2 <- data %>% filter(data$n>=3) # keep placements w/ frequency >= 3

data2$n <- log(data2$n) # change scale

wc <- wordcloud2(data=data2, size=0.45, color='random-dark')

if (is_phantomjs_installed()==FALSE) {
  webshot::install_phantomjs()
}

saveWidget(wc,"wordcloud.html",selfcontained = F)
webshot::webshot("wordcloud.html","wordcloud.png", vwidth = 900, vheight = 600, delay =10)

# Clean environment

setwd(paste0(code))
rm(list = ls())
