
library(XML)
library(stringr)
library(dplyr)
library(haven)
library(writexl)
library(readxl)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(tidytext)
library(ggplot2)
library(shiny)
library(rsconnect)
library(fixest)
library(fastDummies)
library(shinythemes)
library(thematic)

#------------------------------ 1. Read data ------------------------------#

data <- read_dta("https://github.com/pablogguz/econphd_placements/blob/main/0_data/all/data_all_unis_proc_shiny.dta?raw=true")

data$type2 <- ifelse(data$typebis==8,
                     "Private sector",
                     NA)
data$type2 <- ifelse(data$typebis==7,
                     "Think tanks",
                     data$type2)
data$type2 <- ifelse(data$typebis==6,
                     "International organizations",
                     data$type2)
data$type2 <- ifelse(data$typebis==5,
                     "Government",
                     data$type2)
data$type2 <- ifelse(data$typebis==4,
                     "Central banks",
                     data$type2)
data$type2 <- ifelse(data$typebis==3,
                     "Other academic",
                     data$type2)
data$type2 <- ifelse(data$typebis==2,
                     "Post-doc",
                     data$type2)
data$type2 <- ifelse(data$typebis==1,
                     "Tenure-track",
                     data$type2)

# Order 
data$order <- ifelse(data$type2=="Tenure-track",
                     8,
                     NA)
data$order <- ifelse(data$type2=="Post-doc",
                     7,
                     data$order)
data$order <- ifelse(data$type2=="Other academic",
                     6,
                     data$order)
data$order <- ifelse(data$type2=="Central banks",
                     5,
                     data$order)
data$order <- ifelse(data$type2=="Government",
                     4,
                     data$order)
data$order <- ifelse(data$type2=="International organizations",
                     3,
                     data$order)
data$order <- ifelse(data$type2=="Think tanks",
                     2,
                     data$order)
data$order <- ifelse(data$type2=="Private sector",
                     1,
                     data$order)

#----------------------- 1. Read data (field) ------------------------------#

field <- read_dta("https://github.com/pablogguz/econphd_placements/blob/main/0_data/all/shiny_app_field.dta?raw=true")


field$type2 <- ifelse(field$type==1,
                      "Central banks",
                      NA)
field$type2 <- ifelse(field$type==2,
                      "Government",
                      field$type2)
field$type2 <- ifelse(field$type==3,
                      "International organizations",
                      field$type2)
field$type2 <- ifelse(field$type==4,
                      "Other",
                      field$type2)
field$type2 <- ifelse(field$type==5,
                      "Post-doc",
                      field$type2)
field$type2 <- ifelse(field$type==6,
                      "Private sector",
                      field$type2)
field$type2 <- ifelse(field$type==7,
                      "Tenure-track",
                      field$type2)
field$type2 <- ifelse(field$type==8,
                      "Think tanks",
                      field$type2)

#------------------------------ 2. Shiny app ------------------------------#

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("journal"),
  
  # App title ----
  titlePanel("Placement outcomes for PhD graduates in Economics"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      p("We gather individual-level data on PhD placement outcomes for 25 different departments 
        over the period 2012-2022 using web scraping methods to answer the following question:
        who hires PhD graduates in Economics?"),
      
      p("We sort placements into 8 different categories, namely (academic) tenure-track
        positions, post-docs, international organizations, central banks, government, think tanks, 
        private sector, and other academic placements."),
      
      p("Explore the data using the selection tabs below."),
      
      h5(a("pablogguz.github.io", 
           href = "https://pablogguz.github.io/"), "| ", a("@pablogguz_", href = "https://twitter.com/pablogguz_")
         ),
      
      h5(a("malmunia.github.io", 
           href = "https://malmunia.github.io/"), "| ", a("@miguel_almunia", href = "https://twitter.com/miguel_almunia")
      ),
      
      # Input: Selector for choosing department (top panel) ---- 
      selectInput(inputId = "dataset",
                  label = "Panel #1: Placement distribution over time (choose a department):",
                  choices = c("MIT", "Harvard", "Stanford", "Princeton", "Berkeley",
                              "Chicago", "Yale", "Columbia", "Northwestern", "NYU", "UPenn", "UCLA",
                              "Michigan", "Brown", "Minnesota", "Maryland", "Duke", "Cornell", 
                              "Boston University", "LSE",
                              "UCL", "UPF", "Warwick", "Bonn", "EUI")),
      
      # Input: Selector for choosing type and time window ----
      selectInput(inputId = "dataset2",
                  label = "Panel #2: Type-specific placement rate (choose type and time window):",
                  choices = c("Tenure-track", "Post-doc", "Private sector", "International organizations", "Government", 
                              "Central banks", "Think tanks", "Other academic")),
      
      div(style = "margin: auto; width: 40%",
        sliderInput(inputId = "yearslider",
                    label = "",
                    min = 2012,
                    max = 2022,
                    sep = "",
                    value = c(2012,2022),
                    width = "220px")
      ),
      
      selectInput(inputId = "dataset3",
                  label = "Panel #3: Estimated placement probabilities (choose type)",
                  choices = c("Tenure-track", "Post-doc", "Private sector", "International organizations", "Government", 
                              "Central banks", "Think tanks", "Other academic")),
      
      p(withMathJax("$$y_{it}=\\delta_{t}+\\gamma_{i}+\\varepsilon_{it}$$")),
      
      selectInput(inputId = "dataset4",
                  label = "Panel #4: Type-specific placement rate by field (choose type)*",
                  choices = c("Tenure-track", "Post-doc", "Private sector", "International organizations", "Government", 
                              "Central banks", "Think tanks", "Other academic")),
      
      h6("*Subsample of departments including Harvard, Princeton, Stanford, Columbia, LSE, Cornell, Maryland, and Warwick."),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
              fluidRow(
                column(6, plotOutput("distPlot")), 
                column(6, plotOutput("barchart"))
              ),
              
              fluidRow(
                column(6, plotOutput("coefplot")),
                column(6, plotOutput("barchart2"))
              )
              
    )

  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$distPlot<- renderPlot({
    
    x <- data %>% filter(uni == input$dataset) 
    
    ggplot(data = x, aes(x = as.factor(year),
                         fill = as.factor(reorder(type2,order)))) +
      geom_bar(position = "fill") + 
      ylab("Proportion") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = alpha(c("Other academic" = "powderblue",
                                         "Think tanks" = "seagreen1",
                                         "Central banks"="gold",
                                         "Government" = "gray9",
                                         "International organizations" = "purple",
                                         "Post-doc" = "steelblue2",
                                         "Private sector" = "red",
                                         "Tenure-track" = "royalblue"), 0.7))  +
      scale_colour_identity() +
      ggtitle(paste0("Panel #1: Placement distribution over time - ", input$dataset)) +
      theme_minimal() +
      theme(axis.title.x=element_blank()) +
      theme(axis.title.y=element_text(size=12)) +
      theme(axis.text.x = element_text(size=12, angle=45)) +
      theme(axis.text.y = element_text(size=12)) +
      theme(legend.title=element_blank()) +
      theme(legend.text=element_text(size=12)) +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 4)) +
      theme(plot.title = element_text(size = 13))
    
  })
  
  output$barchart<- renderPlot({
    
    x <- data %>% filter(year >= min(input$yearslider) & year <= max(input$yearslider)) 
    
    x <- x %>% group_by(uni) %>%
      add_count(name="n") %>%
      ungroup() %>%
      group_by(uni,type2) %>%
      add_count(name="n1")%>%
      transform(share=n1/n)
    
    x <- x %>% filter(type2 == input$dataset2) 
    x <- x %>% group_by(uni) %>% filter(row_number(uni)==1)
    
    
    ggplot(data = x, aes(x = reorder(as.factor(uni), -share),
                         y = share,
                         fill = as.factor(reorder(type2,share)))) +
      geom_bar(position = "dodge", stat = "identity") + 
      ylab("Proportion") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
      scale_fill_manual(values = alpha(c("Other academic" = "powderblue",
                                         "Think tanks" = "seagreen1",
                                         "Central banks"="gold",
                                         "Government" = "gray9",
                                         "International organizations" = "purple",
                                         "Post-doc" = "steelblue2",
                                         "Private sector" = "red",
                                         "Tenure-track" = "royalblue"), 0.7))  +
      scale_colour_identity() +
      ggtitle(paste0("Panel #2: ", input$dataset2, " placement rate (", min(input$yearslider), "-", max(input$yearslider), ")")) +
      theme_minimal() +
      theme(axis.text.x = element_text(size=10, angle=45)) +
      theme(axis.text.y = element_text(size=12)) +
      theme(axis.title.x=element_blank()) +
      theme(axis.title.y=element_text(size=12)) +
      theme(legend.text=element_text(size=12)) +
      theme(legend.title=element_blank()) +
      theme(legend.position="none") +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      theme(plot.title = element_text(size = 13))
    
  })
  
  output$coefplot<- renderPlot({
    
    x <- data %>% filter(type2 == input$dataset3)
    x$dummy <- 1
    
    x <- left_join(data, x)
    x$dummy <- ifelse(is.na(x$dummy), 0, x$dummy)
    x <- dummy_cols(x, select_columns = 'year')
    
    # Create a model to plot
    
    est <- feols(dummy ~ year_2013 + year_2014 + year_2015 + year_2016 + year_2017 + year_2018
                 + year_2019 + year_2020 + year_2021 + year_2022| uni, x)
    est_clu <- summary(est)
    
    coefs <- as.vector(est_clu$coefficients)
    se <- as.vector(est_clu$se)
    
    coefs <- data.frame(coefs, se)
    coefs$year <- c(2013:2022)
    coefs$year <- as.numeric(coefs$year)
    
    ggplot(coefs, aes(year, coefs)) + 
      geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
      geom_errorbar(aes(ymin=coefs - 1.96*se, ymax=coefs + 1.96*se), 
                    lwd=1, colour="red", width=0) +
      geom_point(size=4, pch=21, fill="red", color="red") +
      scale_x_continuous(labels = c(2013:2022), breaks = c(2013:2022)) +
      theme_minimal() +
      ggtitle(paste0("Panel #3: ", input$dataset3, " placement probabilities: \nestimated changes (relative to 2012)")) +
      theme(axis.title.y=element_text(size=12)) +
      ylab("Estimated coefficient") + 
      xlab("") +
      theme(plot.title = element_text(size = 13)) +
      theme(axis.text.x = element_text(size=12, angle=45)) +
      theme(axis.text.y = element_text(size=12))
    
  })
  
  output$barchart2<- renderPlot({
    
    x <- field 
    x <- x %>% filter(year >= 2016 & year <= 2022) 
    
    x <- x %>% group_by(primary_field_final) %>%
      add_count(name="n") %>%
      ungroup() %>%
      group_by(primary_field_final,type2) %>%
      add_count(name="n1")%>%
      transform(share=n1/n)
    
    x <- x %>% filter(type2 == input$dataset4) 
    x <- x %>% group_by(primary_field_final) %>% filter(row_number(primary_field_final)==1)
    
    ggplot(data = x, aes(x = reorder(as.factor(primary_field_final), -share),
                         y = share)) +
      geom_bar(position = "dodge", stat = "identity", fill="lightsalmon2", alpha = 0.7) + 
      ylab("Proportion") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
      theme_minimal() +
      ggtitle(paste0("Panel #4: ", input$dataset4, " placement rate by field (2016-2022)")) +
      theme(axis.text.x = element_text(size=10, angle=45)) +
      theme(axis.text.y = element_text(size=12)) +
      theme(axis.title.x=element_blank()) +
      theme(axis.title.y=element_text(size=12)) +
      theme(legend.text=element_blank()) +
      theme(legend.title=element_blank()) +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      theme(plot.title = element_text(size = 13))
    
    
  })
  
}

thematic_shiny(font = "auto")
shinyApp(ui = ui, server = server)