##################################
#      Data Visualization        #
#      Dr. Ho, Fall 2022         #
#      Assignment 7              #
##################################

#################################
#File Name: app.R
#Theme: Create Shiny App
#Date: 10/27/22
#Version: 001.000
#Author: Team Kiran
#################################

#################################
#* Synergy report
#* The team used a very basic Shiny app developed by Glen.
#* This app included only basic graphics in the server section.
#* Kiran added additional graphics. Then based on 
#* the entire team's review, additional adjustments
#* to the graphics and formatting were incorporated.
#* For example, Marcus used RColorBrewer to make the 
#* scatterplot colorblind-friendly,
#* and Wen formatted the timeline plot.
#################################



################################
#     Prepare Environment      #
################################
#Set working directory and clear data (use as needed)
#   setwd("C:/Users/glenc/downloads") #Set working directory
#   setwd("C:\\Users\\19728\\Desktop\\EPPS 6356\\Assignments")
rm(list = ls())   # Clear environment
cat("\014")  # Clear the Console

#Load Libraries
#install.packages("shinythemes")
library(readxl)
library(ggplot2)
library(shiny)
library(shinythemes)
#library(datasets)
#library(readr)


################################
#  Import and Extract Data     #
################################

##Import data
#  File name "20221020_CountryRDSpendperGDP.xlsx" loaded
#  and used in server section this script.
#  NOTE: Excel file includes: country, year, countrycode, rd_per_gdp,
#  rd_per_bus, and rd_per_gov


Rd_data <- read_excel("20221020_CountryRDSpendperGDP.xlsx")

################################
#       Generate Shiny         #
################################

##
## Define UI for dataset viewer app ----
##
ui <- fluidPage(# App title ----
                titlePanel("R&D per GDP Analyses"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Two years to compare
                    numericInput(
                      inputId = "year1",
                      label = "Enter first comparison year (1999 to 2016):",
                      value = 1999,
                      min = 1999,
                      max = 2016
                    ),
                    
                    numericInput(
                      inputId = "year2",
                      label = "Enter second comparison year (2000 to 2017):",
                      value = 2017,
                      min = 2000,
                      max = 2017
                    ),
                    
                    # Input: Country of Comparison
                    # Input: Selector for choosing country ----
                    selectInput(
                      inputId = "countrynam",
                      label = "Choose country:",
                      choices = c(
                        "Argentina",
                        "Brazil",
                        "Canada",
                        "China",
                        "Colombia",
                        "Germany",
                        "France",
                        "UK",
                        "Italy",
                        "Japan",
                        "South Korea",
                        "Singapore",
                        "US",
                        "South Africa"
                      )
                    )
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    # Output:  ----
                    plotOutput("summary"),
                    
                    plotOutput("timeline"),
                    
                    plotOutput("plot2"),
                    
                    plotOutput("plot3")
                    
                  )
                ))


##
## Define server logic to summarize and view selected dataset ----
##
server <- function(input, output) {
  mydata <- reactive({
    yr1 <- input$year1
    yr2 <- input$year2
    conam <- input$countrynam
    #Extract data into a separate data set
    Rd_data <- read_excel("20221020_CountryRDSpendperGDP.xlsx")
    Rd_data_2yrs <- subset(Rd_data, year %in% c(yr1, yr2))
    Rd_data_country <- subset(Rd_data_2yrs, country %in% conam)
    Rd_data_country
  })
  
  mydata2 <- reactive({
    conam <- input$countrynam
    #Extract data into a separate data set
    Rd_data <- read_excel("20221020_CountryRDSpendperGDP.xlsx")
    Rd_data_country <- subset(Rd_data, country %in% conam)
    Rd_data_country
  })
  
  mydata3 <- reactive({
    yr1 <- input$year1
    yr2 <- input$year2
    #Extract data into a separate data set
    Rd_data <- read_excel("20221020_CountryRDSpendperGDP.xlsx")
    Rd_data_2yrs <- subset(Rd_data, year %in% c(yr1, yr2))
    Rd_data_2yrs
  })
  
  
  #Summary Plot
  library(tidyr)
  library(RColorBrewer)
 #Show colorblind friendly choices 
  output$summary <- renderPlot({
    md3 <- mydata3()
    plot <-
      ggplot(data = md3, aes(x = rd_per_bus, y = rd_per_gov)) + 
      geom_point(aes(colour = factor(country)), size = 3) +
      scale_color_brewer(palette="Paired") + #Select "Paired" palette because there are 12 colors and we have 12 countries
      guides(color = guide_legend(title = "Country")) +
      ggtitle("GDP by Business vs. GDP by Government for All Countries for Selected Years") + 
      xlab("% R&D of Business") + 
      ylab("% R&D of Government")
    plot
  })
  
  #Timeline Plot
  output$timeline <- renderPlot({
    md <- mydata()
    md2 <- mydata2()
    plot <-
      ggplot(md2, aes(x = round(year,1), y = rd_per_gdp)) + geom_point() + geom_line() + geom_point(data =
                                                                                             md, aes(
                                                                                               x = year,
                                                                                               y = rd_per_gdp,
                                                                                               size = 3
                                                                                             )) + theme(legend.position = "none") + xlab("Year") + ylab("% R&D of GDP") + ggtitle("Total R&D as a % of GDP for Selected Countries by Year")
    plot
  })
  
  #Compare year over year country total R&D Plot 1
  output$plot1 <- renderPlot({
    md <- mydata()
    plot <- ggplot(md, aes(x = year, y = rd_per_gdp, fill = year))+ geom_label(aes(label =round( rd_per_gdp,2)), vjust=0, fontface = "bold", family = "Fira Sans", fill = "white", label.size = 0)
    plot <-
      plot + geom_bar(stat = "identity",
                      position = 'dodge',
                      colour = "blue")
    plot <-
      plot + scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
    
    plot <- plot +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      )
    
    plot <-
      plot + ggtitle("Total Country Expenditure on R&D") + xlab("Year") +
      ylab("% R&D of GDP")
    plot
  })
  
  
  #Compare year over year country business R&D Plot 2
  output$plot2 <- renderPlot({
    md <- mydata()
    plot <- ggplot(md, aes(x = year, y = rd_per_bus, fill = year))+ geom_label(aes(label =round( rd_per_bus,2)), vjust=0, fontface = "bold", family = "Fira Sans", fill = "white", label.size = 0)
    plot <-
      plot + geom_bar(stat = "identity",
                      position = 'dodge',
                      colour = "blue")
    plot <-
      plot + scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
    
    plot <- plot +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      )
    
    plot <-
      plot + ggtitle("Country Expenditure on R&D by Business") + xlab("Year") +
      ylab("% R&D of Business")
    plot
  })
  
  #Compare year over year country government R&D Plot 3
  output$plot3 <- renderPlot({
    md <- mydata()
    plot <- ggplot(md, aes(x = year, y = rd_per_gov, fill = year)) + geom_label(aes(label =round( rd_per_gov,2)), vjust=0, fontface = "bold", family = "Fira Sans", fill = "white", label.size = 0)
    plot <-
      plot + geom_bar(stat = "identity",
                      position = 'dodge',
                      colour = "blue")
    plot <-
      plot + scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
    
    plot <- plot +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      )
    
    plot <-
      plot + ggtitle("Country Expenditure on R&D by Government") + xlab("Year") +
      ylab("% R&D of Government")
    plot
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

