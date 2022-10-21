##################################
#      Data Visualization        #
#      Dr. Ho, Fall 2022         #
#      Assignment 5              #
##################################

#################################
#File Name: 
#    20221021_GCooper_EPPS 6356
#     _Assign5_Solution.R
#Theme: Produce 3 Charts
#Date: 10/24/22
#Version: 001.000
#Author: Glen Cooper
#################################


################################
#     Prepare Environment      #
################################
#Set working directory and clear data
setwd("C:/Users/glenc/downloads") #Set working directory
rm(list = ls())   # Clear environment
cat("\014")  # Clear the Console

#Load Libraries
library(readxl)
library(ggplot2)#library(scales)


################################
#  Import and Extract Data     #
################################

##Import data
#  NOTE: Excel file includes: country, year, countrycode, rd_per_gdp,
#  rd_per_bus, and rd_per_gov
Rd_data <- read_excel("20221020_CountryRDSpendperGDP.xlsx")

##Extract data into separate data sets
#Extract years 1999 and 2017 from data set
Rd_data_2yrs <- subset(Rd_data, year %in% c(1999, 2017))
#Extract United States from data set
Rd_data_us <- subset(Rd_data, country %in% "US")


################################
#     Generate Graphics        #
################################

##Create Side by Side Bar Charts of R&D % of GDP
plot <- ggplot(Rd_data_2yrs, aes(x = country_code, y = rd_per_gdp))
plot <- plot + facet_grid(~ year)
plot <-
  plot + geom_bar(stat = "identity",
                  position = 'dodge',
                  colour = "blue")
plot <-
  plot + scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
plot <-
  plot + ggtitle("Country Expenditure on R&D") + xlab("Country Code") +
  ylab("Total R&D % of GDP")
plot <- plot + coord_flip()
plot

##Create Comparison Column Chart of R&D % of GDP based on Year
plot <-
  ggplot(Rd_data_2yrs, aes(x = country_code, y = rd_per_gdp, fill = factor(year)))
plot <- plot + geom_bar(stat = "identity",
                        width = 0.7,
                        position = position_dodge(width = 0.8))
plot <-
  plot + scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
plot <-
  plot + ggtitle("Country Expenditure on R&D") + xlab("Country Code") +
  ylab("Total R&D % of GDP") + labs(fill = "Year")
plot

##Create Cyclical Data Chart of R&D % of GDP for US by Year
plot <- ggplot(Rd_data_us, aes(x = year, y = rd_per_gdp)) +
  geom_line()
plot <-
  plot + scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
plot <- plot + geom_point(size = 3, color = "red")
plot <- plot + geom_line(size = 1, color = "blue")
plot <-
  plot + ggtitle("United States Expenditure on R&D") + xlab("Year") +
  ylab("Total R&D % of GDP")
plot <- plot + coord_polar()
plot







