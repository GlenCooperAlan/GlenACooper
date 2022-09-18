#############################
#          Graphics         #
#        Happy Planet       #
#           Data            #
#############################

#############################
#File Name: Happyplanent01.R
#Theme: Graphic Analysis
#Date: 09/17/2022
#Version: 001.001
#Author: Glen Cooper
############################

############################
#     Initialization       #
############################

rm(list=ls()) # Clear environment
oldpar <- par() # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")    

###########################
#        Libraries        #
###########################

library(readxl)
library(dplyr)


###########################
#          Data           #
###########################

#Import data
HP_data_set <- 
  read_excel("C:/Users/glenc/Downloads/happy-planet-index-2009-2014-2019-public-data-set.xlsx")

##Dataframe Review
names(HP_data_set)   #Review dataframe names
str(HP_data_set)  #Review classes
countna <- function(x){sum(is.na(x))} #Define count the number of nas function
sapply(HP_data_set, countna) #Count number of nas within dataframe
glimpse(HP_data_set) #Review data by line
summary(HP_data_set)  #Summarize dataframe components

###########################
#        Coding           #
###########################


## Scatterplot
#Create dataframes
country <- HP_data_set$Country
year <- HP_data_set$Year
wellbeing <- HP_data_set$Wellbeing
df_with_na <- data.frame(country, year, wellbeing)
df <- na.omit(df_with_na)
df_USA <- df[df$country =="United States of America",]
df_UK <- df[df$country =="United Kingdom",]
df_France <- df[df$country =="France",]
df_Germany <- df[df$country =="Germany",]

#Draw plots
plot.new()
par(mfrow=c(2, 2)) # Setting the parameter (2 rows by 2 cols)
par(las=1, mar=c(4, 4, 2, 4), cex=.7) #Set label orientation, margins c(bottom, left, top, right) & text size
plot(df_USA$year, df_USA$wellbeing, type='l', lty=3,col='red',lwd=2,main="USA",col.main='blue',xlab="Year",ylab="Wellbeing", cex.lab = .9)
plot(df_UK$year, df_UK$wellbeing, type='l', lty=3,col='red',lwd=2,main="UK",col.main='blue',xlab="Year",ylab="Wellbeing", cex.lab = .9)
plot(df_France$year, df_France$wellbeing, type='l', lty=3,col='red',lwd=2,main="France",col.main='blue',xlab="Year",ylab="Wellbeing", cex.lab = .9)
plot(df_Germany$year, df_Germany$wellbeing, type='l', lty=3,col='red',lwd=2,main="Germany",col.main='blue',xlab="Year",ylab="Wellbeing", cex.lab = .9)


## Histogram
#Create dataframes
#Using all country data after removing na's

#Draw plot
plot.new()
par(mfrow=c(1, 1)) # Setting the parameter (1 rows by 1 cols)
h <- hist(df$wellbeing, main = "All Country Wellbeing (2009, 2014, 2019)", cex.main = .9,
     col.main = 'blue', xlab = "Wellbeing", ylab = "Frequency", col = "lightblue")
     #Storing the histogram in h allows access to histogram variables such as counts
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5), cex = .5) #Display the counts on each bar


## Barplot
#Create dataframe
#Using only USA, UK, France, Germany dataframes

#Draw plot
plot.new()
par(mfrow=c(2, 2)) # Setting the parameter (2 rows by 2 cols)
barplot(height = df_USA$wellbeing, names=df_USA$year, col = "lightblue", 
        main="USA",col.main='blue',xlab="Year",ylab="Wellbeing", cex.lab = .9)
barplot(height = df_UK$wellbeing, names=df_UK$year, col = "lightblue", 
        main="UK",col.main='blue',xlab="Year",ylab="Wellbeing", cex.lab = .9)
barplot(height = df_France$wellbeing, names=df_France$year, col = "lightblue", 
        main="France",col.main='blue',xlab="Year",ylab="Wellbeing", cex.lab = .9)
barplot(height = df_Germany$wellbeing, names=df_Germany$year, col = "lightblue", 
        main="Germany",col.main='blue',xlab="Year",ylab="Wellbeing", cex.lab = .9)


## Boxplot
#Create dataframes
df_2019 <- df[df$year ==2019,]
df_2014 <- df[df$year ==2014,]
df_2009 <- df[df$year ==2014,]

#Draw plot
plot.new()
par(mfrow=c(1, 1)) # Setting the parameter (1 rows by 1 cols)
par(mar=c(5.1, 4.1, 4.1, 2.1)) #These are the default parameters
boxplot(df_2019$wellbeing, df_2014$wellbeing, df_2009$wellbeing, 
        col = c("orange", "white", "blue"), xlab = "Year", ylab = "Wellbeing Index", 
        main = "Worldwide Wellbeing by Year", names = c("2019", "2014", "2009"))


## Persp
#NOT USED


## Piechart
#NOT USED

