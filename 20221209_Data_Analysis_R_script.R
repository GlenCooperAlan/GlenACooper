##############################################################################################
#Administration
#File Name: EPPS6356_R Analysis Files_Glen.R
#Theme: Predict ASEAN Country Sentiment
#Date: 11/24/2022
#Version: 001.000
#Author: Glen Cooper
#############################################################################################
#Author: Glen Cooper 11/15/2022
rm(list = ls())   # Clear environment
oldpar <- par()   # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"]))
  dev.off(dev.list()["RStudioGD"])   # Clear plot window
options(warn = 0) #Enable global warnings.  Note to disable use: options(warn = -1)
cat("\014")  # Clear the Console

##### NAMING CONVENTIONS#####################################################################
#Dataframe & matrixes names: Begins with capital letter and separated by "_"  e.g., Data_name
#Models: Begins with capital letter and separated by "." e.g., Model.name
#Values & vectors: Begin with all capital letters and separated by "_" e.g.,  VARIABLE_name
#Functions: Begins with lowercase letter and separated by "." e.g., function.name
# Major blocks identified with three line block heading, minor blocks with one line heading
# After Major and Minor blocks the following subblock conventions used:
# "##Major subblock heading"
# "#Minor subsubblock heading"
#Ctrl+Shift+A used to reformat highlighted code
#############################################################################################

##############################################################################
#                         Packages                                           #
#                           and                                              #
#                        Libraries                                           #
##############################################################################

#Turn off warnings
options(warn = -1)

##Packages
#install.packages("statnet", dependencies = TRUE)
#install.packages("easystats")
# Website for easystats (https://github.com/easystats/easystats) recommended 
# this command: easystats::install_suggested()
#easystats::install_suggested()

##Libraries
library(readxl)       # Read in Excel files
library(dplyr)        # Review data
library(tidyverse)    # Manipulate data
library(stats)        # Regressions functions
library(ISLR2)        # Regression functions
library(leaps)        # Regression functions
library(glmnet)       # Regression functions
library(pls)          # Regression functions
library(jsonlite)     # JSON parser and generator
library(jsonify)      # Converts Between 'R' Objects and Javascript Object Notation (JSON)
library(lubridate)    # Dates and times manipulate package
library(igraph)       # Package for algorithmic static graphical images
library(pacman)       # Allows loading packages (viz., library or require) & used to animate images
library(magick)       # Advanced Image-Processing package & used to animate images
library(stringr)      # Cohesive functions designed to work with strings
library(writexl)      # Allows exporting of df to Excel file
library(statnet)      # Exponential Random Graph Modeling (ERGM) package
library(network)      # Creates network objects from dataframes, etc. object
library(performance)  # Utilities for computing measures to assess model quality
library(easystats)    # Provides consistent framework for R statistics analysis
library(report)       # easystats data frame report
library(ggplot2)      # Graphical modules
library(plotly)       # Graphical modules
library(scales)       # Calculation tool

#Turn on warnings
options(warn = 0)

##############################################################################
#                           Variables                                        #
#                              and                                           #
#                           Standards                                        #
##############################################################################

##Variables
#Set working directory path, data sub path, and file names
Path_wd <- "D:/glenc/Documents/20221122_EPPS 6356 Data Vis Project_Glen"
setwd(Path_wd)
Path_data <-"/Project Data Sources/"
File_json <-  "ASEAN_Eventdata.json"
File_excel01 <- "EPPS6356_Data4R_TEMP.xlsx"
#File_excel02 <- "EPPS6356_DatafromR.xlsx"
File_excel03 <- "China-Global-Investment-Tracker-2022-SPRING-final-1.xlsx"


##Standards
#set.seed(123)


##############################################################################
#                                Load                                        #
#                                and                                         #
#                             Process Data                                   #
##############################################################################


#*Data sources include:
#  UTDEventData: KateHyoung/UTDEventData
#  UNESCO / UIS.Stat: https://en.unesco.org/
#  GEM - Global Entrepreneurship Monitor: https://www.gemconsortium.org/data/key-aps
#  World Bank: https://datacatalog.worldbank.org/public-licenses#cc-by
#  The Economist Intelligence Unit: https://infographics.economist.com/2018/DemocracyIndex/
#  UNESCO / World Bank: http://uis.unesco.org/
#  Inter-Parliamentary Union (IPU) / World Bank: www.ipu.org
#  UN's International Telecommunication Union (ITU): https://www.itu.int/en/ITU-D/Pages/default.aspx
#  China Global Investment Tracker from https://www.aei.org/china-global-investment-tracker/ 



##############################################################################
#                     Load Data from Events File                             #
##############################################################################

##Load JSON file and create data frame
Eventdata <-
  fromJSON(
    paste(Path_wd, Path_data, File_json,sep = "")
  ) %>% as.data.frame

#Create dataframe using "Source Country," "Target Country," "CAMEO Code," and "Event Date" columns
#using transitory data files: df1, df2, etc. and root_code
df1 <-
  data.frame(
    Eventdata$`Source Country`,
    Eventdata$`Target Country`,
    Eventdata$`CAMEO Code`,
    Eventdata$`Event Date`
  )

#Change column names
colnames(df1) <-
  c("Source_Country",
    "Target_Country",
    "CAMEO_Code",
    "Event_Date") 

#Create separate columns for year (i.e, "year = lubridate::year(Event_Date)")
#leaving out month (i.e., "month = lubridate::month(Event_Date)") 
#and date (i.e., "day = lubridate::day(Event_Date)")
df2<- df1 %>%
  dplyr::mutate(year = lubridate::year(Event_Date))

#Convert CAMEO Code to root_code
#Create matrix
matrix <- as.matrix(df2)
#Convert codes, create new dataframe and remain column "V1" to root_code
#*Neutral sentiment codes: 01, 02
#*Cooperation sentiment codes: 03, 04, 05, 06, 07, 08
#*Conflict sentiment codes: 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
root_code<-str_extract(df2$CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
df3 <- cbind(df2, root_code)
names(df3)[names(df3) == 'V1'] <- 'root_code'

#Filter to get the desired Source Countries (that match country demographic)
#  data and not Target Countries (excluding China from Target Countries)
#  not including "CAMEO_Code", "Event_Date" values
#  and including only selected years
df4 <- data.frame(df3$Source_Country, df3$Target_Country, df3$year, df3$root_code) |>
  #Filter on China as Source_Country and ASEAN Countries as Target_Country
  filter(df3.Source_Country %in% c("China")) |>
  filter(df3.Target_Country %in% c("Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore",
                                   "Thailand", "Vietnam"))|>
  #Filter by desired year 
  filter(df3.year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020'))
#UNUSED FILTERS:
#Filter by desired month (January = '1'; February = '2'; March = '3'; April = '4')
#filter(df3.month %in% c('1'))

#Remove China as a Target_Country because China cannot be "eventing" with itself
df4<- subset(df4, df3.Target_Country != "China")


#Change column names to be compatible with PREDICTORS_df (see below)
colnames(df4) <-
  c("COUNTRY", "Target_Country",
    "YEAR",
    "CAMEO_root") 

#Move the YEAR column after the COUNTRY column to be consistent with PREDICTORS_df (see below)
df4 <- df4 %>% relocate(YEAR, .after = COUNTRY)

#Remove all rows with NAs
df5 <- na.omit(df4)

#Transform dataframe CAMEO_root into numeric rather than characters
df5 <- transform(df5, CAMEO_root = as.numeric(CAMEO_root))

#Add the category of CAMEO code to df as CAMEO_Status
df5 <-
  df5 %>% plyr::mutate(
    CAMEO_Status = dplyr::case_when(
      CAMEO_root %in% c(1, 2) ~ "Neutral",
      CAMEO_root %in% c(3, 4, 5, 6, 7, 8) ~ "Cooperative",
      CAMEO_root %in% c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20) ~ "Conflictual"
    )
  ) 

#Add the category of CAMEO code to df as CAMEO_Status_val
df5 <-
  df5 %>% plyr::mutate(
    CAMEO_Status_val = dplyr::case_when(
      CAMEO_root %in% c(1, 2) ~ 0,
      CAMEO_root %in% c(3, 4, 5, 6, 7, 8) ~ 1,
      CAMEO_root %in% c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20) ~ -1
    )
  ) 


#Create final dataframe by removing all originating countries except China
#and removing Target_Country where China is the target (no "China" on "China" communication)
df6 <- df5 %>% as.data.frame

China_df_ASEAN_CAMEO_Targets<- df6

##Clean up this section
#Remove transitory data files and variables
rm(df1, df2, df3, df4, df5, df6, Eventdata, matrix, root_code)


##############################################################################
#                Load Data from National Demographics File                   #
##############################################################################

##Load Excel file and create data frame
#Read the Excel R&D response & predictors file and load into dataframe
df1 <-
  read_excel(
    paste(Path_wd, Path_data, File_excel01,sep = ""),
    col_names = TRUE,
    na = "",
    trim_ws = TRUE
  ) 

#Convert data to data frame and create final dataframe
PREDICTORS_df <-
  as.data.frame(df1)


##Merge dataframes
#Merge preliminary dataframes based on Country and Year
df2 <- merge(China_df_ASEAN_CAMEO_Targets, PREDICTORS_df, by.ASEAN_df=c("COUNTRY", "YEAR"))

#Transform dataframe into numeric rather than characters
#*Variable descriptions:
# COUNTRY = Country Name
# YEAR = Year
# CAMEO_root_mean = Average CAMEO code from Country by year
# DEMINDX = Economist Intelligence Unit's Democracy Index
# EFCPI = Commercial and professional infrastructure
# EFIMD = Internal market dynamics
# EFIMO = Internal market openness
# INDPERINT = Percentage of Individuals using the Internet 
# ACCESSELEC = Access to electricity (% of population)
# ARMFORCEPERLABOR = Armed forces personnel (% of total labor force)
# GDPMKTP = GDP growth (annual %)
# GDITOTL = Gross capital formation (% of GDP)
# MILXPND = Military expenditure (% of GDP)
# TRDPERGDP = Trade (% of GDP)
# FMBACH = Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative)
# MABACH = Educational attainment, at least Bachelor's or equivalent, population 25+, male (%) (cumulative)
# GINI = GINI index (World Bank estimate)
# FMPAR = Proportion of seats held by women in national parliaments (%)
# FMIN = Proportion of women in ministerial level positions (%)
# STRGLEG = Strength of legal rights index (0=weak to 12=strong)
df2 <- transform(df2, 
                 CAMEO_root = as.numeric(CAMEO_root),
                 CAMEO_Status_val= as.numeric(CAMEO_Status_val),
                 DEMINDX = as.numeric(DEMINDX),
                 EFCPI = as.numeric(EFCPI),
                 EFIMD = as.numeric(EFIMD),
                 EFIMO = as.numeric(EFIMO),
                 INDPERINT = as.numeric(INDPERINT),
                 ACCESSELEC = as.numeric(ACCESSELEC),
                 ARMFORCEPERLABOR = as.numeric(ARMFORCEPERLABOR),
                 GDPMKTP = as.numeric(GDPMKTP),
                 GDITOTL = as.numeric(GDITOTL),
                 MILXPND = as.numeric(MILXPND),
                 TRDPERGDP = as.numeric(TRDPERGDP),
                 FMBACH = as.numeric(FMBACH),
                 MABACH = as.numeric(MABACH),
                 GINI = as.numeric(GINI),
                 FMPAR = as.numeric(FMPAR),
                 FMIN = as.numeric(FMIN),
                 STRGLEG = as.numeric(STRGLEG))


#Move the CAMEO_Status column after the CAMEO_root column
df2 <- df2 %>% relocate(CAMEO_Status, .after = CAMEO_root)

##Final Base dataframe creation
Base_df <- as.data.frame(df2)

#Group by Country and Year and calculate mean values for all variables;
#Note: for all variables that are not CAMEO_root the mean will just be
#the yearly value because that value did not vary during the year
Base_df_mean <- Base_df %>% 
  group_by(COUNTRY, Target_Country, YEAR) %>% 
  summarise(across(c(CAMEO_root, CAMEO_Status_val, DEMINDX, EFCPI, EFIMD, EFIMO, 
                     INDPERINT, ACCESSELEC,ARMFORCEPERLABOR, 
                     GDPMKTP, GDITOTL, MILXPND, TRDPERGDP, 
                     FMBACH, MABACH, GINI, FMPAR, 
                     FMIN, STRGLEG), mean))

#Change name of CAMEO_root and CAMEO_Status_val
names(Base_df_mean)[names(Base_df_mean) == 'CAMEO_root'] <- 'CAMEO_root_mean'
names(Base_df_mean)[names(Base_df_mean) == 'CAMEO_Status_val'] <- 'CAMEO_Status_val_mean'

##Clean up this section
#Write dataframe to Excel file for future review
#write_xlsx(Base_df_mean, paste(Path_wd, Path_data, File_excel02 ,sep = ""))
#Remove transitory data files and variables

rm(df1, df2, PREDICTORS_df)

##############################################################################
#               Load Data from China Investment Tracker                      #
##############################################################################

#Process data from https://www.aei.org/china-global-investment-tracker/ 
#in China-Global-Investment-Tracker-2022-SPRING-final-1.xlsx

df1 <-
  read_excel(
    paste(Path_wd, Path_data, File_excel03,sep = ""),
    col_names = TRUE,
    na = "",
    trim_ws = TRUE
  ) 


##Convert data to data frame and create final dataframe
df2 <-
  as.data.frame(df1)
#Rename column(s)
df2 <- df2 %>% rename("Invest_by_China_mils" = "Quantity in Millions")

#Extract data
df3 <- data.frame(df2$Country, df2$Year, df2$Invest_by_China_mils) |>  
  # Filter by ASEAN countries
  filter(df2.Country %in% c("Cambodia", "Indonesia", "Laos", "Malaysia", 
                            "Myanmar", "Philippines", "Singapore", "Thailand",
                            "Vietnam", "Brunei")) |>
  filter(df2.Year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))

#Convert data to data frame and create final dataframe
df4 <-
  as.data.frame(df3)

colnames(df4) <-
  c("Target_Country",
    "YEAR",
    "Invest_by_China_mils") 

#Add the column for COUNTRY and fill it with China
df4 <- add_column(df4, COUNTRY = "China", .before = "Target_Country")

#Transform dataframe into numeric rather than characters
#*Variable descriptions:
#Year = Year 
#Country = Country Name
#Invest_by_China_mils =  Investment made by China, in millions
df4 <- transform(df4, 
                 Invest_by_China_mils = as.numeric(Invest_by_China_mils))

#Calculate the sum of China investment by Target_Country and YEAR
#and create a dataframe China_ASEAN_Invest
China_df_ASEAN_Invest <- df4 %>% 
  group_by(COUNTRY, Target_Country, YEAR) %>% 
  summarise(across(Invest_by_China_mils, sum))

#Move the YEAR column after the COUNTRY column to be consistent with other dataframes
China_df_ASEAN_Invest <- China_df_ASEAN_Invest %>% relocate(YEAR, .after = COUNTRY)

#Merge the China_ASEAN_Invest dataframe into the Base_df_mean dataframe 
#keeping all Base_df_mean values using a left join strategy
Base_df_meanCAMEO_invest <- merge(x=Base_df_mean, y=China_df_ASEAN_Invest,
                                  by=c("COUNTRY", "Target_Country", "YEAR"), all.x=TRUE)

#Move the YEAR column after the COUNTRY column to be consistent with other dataframes
Base_df_meanCAMEO_invest <- Base_df_meanCAMEO_invest %>% relocate(YEAR, .after = COUNTRY)


#Remove Base_df_mean as now redundant #with the data in Base_df_mean_invest
rm(Base_df_mean)

##Clean up this section
#Remove transitory data files and variables
rm(df1, df2, df3, df4)


##############################################################################
#                            Graphical                                       #
#                           Processing                                       #
#                                                                            #
##############################################################################


##############################################################################
#                           3D Scatter Plot                                  #
##############################################################################

##3D scatter plot 01
#*Title:        Annual Mean CAMEO by Year & Target Country
#*Propose:      Determine which ASEAN countries are being impacted by events between
#*              China and ASEAN countries over time.
#*#*Conclusion: Most CAMEO Codes are in are at the Cooperation level and that rating is
#*              is consistent over the years, resulting in little information available
#*              to show variance between ASEAN countries.

#*Turn off warings
options(warn=-1)

#*Construct graphic
Plot3D00 <- plot_ly(x=Base_df_meanCAMEO_invest$Target_Country, y=Base_df_meanCAMEO_invest$YEAR, 
                    z=Base_df_meanCAMEO_invest$CAMEO_Status_val_mean, type="scatter3d",
                    mode="markers")
Plot3D01 <- Plot3D00  %>%  add_markers()
Plot3D01 <- Plot3D01 %>% layout(scene = list(xaxis = list(title = 'Target Country'),
                                             yaxis = list(title = 'Year'),
                                             zaxis = list(title = 'CAMEO: Coop. = 1, Neutral = 0, Conflict= -1')))
Plot3D01 <- Plot3D01 %>% layout(title = 'Annual Mean CAMEO by Year & Target Country',
                                plot_bgcolor = "#e5ecf6")
#Set up layout formatting
axx <- list(
  ticks="outside",
  tickangle=45, 
  tickfont = list(family='Courier', color='orange', size=12),
  backgroundcolor="rgb(200, 200, 230",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)

axy <- list(
  ticks="outside",
  tickangle=45, 
  tickfont = list(family='Courier', color='orange', size=12),
  backgroundcolor="rgb(230, 200,230)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)
axz <- list(
  tickfont = list(family='Courier', color='orange', size=12),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)
#Layout Display
Plot3D01 <- Plot3D01 %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
Plot3D01 <- Plot3D01 %>% layout(showlegend = FALSE)
Scatter_3D_by_country_year <- Plot3D01

##Clean up this section / Remove transitory data files and variables
rm(Plot3D00, Plot3D01, axx, axy, axz)

#Display Graph
Scatter_3D_by_country_year

#*Turn on warings
options(warn=0)


##############################################################################
#                              Bar Charts                                    #
##############################################################################

##Bar Chart 01
#*Title:        Bar plot by Country on CAMEO_Status
#*Propose:      Determine which ASEAN countries are being impacted by events between
#*               China and ASEAN countries.
#*#*Conclusion: Most CAMEO Codes are between China and Cambodia, resulting in
#*              little information available to show variance between ASEAN countries.

#*Turn off warings
options(warn=-1)

#*Construct graphic
Bar00 <- ggplot(Base_df, aes(x=Target_Country, fill=CAMEO_Status))
Bar01 <- Bar00 + theme_bw() + geom_bar()
Bar01 <- Bar01 + labs(x="Target ASEAN Country", y="Count CAMEO Type", 
                      title = "CAMEO Status by Country")
Bar01 <- Bar01 +  guides(fill=guide_legend(title = "CAMEO Status"))
Bar_CAMEO_by_country <- Bar01

##Clean up this section / Remove transitory data files and variables
rm(Bar00, Bar01)

#Display Graph
Bar_CAMEO_by_country

#*Turn on warings
options(warn=0)


##############################################################################
#                              Scatter Charts                                #
##############################################################################

##* Scatter Chart 01
#*Title:      Scatter Plot of Value of CAMEO Status vs Democracy Index
#*Purpose:    To compare CAMEO mean values based on the average CAMEO status values
#*            (i.e., 1 = Cooperation, 0 = Neutral, and -1 = Conflict) to country 
#*            Democracy Index values and to project useful correlations.
#*Conclusion: While there is possibly some trend the graph is showing two separate
#*            trend lines and has a very wide confidence interval.  No correlation indicated.

#*Turn off warings
options(warn=-1)

#*Construct graphic
Scatter00 <- ggplot(Base_df_meanCAMEO_invest, aes(x=CAMEO_Status_val_mean, 
                                                  y=DEMINDX, shape=Target_Country,
                                                  color=Target_Country))
Scatter01 <- Scatter00 + theme_classic()
Scatter01 <- Scatter01 + geom_point(size=2, shape=24)
Scatter01 <- Scatter01 + geom_rug()
Scatter01 <- Scatter01 + geom_smooth(method=lm, linetype="dashed", 
                                     color="red", fill="lightblue")
Scatter01 <- Scatter01 + ggtitle("CAMEO Mean Status Value to Democracy Index")
Scatter01 <- Scatter01 + labs(colour = "Targeted Country")
Scatter01 <- Scatter01 + labs(x="CAMEO Mean Status Value", y="Democracy Index")
Scatter01 <- Scatter01 + xlim(0.7, 1.00) #Limit x axis values to better display graphic
Scatter_Dem_Indx <- Scatter01

##Clean up this section / Remove transitory data files and variables
rm(Scatter00, Scatter01)

#Display Graph
Scatter_Dem_Indx

#*Turn on warings
options(warn=0)


##* Scatter Chart 02
#*Title:      Scatter Plot of Value of CAMEO Status vs Military Expenditure as Percent of GDP 
#*Purpose:    To compare CAMEO mean values based on the average CAMEO status values
#*            (i.e., 1 = Cooperation, 0 = Neutral, and -1 = Conflict) to country 
#*            annual military expenditure as a percent of GDP and to project useful correlations.
#*Conclusion: There is little correlation between CAMEO status and the amount of
#*            military expenditures by ASEAN countries.

#*Turn off warings
options(warn=-1)

#*Construct graphic
Scatter00 <- ggplot(Base_df_meanCAMEO_invest, aes(x=CAMEO_Status_val_mean, 
                                                  y=MILXPND,
                                                  shape=Target_Country,
                                                  color=Target_Country))
Scatter01 <- Scatter00 + theme_classic()
Scatter01 <- Scatter01 + geom_point(size=2, shape=24)
Scatter01 <- Scatter01 + geom_rug()
Scatter01 <- Scatter01 + geom_smooth(method=lm, linetype="dashed", 
                                     color="red", fill="lightblue")
Scatter01 <- Scatter01 + ggtitle("CAMEO Mean Status Value to Military Expense % GDP")
Scatter01 <- Scatter01 + labs(colour = "Targeted Country")
Scatter01 <- Scatter01 + labs(x="CAMEO Mean Status Value (Cooperation=1, Neutral=0, Conflict=-1)", 
                              y="Military Expenditures percent of GDP")
Scatter_Military_Exp <- Scatter01

##Clean up this section / Remove transitory data files and variables
rm(Scatter00, Scatter01)

#Display Graph
Scatter_Military_Exp

#*Turn on warings
options(warn=0)


##* Scatter Chart 03
#*Title:      Scatter Plot of Value of CAMEO Status vs China Investment 
#*Purpose:    To compare CAMEO mean values based on the average CAMEO status values
#*            (i.e., 1 = Cooperation, 0 = Neutral, and -1 = Conflict) to country 
#*            annual investments in that country and to project useful correlations.
#*Conclusion: There is little correlation between CAMEO status and China's investment

#*Turn off warings
options(warn=-1)

#*Construct graphic
Scatter00 <- ggplot(Base_df_meanCAMEO_invest, aes(x=CAMEO_Status_val_mean, 
                                                  y=Invest_by_China_mils/1000,
                                                  shape=Target_Country,
                                                  color=Target_Country))
Scatter01 <- Scatter00 + theme_classic()
Scatter01 <- Scatter01 + geom_point(size=2, shape=24)
Scatter01 <- Scatter01 + geom_rug()
Scatter01 <- Scatter01 + geom_smooth(method=lm, linetype="dashed", 
                                     color="red", fill="lightblue")
Scatter01 <- Scatter01 + ggtitle("CAMEO Mean Status Value to China Investment")
Scatter01 <- Scatter01 + labs(colour = "Targeted Country")
Scatter01 <- Scatter01 + labs(x="CAMEO Mean Status Value (Cooperation=1, Neutral=0, Conflict=-1)", 
                              y="Annual Invest by China (Bil)")
Scatter01 <- Scatter01 + xlim(0.8, 1.0) #Limit x axis values to better display graphic
Scatter01 <- Scatter01 +ylim(0.0, 7.0) #Limit y axis values to better display graphic
Scatter_China_Invest <- Scatter01

##Clean up this section / Remove transitory data files and variables
rm(Scatter00, Scatter01)

#Display Graph
Scatter_China_Invest

#*Turn on warings
options(warn=0)

##############################################################################
#                             Linear Models                                  #
#                          Basic Linear Model                                #
#                             Regressions                                    #
##############################################################################


##############################################################################
#                          Simple Linear Regressions                         #
##############################################################################


#Drop predictors that are linear dependent (LD)
#Generally, a correlation of 70% to 80% should be of concern.
#Drop ARMFORCEPERLABOR (Armed forces personnel % of total labor force) LD on TRDPERGDP (Trade (% of GDP)
#Drop FMBACH (Females with BS degrees) LD on multiple independent variables
#Drop MABACH (Males with BS degrees) LD on multiple independent variables
#Drop Country, Year, Target Country, and CAMEO_Status_val_mean as not relevant to this analaysis
df1 <-
  Base_df_meanCAMEO_invest[, c(-1, -2, -3, -5, -12, -17, -18)]


##All in Model
m1 <- lm(CAMEO_root_mean~., data = df1)
summary(m1)$coefficient
summary(m1)
model_performance(m1)
model_dashboard(m1)
#*CONCLUSION: Several variables where dropped because of singularity.  The only those variables 
#*that produced estimates were maintained.  Note that the Democracy index reports a slight relationship.
#*Additionally, the Internal market dynamics and openness are reporting a slightly larger relationship, 
#*however, given the data set it is difficult to consider these relationships meaningful.


#All in Model excluding those causing singularity
m2 <- lm(CAMEO_root_mean~ DEMINDX + EFCPI + EFIMD + EFIMO + Invest_by_China_mils, data = df1)
summary(m2)$coefficient
summary(m2)
model_performance(m2)
model_dashboard(m2)
#CONCLUSION: This model reports the same results as model #1 but removes
#the singularity variables from the output.



##Democracy Index, Military expendiatures, and China investment model
m3 <- lm(CAMEO_root_mean~ DEMINDX + MILXPND + Invest_by_China_mils, data = df1)
summary(m3)$coefficient
summary(m3)
model_performance(m3)
model_dashboard(m3)
#CONCLUSION: No signficant variables or R2

##Clean up this section
#Remove transitory data files and variables
rm(df1, m1, m2, m3)


##############################################################################
#                      Parametric Linear Models                              #
#               Stepwise (forward/backward) & Best Subset Selection          #
#                              Regression                                    #
##############################################################################


##############################################################################
#                  Linear Regression Forward Stepwise                        #
##############################################################################

##Drop predictors that are linear dependent (LD)
#Generally, a correlation of 70% to 80% should be of concern.
#Drop ARMFORCEPERLABOR (Armed forces personnel % of total labor force) LD on TRDPERGDP (Trade (% of GDP)
#Drop FMBACH (Females with BS degrees) LD on multiple independent variables
#Drop MABACH (Males with BS degrees) LD on multiple independent variables
#Drop Country and Year as not relevant to this analaysis
df1 <-
  Base_df_meanCAMEO_invest[, c(-1, -2, -3, -5, -12, -17, -18)] 


##Run Model and Create Summary
#Run model
CAMEO.reg.step.frwd <-
  regsubsets(CAMEO_root_mean ~ ., df1, method = "forward")
#Create summary of regression output features
reg.summary <- summary(CAMEO.reg.step.frwd)

##Review Model Output
#Plot relative value of each predictor based on Adjusted R sqr and Cp
plot(CAMEO.reg.step.frwd, scale = "adjr2") #Larger Adj R sqr better
plot(CAMEO.reg.step.frwd, scale = "Cp") #Lower Cp value better
#Identify impact of number of predictors to performance measures
#  for Adjusted RSq
plot(reg.summary$adjr2,
     xlab = "Number of Variables",
     ylab = "Adjusted RSq",
     type = "l")
points(
  which.max(reg.summary$adjr2),
  reg.summary$adjr2[which.max(reg.summary$adjr2)],
  col = "red",
  cex = 2,
  pch = 20
)
#  for CP
plot(reg.summary$cp,
     xlab = "Number of Variables",
     ylab = "Cp",
     type = "l")
points(
  which.min(reg.summary$cp),
  reg.summary$cp[which.min(reg.summary$cp)],
  col = "red",
  cex = 2,
  pch = 20
)
#Model output
reg.summary # Predictors included in each model indicated with "*"
coef(CAMEO.reg.step.frwd, 8) #Display predictor coefficients
reg.summary$adjr2 #Highest adj r sq best
reg.summary$cp #Least cp value best
reg.summary$rsq #Highest r sq best
LR_frwrd_Rsq <- max(reg.summary$rsq)
LR_frwrd_Rsq

##Clean up this section
#Remove transitory data files and variables, and save the model output
#for later comparison
rm(df1, CAMEO.reg.step.frwd, reg.summary)



##############################################################################
#                   Linear Regression Backward Stepwise                      #
##############################################################################

##Drop predictors that are linear dependent (LD)
#Generally, a correlation of 70% to 80% should be of concern.
#Drop ARMFORCEPERLABOR (Armed forces personnel % of total labor force) LD on TRDPERGDP (Trade (% of GDP)
#Drop FMBACH (Females with BS degrees) LD on multiple independent variables
#Drop MABACH (Males with BS degrees) LD on multiple independent variables
#Drop Country and Year as not relevant to this analaysis
df1 <-
  Base_df_meanCAMEO_invest[, c(-1, -2, -3, -5, -12, -17, -18)]

##Run Model and Create Summary
#Run model
CAMEO.reg.step.bckwd <-
  regsubsets(CAMEO_root_mean ~ ., df1, method = "backward")
#Create summary of regression output features
reg.summary <- summary(CAMEO.reg.step.bckwd)

##Review Model Output
#Plot relative value of each predictor based on Adjusted R sqr and Cp
plot(CAMEO.reg.step.bckwd, scale = "adjr2") #Larger Adj R sqr better
plot(CAMEO.reg.step.bckwd, scale = "Cp") #Lower Cp value better
#Identify impact of number of predictors to performance measures
#  for Adjusted RSq
plot(reg.summary$adjr2,
     xlab = "Number of Variables",
     ylab = "Adjusted RSq",
     type = "l")
points(
  which.max(reg.summary$adjr2),
  reg.summary$adjr2[which.max(reg.summary$adjr2)],
  col = "red",
  cex = 2,
  pch = 20
)
#  for CP
plot(reg.summary$cp,
     xlab = "Number of Variables",
     ylab = "Cp",
     type = "l")
points(
  which.min(reg.summary$cp),
  reg.summary$cp[which.min(reg.summary$cp)],
  col = "red",
  cex = 2,
  pch = 20
)
#Model output
reg.summary # Predictors included in each model indicated with "*"
coef(CAMEO.reg.step.bckwd, 8) #Display predictor coefficients
reg.summary$adjr2 #Highest adj r sq best
reg.summary$cp #Least cp value best
reg.summary$rsq #Highest r sq best
LR_Bkwrd_Rsq <- max(reg.summary$rsq)
LR_Bkwrd_Rsq

##Clean up this section
#Remove transitory data files and variables, and save the model output
#for later comparison
rm(df1, CAMEO.reg.step.bckwd, reg.summary, LR_Bkwrd_Rsq)


##############################################################################
#              Linear Regression Best Subset Selection                       #
##############################################################################

##Drop predictors that are linear dependent (LD)
#Generally, a correlation of 70% to 80% should be of concern.
#Drop ARMFORCEPERLABOR (Armed forces personnel % of total labor force) LD on TRDPERGDP (Trade (% of GDP)
#Drop FMBACH (Females with BS degrees) LD on multiple independent variables
#Drop MABACH (Males with BS degrees) LD on multiple independent variables
#Drop Country and Year as not relevant to this analaysis
df1 <-
  Base_df_meanCAMEO_invest[, c(-1, -2, -3, -5, -12, -17, -18)] 

##Run Model and Create Summary
#Run model
CAMEO.reg.step.exhast <-
  regsubsets(CAMEO_root_mean ~ ., df1, method = "exhaustive")
#Create summary of regression output features
reg.summary <- summary(CAMEO.reg.step.exhast)


##Review Model Output
#Plot relative value of each predictor based on Adjusted R sqr and Cp
plot(CAMEO.reg.step.exhast, scale = "adjr2") #Larger Adj R sqr better
plot(CAMEO.reg.step.exhast, scale = "Cp") #Lower Cp value better
#Identify impact of number of predictors to performance measures
#  for Adjusted RSq
plot(reg.summary$adjr2,
     xlab = "Number of Variables",
     ylab = "Adjusted RSq",
     type = "l")
points(
  which.max(reg.summary$adjr2),
  reg.summary$adjr2[which.max(reg.summary$adjr2)],
  col = "red",
  cex = 2,
  pch = 20
)
#  for CP
plot(reg.summary$cp,
     xlab = "Number of Variables",
     ylab = "Cp",
     type = "l")
points(
  which.min(reg.summary$cp),
  reg.summary$cp[which.min(reg.summary$cp)],
  col = "red",
  cex = 2,
  pch = 20
)
#Model output
reg.summary # Predictors included in each model indicated with "*"
coef(CAMEO.reg.step.exhast, 8) #Display predictor coefficients
reg.summary$adjr2 #Highest adj r sq best
reg.summary$cp #Least cp value best
reg.summary$rsq #Highest r sq best
LR_BestSub_Rsq <- max(reg.summary$rsq)
LR_BestSub_Rsq

##Clean up this section
#Remove transitory data files and variables, and save the model output
#for later comparison
rm(df1, CAMEO.reg.step.exhast, reg.summary, LR_BestSub_Rsq)




