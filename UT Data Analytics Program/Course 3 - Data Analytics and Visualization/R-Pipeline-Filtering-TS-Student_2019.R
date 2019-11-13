# R-Pipeline-Filtering-TS-Student-2019

# Name:
# Last updated:


###############
# Project notes
###############

# Summarize project


###############
# Housekeeping
###############

# Clear all variables from R
rm(list = ls())

# Set working directory
getwd()
setwd()
dir()


################################
## Install and load packages
################################

install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("forecast")
install.packages("TTR")
install.packages("RMySQL")
install.packages("plotly")
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(tidyr)
library(lubridate) # work with dates
library(forecast)
library(TTR)
library(RMySQL)
library(plotly)


###############
# Load dataset 
###############

# Load using sql
## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
## List the tables contained in the database 
dbListTables(con)
## Lists attributes contained in a table
dbListFields(con,'iris')
## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
colnames(irisALL)
## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
colnames(irisSELECT)

## Energy data
dbListFields(con,'yr_2007')

## Exclude 2006 - mostly incomplete
## ** Include 2010 since it's mostly complete even though POA states to exclude 
##    all years that are incomplete
energy2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
energy2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
energy2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
energy2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

str(energy2007)
# 'data.frame':	521669 obs. of  10 variables:
# $ id                   : num  1 2 3 4 5 6 7 8 9 10 ...
# $ Date                 : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
# $ Time                 : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ Global_active_power  : num  2.58 2.55 2.55 2.55 2.55 ...
# $ Global_reactive_power: num  0.136 0.1 0.1 0.1 0.1 0.1 0.096 0 0 0 ...
# $ Global_intensity     : num  10.6 10.4 10.4 10.4 10.4 10.4 10.4 10.2 10.2 10.2 ...
# $ Voltage              : num  242 242 242 242 242 ...
# $ Sub_metering_1       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3       : num  0 0 0 0 0 0 0 0 0 0 ...

## Combine tables into one dataframe using dplyr
# ** Be sure to include 2010 
energy <- bind_rows(energy2007, energy2008, energy2009, energy2010)
# Confirm
head(energy)
tail(energy)
class(energy)
# Remove ID
energy$id <- NULL
str(energy)



##################
# Pre-process DS 
##################

#------Create a DateTime col by using unite() in tidyr-------------#

# as.Date() is an R method [R - Date Class - as.Date]
# If use as.Date, will lose any time stamp; (time less than a day)
# as.POSIXct will preserve time stamp; [R - Date-Time - POSIX classes]
# as.POSIXct stores both a date and time with an associated time zone. 
# Default is tz of your computer.
# Be sure to keep the date format the same (e.g.,"%d/%m/%Y %H:%M:%S")

# combine Date and Time using unite in tidyr
hhpwrDT <- energy %>% unite(DateTime, Date, Time, sep = " ", remove = FALSE)
# Could remove Date and Time by remove = TRUE and could add back using spread()
# convert DateTime to POSIXct
hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "Europe/Paris")

str(hhpwrDT)
class(hhpwrDT$DateTime) #[1] "POSIXct" "POSIXt" 
tz(hhpwrDT$DateTime) # "Europe/Paris"

# convert Date to as.Date
hhpwrDT$Date <- as.Date(hhpwrDT$Date, "%Y-%m-%d")
str(hhpwrDT)


##------- Change data types---------##

# Be sure to use as.numeric(as.character()), if needed, or may end up with index value I/O actual
# dataset$feature <- as.numeric(as.character(dataset$feature))


## ------ Evaluate NA values ----------##

# Are there any NAs in df?
any(is.na(hhpwrDT)) 
# Use summary to identify where NAs are 
summary(hhpwrDT)


## -------- Save pre-processed dataset --------##

# Save file (export to other programs), or
# Save object (for R only)


#####################
# Filtering pipeline
#####################

##----Process Steps from Sub-setting to Graphing------#
# 1. dplyr::mutate(): add column with desired time interval (e.g., Yr/Mo/Day) using lubridate 
# 2. dplyr::filter(): select cols to filter by; full ds + added col from mutate are avail to filter at this stage
# 3. dplyr::group_by(): select which time intervals to subset and their order 
# 4. dplyr::summarize(): select the vars and any calculations for these vars
# 5. dplyr::first(): add DateTime col to end summary() string using first() (not needed for forecasting)
# 6. dplyr::filter() to remove any NA or narrow data ranges 


#############
## Subsets
#############

########
# 1) Annual subset
########

# Example.

# Create a subset that shows the total annual consumption (kWh) for each submeter over the 
# Jan-07 thru Dec-09 period. 


#--- Create annual aggregate dataset ---#
# Total kWh per SM by year 

Yr.sum <- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2007 | Year==2008 | Year==2009) %>%
  group_by(Year) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance
Yr.sum

# A tibble: 4 x 5
# Year   SM1   SM2   SM3 DateTime           
# <dbl> <dbl> <dbl> <dbl> <dttm>             
# 1  2007  643.  854. 3023. 2007-01-01 00:00:00
# 2  2008  585.  662. 3178. 2008-01-01 00:00:00
# 3  2009  593.  592. 3557. 2009-01-01 00:00:00



#--- Plot (plot_ly) ---#

?plot_ly

# Plot sub-meter 1, 2 and 3 with title, legend and labels - Year frequency
plot_ly(Yr.sum, x = ~Yr.sum$Year, y = ~Yr.sum$SM1, name = 'SM1-Kitchen', type = 'bar') %>%
  add_trace(y = ~Yr.sum$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~Yr.sum$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Annual consumption (kWh) 2007~2009",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Power (kWh)"))  


#--- Create average annual dataset ---#

Yr.mean <- Yr.sum %>%
    summarize(SM1 = mean(SM1), # Total kWh per hour
            SM2 = mean(SM2), 
            SM3 = mean(SM3),
            DateTime = first(DateTime))   # To verify date of first instance
Yr.mean

# A tibble: 1 x 4
#      SM1   SM2   SM3 DateTime           
#     <dbl> <dbl> <dbl> <dttm>             
#   1  607.  703. 3253. 2007-01-01 01:00:00


#--- Plot ---#
plot_ly(Yr.mean, x = "", y = ~Yr.mean$SM1, name = 'SM1-Kitchen', type = 'bar') %>%
  add_trace(y = ~Yr.mean$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~Yr.mean$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Average Annual Consumption: 2007~2009",
         xaxis = list(title = "Submeter"),
         yaxis = list (title = "Power (kWh)"))  



#####
# 2) Weekday subset
#####

# Another example.

# Create a subset that shows the average daily consumption (kWh) for each submeter 
# by weekday for the winter months (Dec-Feb) over Jan-07 thru Oct-10 period. 
# This subset will have 7 values (one for each weekday - it reflects the 
# typical usage per weekday during the winter season). 

# Create aggregate daily subset

day.sum <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime)) %>%  
  # Don't add "1" after month (or label) - causes subset class=tibble when subsetting for seasons below 
  filter(Date >= "2007-01-01" & Date <= "2010-11-25") %>%
  group_by(Year, Month, Day) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per day
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) %>% # For filters in Tableau
  filter(!is.na(Day)) # Remove the last row that has NA 

head(day.sum)

# A tibble: 6 x 7
# Groups:   Year, Month [1]
#    Year Month   Day   SM1   SM2   SM3 DateTime           
# <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dttm>             
# 1  2007     1     1  0    0.352  5.88 2007-01-01 00:00:00
# 2  2007     1     2  0    0.348  6.56 2007-01-02 00:00:00
# 3  2007     1     3  0    0.344  4.76 2007-01-03 00:00:00
# 4  2007     1     4  1.05 7.60  10.9  2007-01-04 00:00:00
# 5  2007     1     5  1.48 0.379  7.60 2007-01-05 00:00:00
# 6  2007     1     6  1.34 0.402  5.68 2007-01-06 00:00:00


# Create weekday subset for winter 

# Subset for weekday in winter
wday.avg.winter <- day.sum %>%
  mutate(wDay = lubridate::wday(DateTime,1)) %>%  # Add col wDay by adding "1"
  filter(Month==12 | Month==1 | Month==2) %>%   # Filter after mutate, but before group_by
  group_by(wDay) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
wday.avg.winter
any(is.na(wday.avg.winter))  # FALSE

# A tibble: 7 x 5
#    wDay   SM1   SM2    SM3   DateTime
#   <ord> <dbl> <dbl>  <dbl>     <dttm>
# 1   Sun 3.151 3.248 10.214 2007-01-07
# 2   Mon 1.190 0.817 10.977 2007-01-01
# 3   Tue 1.095 2.026 11.063 2007-01-02
# 4   Wed 1.755 2.910 10.484 2007-01-03
# 5   Thu 1.218 0.889 10.768 2007-01-04
# 6   Fri 1.307 1.433 11.440 2007-01-05
# 7   Sat 3.225 2.962 12.580 2007-01-06


# Plot 

plot_ly(wday.avg.winter, x = ~wday.avg.winter$wDay, y = ~wday.avg.winter$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~wday.avg.winter$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~wday.avg.winter$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Winter Consumption (kWh) 2007~2010",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (kWh)"))  



##########
# 3) Hourly subset
##########

# Create a subset that shows the average hourly kWh used for each hour of the day during 
# January 2010. This subset should only have 24 values (it reflects the typical usage per 
# hour of day during Jan-10). Plot this subset using a line plot. 

# Your code




# Answer: Aggegrate by hour

# A tibble: 6 x 8
# Groups:   Year, Month, Day [1]
#    Year Month   Day  Hour   SM1   SM2   SM3            DateTime
#   <int> <int> <int> <int> <dbl> <dbl> <dbl>              <dttm>
# 1  2007     1     1     0     0 0.035     0 2007-01-01 00:00:00
# 2  2007     1     1     1     0 0.000     0 2007-01-01 01:00:00
# 3  2007     1     1     2     0 0.020     0 2007-01-01 02:00:00


# Answer: Average hourly, January 2010

# A tibble: 6 x 5
#    Hour   SM1   SM2   SM3           DateTime           
#   <int> <dbl> <dbl> <dbl>             <dttm>             
# 1     0 0.052 0.018 0.408 2010-01-01 00:00:00
# 2     1 0     0.015 0.304 2010-01-01 01:00:00
# 3     2 0     0.021 0.206 2010-01-01 02:00:00 


# Plot (line plot)




#############
# TSLM Forecast
#############

?tslm


######
# 4) TSLM annual forecast. Using the annual subset above (2007-09), forecast for
# submeter 3 for 2010 and 2011. 
######

# Your code



# Answers  

# ---- Fit using liner model ----# 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
#   (Intercept)  2718.93     138.15  19.681   0.0323 *
#   trend         266.99      63.95   4.175   0.1497 (Can't rej Ho of no-trend) 
#
# Residual standard error: 90.44 on 1 degrees of freedom
# Multiple R-squared:  0.9457,	Adjusted R-squared:  0.8915 
# F-statistic: 17.43 on 1 and 1 DF,  p-value: 0.1497

#--- Forecast using linear model

# Error measures:
#                        ME     RMSE      MAE         MPE     MAPE      MASE       ACF1
# Training set 1.515825e-13 52.21701 49.23067 -0.02111001 1.527482 0.1843928 -0.6666667
# 
# Forecasts:
#      Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2010       3786.881 3278.680 4295.082 1688.773 5884.989
# 2011       4053.869 3381.582 4726.156 1278.333 6829.405


# Plot


######
# 5) Create a subset that shows the total kWh per month for submeter 3 for the 
# months Jan-07 through Oct-10. Forecast for Nov-10 through Dec-11. Note: Be 
# sure to make the adjustment depending on if the ts is seasonal. Also, be sure 
# to record the summary metrics and know how to interpret the output; specifically, 
# R-squared, Adjusted R-squared, F-stat, and p-value. Also, understand how the 
# p-value relates to the null hypothesis regarding the statistics (i.e., slope 
# coefficients). For an additional resource for learning about regression output, 
# I suggest Brandon Foltz's tutorials on YouTube for statistics/regression. 
######


# Your code






# Answers:

# ---- Filter  ---- #

# Groups:   Year [1]
#    Year Month    SM1     SM2     SM3   DateTime
#   <dbl> <ord>  <dbl>   <dbl>   <dbl>     <dttm>
# 1  2007   Jan 56.433  79.274 329.578 2007-01-01
# 2  2007   Feb 47.584  64.604 270.274 2007-02-01
# 3  2007   Mar 60.769 104.733 290.361 2007-03-01
# 4  2007   Apr 42.078  38.417 189.503 2007-04-01


# ---- Fit using linear model ----#

# Call:
# tslm(formula = YrMo0710tsSM3 ~ trend + season)
#
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -73.206 -22.304  -0.428  19.314  95.510 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  305.4980    20.6848  14.769 4.22e-16 ***
# trend          1.9116     0.4308   4.437 9.59e-05 ***
# season2      -35.2116    26.8680  -1.311  0.19906    
# season3      -39.9132    26.8783  -1.485  0.14705    
# season4      -65.2747    26.8956  -2.427  0.02085 *  
# season5      -50.5291    26.9197  -1.877  0.06938 .  
# season6      -89.9117    26.9507  -3.336  0.00211 ** 
# season7     -162.5647    26.9886  -6.023 9.00e-07 ***
# season8     -190.8591    27.0333  -7.060 4.42e-08 ***
# season9      -90.8444    27.0847  -3.354  0.00201 ** 
# season10     -65.2370    27.1429  -2.403  0.02202 *  
# season11     -44.1863    29.0681  -1.520  0.13801    
# season12      -0.1499    29.0968  -0.005  0.99592    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 37.99 on 33 degrees of freedom
# Multiple R-squared:  0.7666,	Adjusted R-squared:  0.6817 
# F-statistic:  9.03 on 12 and 33 DF,  p-value: 2.522e-07


# ---- Forecast using linear model ----@

# Call:
# tslm(formula = ds_YrMoSM3ts ~ trend + season)
#
# Coefficients:
#  (Intercept)       trend      season2      season3      season4      season5      season6  
#    305.4980       1.9116     -35.2116     -39.9132     -65.2747     -50.5291     -89.9117  
#     season7      season8      season9     season10     season11     season12  
#   -162.5647    -190.8591     -90.8444     -65.2370     -44.1863      -0.1499  
#
# Error measures:
#                        ME     RMSE     MAE       MPE     MAPE      MASE      ACF1
# Training set -2.161824e-15 32.17895 25.4186 -2.412659 11.10408 0.5400616 0.1368401
#
# Forecasts:
#          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# Nov 2010       351.1559 292.2140 410.0978 259.4569 442.8550
# Dec 2010       397.1039 338.1620 456.0458 305.4049 488.8030
# Jan 2011       399.1654 341.1026 457.2283 308.8340 489.4969
# Feb 2011       365.8654 307.8026 423.9283 275.5340 456.1969
# Mar 2011       363.0754 305.0126 421.1383 272.7440 453.4069
# Apr 2011       339.6254 281.5626 397.6883 249.2940 429.9569
# May 2011       356.2827 298.2198 414.3455 265.9513 446.6141
# Jun 2011       318.8117 260.7488 376.8745 228.4803 409.1431
# Jul 2011       248.0702 190.0073 306.1330 157.7388 338.4016
# Aug 2011       221.6874 163.6246 279.7503 131.3560 312.0189
# Sep 2011       323.6137 265.5508 381.6765 233.2823 413.9451
# Oct 2011       351.1327 293.0698 409.1955 260.8013 441.4641
# Nov 2011       374.0949 313.2450 434.9448 279.4275 468.7623
# Dec 2011       420.0429 359.1930 480.8928 325.3755 514.7103



###########
# Decompose
###########

######
# 6) Using the ts for SM3 that shows kWh by month over the Jan-07 thru Oct-10 
# time period (monthly subset above), decompose this ts into seasonal, trend, 
# and random components. Also provide a plot for these components.
######

# Your code for decomposing - show summary statistics for seasonal, trend, random






######
# 7) Create a subset that shows kWh by hour over each day during Feb-10. Create 
# a ts object for SM3 and decompose this ts into seasonal, trend, and random 
# components. Also provide a plot for these components. Things to consider: 1) the 
# number of seasonal periods in this month, and 2) the frequency value needed for 
# each of these seasonal periods (you may need to research how to set the frequency 
# argument for seasonal periods less than a year). 
######

# Your code to subset





# A tibble: 6 x 6
# Groups:   Day [1]
# Day  Hour   SM1   SM2   SM3            DateTime
# <int> <int> <dbl> <dbl> <dbl>              <dttm>
# 1     1     0     0 0.000 0.041 2010-02-01 00:00:00
# 2     1     1     0 0.000 0.041 2010-02-01 01:00:00
# 3     1     2     0 0.047 0.306 2010-02-01 02:00:00
# 4     1     3     0 0.049 0.431 2010-02-01 03:00:00
# 5     1     4     0 0.000 0.041 2010-02-01 04:00:00
# 6     1     5     0 0.000 0.041 2010-02-01 05:00:00


# Filter from the hourly subset (3) created above.


# Your code for decomposing - show summary statistics for seasonal, trend, random





###################
# Holt-Winters (HW)
###################

######
# 8) Need non-seasonal ts for HW. Therefore, create a ts object for SM3 for the 
# Winter months Dec-09 thru Feb-10 season. To do this, create a subset that shows 
# kWh by day over the season, then forecast the next 30 days. Plot the fit and 
# forecast objects for the season. (The plot will include the data leading up to 
# the forecast, and the forecast itself. In the POA, it shows how to plot the 
# 'forecast only', which you could do as well. The plot will show the actual data 
# and the forecasted (in red) in the same chart. Note: to create the HW forecast 
# object, you may need to use forecast() I/O forecast.HoltWinters(). You may want 
# to consider using decompose to remove any seasonality that may be present in these 
# ts objects. Be sure to evaluate the residuals using the Ljung-Box test, etc. 
# Refer to The Little Book of R.                                                                                                                                                                                                                                                                                                
######                                                                                                                                                                                                                                                                                                                 


# Your code for the subset





# Groups:   Year, Month [1]
#    Year Month   Day   SM1   SM2    SM3   DateTime
#   <dbl> <dbl> <int> <dbl> <dbl>  <dbl>     <dttm>
# 1  2009    12     1 0.000 0.354 10.821 2009-12-01
# 2  2009    12     2 1.143 7.052 12.851 2009-12-02
# 3  2009    12     3 1.039 0.402  9.906 2009-12-03
# 4  2009    12     4 1.158 0.352  9.134 2009-12-04


# Your code for ts





# Your code for HW





