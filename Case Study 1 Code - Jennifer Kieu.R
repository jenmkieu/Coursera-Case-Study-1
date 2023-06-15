# Coursera Google Data Analytics Case Study 1 - Capstone Project
# Author: Jennifer Kieu
# 2023

# Scenario / Introduction:

# You are a junior data analyst working in the marketing analyst team at Cyclistic, 
# a bike-share company in Chicago. The director of marketing believes the company’s future 
# success depends on maximizing the number of annual memberships. Therefore,
# your team wants to understand how casual riders and annual members use 
# Cyclistic bikes differently. From these insights, your team will design a new marketing 
# strategy to convert casual riders into annual members. But first, Cyclistic executives
# must approve your recommendations, so they must be backed up with compelling data insights 
# and professional data visualizations.


# Prepare:
# I cleaned my data in Excel prior to merging and manipulating the data using R

# 1) I first checked the integrity of the data.
#    The first party data is provided by Motivate International Inc. in zip files hosted on 
#    Amazon Web Services (AWS). The website link is provided through Coursera.

# 2) I created a master folder named 'Cyclistic Dataset' along with two sub-folders
#    named '2023 Bike Data Excel Files' and '2023 Bike Data CSV Files'.

# 3) I unzipped all the files after downloading them and saved the data in 
#    Excel Workbook format within the folder named '2023 Bike Data Excel Files'.

# 4) The Excel files were then converted to CSV format and saved under the 
#    '2023 Bike Data CSV Files' folder.

# Cleaning:
# (Note) For the purpose of this case study, I will be using the data set for the year 2023.

# 1) I renamed the excel files with the 2023 data set to: 
#    'bike_data_01_2023', 'bike_data_02_2023', and 'bike_data_03_2023'

# 2) After opening the relevant excel files, I centered all the columns and 
#    froze the top header row to make it more convenient to see what columns 
#    I am working on when scrolling up and down.

# 3) I removed duplicates for each Excel file, if any were found.

# 4) I formatted columns C (started_at) and D (ended_at) into the HH:MM:SS format.

# 5) I renamed column M to member_type.

# 5) To make the spreadsheets more readable, I hid columns A(ride_id), E(start_station_name),
#    F (start_station_id), G (end_station_name), H (end_station_id), I (start_lat), 
#    J (start_lng), K (end_lat), and L (end_lng). 

# 6) I created the ride_length column (N), which subtracts the started_at column 
#    from the ended_at column to find the total time elapsed for each individual bike trip.

# 7) I created the day_of_week column (O), which finds the day of the week that the ride
#    was taken for each ride ID. 1 represents Sunday, while 7 represents Saturday.


# Manipulating the Data using R:

# Install and load the 'tidyverse', 'dplyr', 'readr', and 'ggplot2' packages

install.packages("tidyverse")
install.packages ("dplyr")
install.packages("readr")
library(tidyverse)
library (dplyr)
library (readr)
library (ggplot2)

# Set the working directory to the location of the data set and read the csv files

setwd('~/Coursera Data Analytics Case Study 1/Cyclistic Dataset/2023 Bike Data CSV Files/')
trip_data_jan <- read_csv("bike_data_01_2023.csv")
trip_data_feb <- read_csv("bike_data_02_2023.csv")
trip_data_mar <- read_csv("bike_data_03_2023.csv")

# Checked to make sure that there are the same amounts of columns 
# as well as the same variable names for each of the files

colnames(trip_data_jan)
colnames(trip_data_feb)
colnames(trip_data_mar)

# I then combined/aggregated all the csv files into one data frame
# (Excel is unable to do this, due to its capacity limitations)

trip_data_2023 <- bind_rows(trip_data_jan, trip_data_feb, trip_data_mar)

# Viewed the new data frame containing all the customer bike data for 2023

View(trip_data_2023)
glimpse (trip_data_2023)
colnames (trip_data_2023)

# Analysis Step

# I began the analysis by looking at the most popular days for all riders using a column graph.
# Keeping in mind that 1 = Sunday, while 7 = Saturday

# The most popular day to ride is Tuesday (113,112 rides) for both member and casual riders.
# Starting from Sunday, the amount of riders increases and peaks on Tuesday,
# before gradually declining as we get closer to Saturday.

trip_data_2023 %>%
  group_by(day_of_week) %>%
  summarise(ride_count = n()) %>%
  ggplot(mapping = aes (x = day_of_week, y = ride_count)) + geom_col() +
  geom_text(aes(label = after_stat(y), group = day_of_week), stat = 'summary', fun = sum, vjust = -0.5) +
  labs (x = "Day of the Week", y = "Count of Rides") +
  labs(title = "Ride Count vs. Day of the Week", subtitle = "Sample of Casual and Member Riders (2023)", caption = "Data collected from Coursera") +
  theme (plot.title = element_text(hjust = 0.5)) +
  theme (plot.subtitle = element_text(hjust = 0.5))

# I also used the mode() function to find the most popular day for casual vs. member riders.
# (Note) I used na.rm = TRUE to ignore NA / missing values prior to the calculation.

# The most popular day to ride for casual riders is Sunday, while the most popular day 
# for members is Tuesday.

mode <- function(x, na.rm = TRUE) 
  {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

trip_data_2023 %>%
  group_by(member_type) %>%
  summarise (most_popular_day = mode(day_of_week))


# I then looked for the maximum ride length for both casual and member riders.

# The longest ride length for casual riders is 86,287 secs or 23.97 hours, while the longest ride
# length for member riders is 85,427 secs or 23.73 hours.

trip_data_2023 %>%
  group_by (member_type) %>%
  summarise (max_ride_length = max(ride_length, na.rm = TRUE))


# I then looked for the average of the ride lengths for casual riders vs. member riders

# The average ride length for casual riders is 943.1976 secs or 15.72 minutes,
# whereas the average ride length for member riders is 614.5514 seconds or 10.24 minutes

trip_data_2023 %>%
  group_by (member_type) %>%
  summarise (avg_ride_length = mean(ride_length, na.rm = TRUE))

# I then found the most popular starting and ending station for casual riders vs. member riders.

# The most popular starting and ending station for casual riders: 'Yates Blvd & 93rd St' for both.
# The most popular starting and ending station for member riders: 'Yates Blvd & 75th St' 
# and 'Yates Blvd & 93rd St.

trip_data_2023 %>%
  group_by (member_type) %>%
  summarise (most_popular_starting_station = max (start_station_name, na.rm = TRUE))

trip_data_2023 %>%
  group_by (member_type) %>%
  summarise (most_popular_ending_station = max (end_station_name, na.rm = TRUE))

# I calculated the total ride length (hours) vs. the day of the week for casual and member riders.

# Casual riders utilize the Cyclistic services the most on the weekend as compared to member riders,
# who prefer to use Cyclistic services during weekdays.
# I utilized a stacked bar chart to showcase the differences.

trip_data_2023 %>%
  group_by (day_of_week) %>%
  ggplot (mapping =  aes(x = day_of_week, y = ride_length, color = member_type)) + geom_col() +
  geom_text(aes(label = after_stat(y), group = day_of_week), stat = 'summary', fun = sum, vjust = 1) +
  theme(legend.position = "bottom") +
  labs (x = "Day of the Week", y = "Ride Length (Hours)") +
  labs(title = "How Cyclistic Riders Differ: Ride Length Vs. Day of the Week", subtitle = "Sample of Casual and Member Riders (2023)", caption = "Data collected from Coursera") +
  theme (plot.title = element_text(hjust = 0.5)) +
  theme (plot.subtitle = element_text(hjust = 0.5))


# Finally, I viewed the differences in bike types that casual and member riders choose using
# a column graph.

options(scipen = 999)

trip_data_2023 %>%
  group_by(member_type) %>%
  ggplot (mapping = aes (x = member_type, y = lengths(rideable_type), color = member_type)) + geom_col()+
  geom_text(aes(label = after_stat(y), group = rideable_type), stat = 'summary', fun = sum, vjust = 1) +
  facet_wrap(~rideable_type) +
  labs (x = "", y = "") +
  labs (title = "How Cyclistic Riders Differ: Choice of Bike Type", subtitle = "Sample of Casual and Member Riders (2023)", caption = "Data collected from Coursera") +
  theme (plot.title = element_text(hjust = 0.5)) +
  theme (plot.subtitle = element_text(hjust = 0.5))

# Conclusion and Recommendations

# ***The goal is to provide incentives for casual riders to obtain a Cyclistic rider membership.
  
# 1) Most casual riders use the bike share service during the weekend (Sunday and Saturday) for
#    possibly recreational purposes rather than commuting, therefore, it is suggested that Cyclistic 
#    offer discounted rates for rides that occur during the weekend for customers who choose to 
#    get a membership.

# 2) The average ride length for a casual rider is typically longer (15.72 minutes) as compared to a
#    member rider (10.24 minutes). With this in mind, it is suggested that the company provides 
#    a discount offer on longer rides for customers with a membership.
   
# 3) Both casual and member riders pick up or drop off their bikes mainly at Yates Blvd 
#    & 93rd St. / 75th St. A large hospital is located right at the intersection of these two streets,
#    therefore, it is suggested that Cyclistic collaborate with this facility to provide digital or 
#    sign advertisement of exclusive membership discounts for rides heading towards the hospital.
#    The hospital can also gain more foot traffic through this partnership with Cyclistic.

# 4) Due to electric bikes being the most popular among casual riders, Cyclistic could choose to
#    provide a larger selection of electric bikes at the most popular station location, as well as
#    a discount on using them.


#    Thank you for your time!



