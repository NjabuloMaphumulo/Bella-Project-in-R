## Install and Load Packages
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("Rcpp")
library(Rcpp)
-----------------------------------------------------------------------------

## importing data
activity <- read.csv("/cloud/project/Bella/dailyActivity_merged.csv")
intensities <- read.csv("/cloud/project/Bella/hourlyIntensities_merged.csv")
calories <- read.csv("/cloud/project/Bella/hourlyCalories_merged.csv")
sleep <- read.csv("/cloud/project/Bella/sleepDay_merged.csv")
weight <- read.csv("/cloud/project/Bella/weightLogInfo_merged.csv")

-----------------------------------------------------------------------------

## Preview Data
head(activity)
head(intensities)
head(calories)
head(sleep)
head(weight)
  # Checked and verified data type, accurate and consistent, except for Date/time.
  # Column headers consistent. 
  # noticed N/A values in the "weight" data
---------------------------------------------------------------------------------

## Data Cleaning
  # Sorting Date/Time formating
  #activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())     
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
  
  #Intensities
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())      
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")

  #calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")

  #sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")

  #weight
weight$Date=as.POSIXct(weight$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight$Date <- format(weight$Date, format = "%m/%d/%y")

  # Check and Remove Duplicates
sum(duplicated(activity))
sum(duplicated(intensities))
sum(duplicated(calories))
sum(duplicated(sleep))            # 3 duplicated entries
sum(duplicated(weight))

sleep <- sleep[!duplicated(sleep)]   # found duplicates removed
sum(duplicated(sleep))      # checking if duplicates removed

  # Check and Remove N/A values
sum(is.na(activity))
sum(is.na(intensities))
sum(is.na(calories))
sum(is.na(sleep))
sum(is.na(weight))       # 65 n/a values found

weight <- weight[!is.na(weight)]    # found n/a values removed
sum(is.na(weight))     #checking if n/a removed


  # data merging
activity_intensities <- merge(activity, intensities, by = c("Id", "date"))
head(activity_intensities)

calories_sleep <- merge(calories, sleep, by = c("Id", "date"))
head(calories_sleep)

first_merger <- merge(activity_intensities, calories_sleep, by = c("Id", "date"))
head(first_merger)

---------------------------------------------------------------------------------
  
## Data Analysis
  #Statistical summary 
    #activity
activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()


    #activity duration 
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

    #calories burnt during activity
calories %>%
  select(Calories) %>%
  summary()
    
    #sleep duration
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
    #Average sleep time is 7hours (419.2 minutes), most subjects sleep once a day.

## Categorise users according to their total daily steps & sedentary (ranged according to the CDC)
user_cat <- first_merger %>%
  mutate(user_cat = case_when(
    TotalSteps < 7406 ~ "Inactive",
    TotalSteps >= 7406 & TotalSteps < 9999 ~ "Somewhat Active", 
    TotalSteps >= 9999 & TotalSteps <= 12500 ~ "Active",
    TotalSteps > 12500 ~ "Highly Active")) %>%
  mutate(sedentary_cat = case_when(
    SedentaryMinutes < 700  ~ "Good Sedentary", 
    SedentaryMinutes >= 700 ~ "Excessive Sedentary"
  ))

## Create percentage values
user_cat_perc <- user_cat %>%
  group_by(user_cat) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_cat) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

sedentary_cat_perc <- user_cat %>%
  group_by(sedentary_cat) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(sedentary_cat) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

##Check percentage values
head(user_cat_perc)
head(sedentary_cat_perc)

--------------------------------------------------------------------------------------

##Viz

install.packages("flexdashboard")
library(flexdashboard)
install.packages("htmltools")
library(htmltools)

  #Chart - Sedentary cat  
  sedentary_cat_perc %>%
    ggplot(aes(x="",y=total_percent, fill=sedentary_cat)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
    scale_fill_manual(values = c("#669933", "#99CC66")) +
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5))+
    labs(title ="User Sedentary Level",
         caption = 'Data Source: FitBit Fitness Tracker Data')

  #Chart - user's activity category based on daily steps
  user_cat_perc %>%
    ggplot(aes(x="",y=total_percent, fill=user_cat)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
    scale_fill_manual(values = c("#99CC66", "#669933", "#FFFFCC", "#CCFF99")) +
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5))+
    labs(title ="User Activity Level",
         caption = 'Data Source: FitBit Fitness Tracker Data')
  
  
    #Chart -
  head(first_merger)
 
  first_merger %>% 
    group_by(TotalMinutesAsleep, SedentaryMinutes) %>% 
    ggplot(aes(x = TotalMinutesAsleep, y = SedentaryMinutes, color = SedentaryMinutes)) +
    geom_point() +
    geom_smooth(color = "#669933") + 
    #theme(legend.position = c(.8, .3),
    #      legend.spacing.y = unit(2, "mm"), 
    #      panel.border = element_rect(colour = "black", fill=NA),
    #      legend.background = element_blank(),
    #      legend.box.background = element_rect(colour = "black")) +
    labs(title = 'Sedentarys impact on Sleep', 
         style = list(fontsize = '25px', fontweight = 'bold',
         y = 'Sedentary Duration',
         x = 'Sleep Duration',
         caption = 'Data Source: FitBit Fitness Tracker Data')
  
         ggplot(data=first_merger, aes(x = TotalMinutesAsleep, y = SedentaryMinutes, color = '#CCFF99')) + 
           geom_point(color='#669933') + geom_smooth() +
           labs(title="Sedentary impact on sleep")
         
  #Times of Activity
  head(intensities)
  Times_of_Activity <- intensities %>%
    group_by(time) %>%
    #drop_na() %>%
    summarise(Average_Activity = mean(TotalIntensity))
  
  ggplot(data=Times_of_Activity, aes(x=time, y=Average_Activity)) + geom_histogram(stat = "identity", fill = 'green4') +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title="User active Times",
         x = 'Time of the Day',
         y = 'Average Activity',
         caption = 'Data Source: FitBit Fitness Tracker Data')

  sedentary_cat_perc %>%
    ggplot(aes(x="",y=total_percent, fill=sedentary_cat)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
    scale_fill_manual(values = c("#669933", "#99CC66")) +
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5))+
    labs(title ="User Sedentary Level",
         caption = 'Data Source: FitBit Fitness Tracker Data')
  
  
  
  #Chart – Sedentary’s impact on sleep
  head(first_merger)
  first_merger %>% 
    group_by(TotalMinutesAsleep, SedentaryMinutes) %>% 
    ggplot(aes(x = TotalMinutesAsleep, y = SedentaryMinutes, color = SedentaryMinutes)) +
    geom_point() +
    geom_smooth(color = "#669933") + 
    theme(legend.position = c(.9, .8),
          legend.spacing.y = unit(.001, "mm"), 
          panel.border = element_rect(colour = "#99cc66", fill=NA),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "#ccff99")) +
    labs(title = "Sedentarys impact on sleep",
         y = 'Sedentary Duration',
         x = 'Sleep Duration',
         caption = 'Data Source: FitBit Fitness Tracker Data')
  
  