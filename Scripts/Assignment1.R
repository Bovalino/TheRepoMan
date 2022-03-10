#Load packages to be able to use their functions
library(tidyverse)
library(here)

#Load sample data files
force_data <- read_csv(here("","Data/Dataset1_Assessment1.csv"))
load_data <- read_csv(here("","Data/Dataset2_Assessment1.csv"))


#Force data
#Review raw data as a tibble to understand data types and see if rearranging is required
force_data

#Create summary statistics at an individual level by limb and measure (this data is taken at the same knee angle but that might change so good to include it now)
force_summary_indiv <- force_data %>% 
  group_by(Name, Limb, Measure) %>% 
  summarise (Ave = round(mean(Force),1)
             , Median = round(median(Force),1)
             , Max = round(max(Force),1)
             , Min = round(min(Force),1)
             , Std.Dev = round(sd(Force),1)
             )

#View the data frame for individuals
view(force_summary_indiv)

#Create summary statistics at the team level by limb and measure
force_summary_team <- force_data %>% 
  group_by(Limb, Measure) %>% 
  summarise (Ave = round(mean(Force),1)
             , Median = round(median(Force),1)
             , Max = round(max(Force),1)
             , Min = round(min(Force),1)
             , Std.Dev = round(sd(Force),1)
             )

#View the data frame for the team
view(force_summary_team)


#Load data
#Review raw data as a tibble to understand data and see if it needs cleaning or rearranging
load_data

#Create summary statistics at an indvidual level by session type
load_summary_indiv <- load_data %>% 
  group_by(Athlete, SessionType) %>% 
  summarise (across(TotalDistance:SprintDistance, mean, .names = "Mean_{col}")
             , across(TotalDistance:SprintDistance, median, .names = "Median_{col}")
             , across(TotalDistance:SprintDistance, max, .names = "Max_{col}")
             , across(TotalDistance:SprintDistance, min, .names = "Min_{col}")
             , across(TotalDistance:SprintDistance, sd , .names = "SD_{col}")
              ) %>% 
  mutate(across(where(is.numeric), round, 1))

#View the data frame for individuals
view(load_summary_indiv)

#Create summary statistics at the team level by session type
load_summary_team <- load_data %>% 
  group_by(SessionType) %>% 
  summarise (across(TotalDistance:SprintDistance, mean, .names = "Mean_{col}")
             , across(TotalDistance:SprintDistance, median, .names = "Median_{col}")
             , across(TotalDistance:SprintDistance, max, .names = "Max_{col}")
             , across(TotalDistance:SprintDistance, min, .names = "Min_{col}")
             , across(TotalDistance:SprintDistance, sd, .names = "SD_{col}")
              ) %>% 
  mutate(across(where(is.numeric), round, 1))
            
#View the data frame for the team
view(load_summary_team)



