#### Preamble ####
# Purpose: Clean the survey data downloaded from Fund + UCLA Nationscape
# Author:  HAILAN HUANG
# Data: 27 April 2022
# Contact: HAILAN.HUANG@utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the Fund + UCLA Nationscape data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
library(labelled)
#Read data
raw_data <-  read_dta("inputs/data/ns20200625.dta") %>% 
  to_factor() %>% 
  select(age,
         gender, 
         race_ethnicity,
         news_sources_cnn,
         household_income,
         vote_2016,
         vote_2020
  )

#clean data for 2016 vote
#income into low and high level
#age into 4 groups
#race into while and black 
#cnn dummy code 1 or 0
#vote outcome Trump 1 or 0

raw_data_2016 <- raw_data %>%
  filter(vote_2016 %in% c("Hillary Clinton", "Donald Trump")) %>% 
  mutate(Trump = as.integer(vote_2016 == "Donald Trump"),
      income = factor(case_when(
        household_income %in% 
          c('Less than $14,999','$15,000 to $19,999','$20,000 to $24,999','$25,000 to $29,999','$30,000 to $34,999','$35,000 to $39,999')   ~ "Low",
        ! household_income %in% 
          c('Less than $14,999','$15,000 to $19,999','$20,000 to $24,999','$25,000 to $29,999','$30,000 to $34,999','$35,000 to $39,999')    ~ "High",
      )),
    race = factor(case_when(
      race_ethnicity == "White" ~ "white",
      race_ethnicity == "Black, or African American" ~ "black",
      ! race_ethnicity %in% c("White" ,"Black, or African American" ) ~ "other"
    )),
    age = factor(case_when(
      age <= 34 ~ "18-34",
      age <= 50 ~ "35-50",
      age <= 65 ~ "51-65",
      age >  65 ~ "65+"
    )),
    cnn = as.integer(   news_sources_cnn == "Yes")
    ) %>%
  select(
    age, 
    gender, 
    race,
    income,
    cnn,
    Trump
  )%>%
  drop_na()

#clean data for 2020 vote (survey)
#income into low and high level
#age into 4 groups
#race into while and black 
#cnn dummy code 1 or 0
#vote outcome Trump 1 or 0

raw_data_2020 <- raw_data %>%
  filter(vote_2020 %in% c("Joe Biden", "Donald Trump")) %>% 
  mutate(Trump = as.integer(vote_2020 == "Donald Trump"),
         income = factor(case_when(
           household_income %in% 
             c('Less than $14,999','$15,000 to $19,999','$20,000 to $24,999','$25,000 to $29,999','$30,000 to $34,999','$35,000 to $39,999')   ~ "Low",
           ! household_income %in% 
             c('Less than $14,999','$15,000 to $19,999','$20,000 to $24,999','$25,000 to $29,999','$30,000 to $34,999','$35,000 to $39,999')    ~ "High",
         )),
         race = factor(case_when(
           race_ethnicity == "White" ~ "white",
           race_ethnicity == "Black, or African American" ~ "black",
           ! race_ethnicity %in% c("White" ,"Black, or African American" ) ~ "other"
         )),
         age = factor(case_when(
           age <= 34 ~ "18-34",
           age <= 50 ~ "35-50",
           age <= 65 ~ "51-65",
           age >  65 ~ "65+"
         )),
         cnn = as.integer(   news_sources_cnn == "Yes")
  ) %>%
  select(
    age, 
    gender, 
    race,
    income,
    cnn,
    Trump
  )%>%
  drop_na()
#save to outputs
write_rds(raw_data_2016, "outputs/paper/data/vote2016.rds")
write_rds(raw_data_2020, "outputs/paper/data/vote2020.rds")



         