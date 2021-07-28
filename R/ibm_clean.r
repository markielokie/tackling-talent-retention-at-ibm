### This script is to clean the IBM attrition data

## Set working directory to source file location

## Load libraries
library(dplyr)
library(correlationfunnel)
library(plotly)
library(caret)
library(corrplot)
library(car)
library(ROCR)
library(rpart)
library(cluster) 
library(DT)
library(forcats)
library(skimr) 
library(flexclust)

## Load data
dat_hr <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors=T)

## Explore data
skim(dat_hr)

## Clean data
# Renaming the Age column
dat_hr <- dat_hr %>%
  dplyr::rename(Age = ï..Age)

# Converting some numeric columns to factor
dat_hr$Education <- as.factor(dat_hr$Education)
dat_hr$EnvironmentSatisfaction <- as.factor(dat_hr$EnvironmentSatisfaction)
dat_hr$JobInvolvement <- as.factor(dat_hr$JobInvolvement)
dat_hr$JobLevel <- as.factor(dat_hr$JobLevel)
dat_hr$JobSatisfaction <- as.factor(dat_hr$JobSatisfaction)
dat_hr$StockOptionLevel <- as.factor(dat_hr$StockOptionLevel)
dat_hr$PerformanceRating <- as.factor(dat_hr$PerformanceRating)
dat_hr$RelationshipSatisfaction <- as.factor(dat_hr$RelationshipSatisfaction)
dat_hr$WorkLifeBalance <- as.factor(dat_hr$WorkLifeBalance)

# Remove columns with only 1 unique value
dat_hr <- dat_hr %>%
  dplyr::select(-Over18, -EmployeeCount, -StandardHours)

# Check for NAs in the data
sum(is.na(dat_hr))

# Glimpse the cleaned data
glimpse(dat_hr)

##########################################
### Proceed with part 2 of the project ###
########################################## 

# Convert JobRole from factor to character
dat_hr$JobRole <- as.character(dat_hr$JobRole)

# Rename all roles into standard role types
dat_hr_newroles <- dat_hr %>%
  mutate(JobRole = case_when(
    str_detect(JobRole, "Sales Executive") ~ "Sales",
    str_detect(JobRole, "Research Scientist") ~ "ESP",
    str_detect(JobRole, "Laboratory Technician") ~ "AESP",
    str_detect(JobRole, "Manufacturing Director") ~ "Director",
    str_detect(JobRole, "Healthcare Representative") ~ "Corporate",
    str_detect(JobRole, "Sales Representative") ~ "Sales",
    str_detect(JobRole, "Research Director") ~ "Director",
    str_detect(JobRole, "Human Resources") ~ "Corporate",
    TRUE ~ JobRole
  ))

# Convert JobRole to factor
dat_hr_newroles$JobRole <- as.factor(dat_hr_newroles$JobRole)

# Rename JobRole to Role in order to match dat_gd_avgsentiment_pros and cons
dat_hr_renamed <- dat_hr_newroles %>%
  rename(Role = JobRole)

## Join sentiment scores to dat_hr_renamed

# dat_gd_avgsentiment_pros joined to dat_hr_renamed
dat_hr_final <- dat_hr_renamed %>%
  left_join(dat_gd_avgsentiment_pros, by="Role") %>%
  select(-word_count, -sd) %>%
  rename(ave_sentiment_pros = ave_sentiment) %>%
  left_join(dat_gd_avgsentiment_cons, by="Role") %>%
  select(-word_count, -sd) %>%
  rename(ave_sentiment_cons = ave_sentiment)

glimpse(dat_hr_final)
