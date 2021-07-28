### This script is to clean the Glassdoor scraped text data

## Set working directory to source file location

## Load libraries
library(readxl)
library(dplyr)
library(tm)
library(tidyr)

## Load data
dat_gd <- read_excel("IBMReviewsGlassdoor_USA.xlsx")

## Explore data
glimpse(dat_gd)

## Clean data
# Convert Ratings column to numeric
dat_gd$Ratings <- as.numeric(dat_gd$Ratings)

# Clean Current_Former column by splitting into 2 columns and converting to factor
dat_gd <- dat_gd %>%
  separate(Current_Former, c("Current_Former", "Employment_Length"), ", ")

dat_gd$Current_Former <- as.factor(dat_gd$Current_Former)

# Clean Review_ID column by removing links and extracting unique IDs
dat_gd$Review_ID <- sub("https://www.glassdoor.sg/Reviews/Employee-Review-IBM-RVW", "", dat_gd$Review_ID)
dat_gd$Review_ID <- sub(".htm", "", dat_gd$Review_ID)

dat_gd$Review_ID <- as.numeric(dat_gd$Review_ID)

# Check for duplicates in Review_ID column
dim(dat_gd[duplicated(dat_gd$Review_ID),])[1] # There is one duplicate review ID
dat_gd[duplicated(dat_gd$Review_ID),] # ID 42772946 is duplicated
dat_gd <- dat_gd[!duplicated(dat_gd$Review_ID),] # Duplicate is removed

# Clean Role column and split into 2 columns
# Convert these 5 rows to the correct form before splitting the strings
dat_gd[181,"Role"] <- "23 Apr 2021 - Digital Sales"
dat_gd[427,"Role"] <- "31 Mar 2021 - Support Services Representative"
dat_gd[628,"Role"] <- "15 Apr 2021 - Senior Project Manager"
dat_gd[680,"Role"] <- "15 Feb 2021 - Manager"
dat_gd[829,"Role"] <- "3 Feb 2021 - Support Services Representative"

# Split the string into Review_Date and Role
dat_gd <- dat_gd %>%
  separate(Role, c("Review_Date", "Role"), " - ") 

# Change Review_Date column to date format
dat_gd$Review_Date <- as.Date(dat_gd$Review_Date, "%d %b %Y")

# Clean Location column by extracting state only
dat_gd$Location <- sub(".*,\\s*", "", dat_gd$Location)

# Check for rows that are not in the 2-state abbreviation format
dat_gd$Location[nchar(dat_gd$Location) != 2] # There are 2 rows that are not in the correct format and likely not in the US
dat_gd <- dat_gd[nchar(dat_gd$Location) == 2,] # Remove those 2 rows