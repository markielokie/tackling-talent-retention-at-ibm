### This script is to perform the sentiment analysis on the Glassdoor reviews

## Set working directory to source file location
## Ensure that glassdoor_clean.r has been run first before running this script

## Load libraries
library(readxl)
library(dplyr)
library(tm)
library(tidyr)
library(stringr)
library(sentimentr)
library(ggplot2)

# Convert all Roles to lowercase
dat_gd$Role <- tolower(dat_gd$Role)

# Rename all roles into standard role types
dat_gd_newroles <- dat_gd %>%
  mutate(Role  = case_when(
    str_detect(Role, "manager") ~ "Manager",
    str_detect(Role, "bdm") ~ "Manager",
    str_detect(Role, "director") ~ "Director",
    str_detect(Role, "engineer") ~ "ESP",
    str_detect(Role, "developer") ~ "ESP",
    str_detect(Role, "analyst") ~ "ESP",
    str_detect(Role, "programmer") ~ "ESP",
    str_detect(Role, "architect") ~ "ESP",
    str_detect(Role, "scientist") ~ "ESP",
    str_detect(Role, "data science") ~ "ESP",
    str_detect(Role, "data scientist") ~ "ESP",
    str_detect(Role, "data scentist") ~ "ESP",
    str_detect(Role, "senior technical staff member") ~ "ESP",
    str_detect(Role, "researcher") ~ "ESP",
    str_detect(Role, "design") ~ "ESP",
    str_detect(Role, "sales") ~ "Sales",
    str_detect(Role, "talent") ~ "Corporate",
    str_detect(Role, "hr") ~ "Corporate",
    str_detect(Role, "human") ~ "Corporate",
    str_detect(Role, "executive") ~ "Corporate",
    str_detect(Role, "partner") ~ "Corporate",
    str_detect(Role, "accountant") ~ "Corporate",
    str_detect(Role, "technician") ~ "AESP",
    str_detect(Role, "specialist") ~ "AESP",
    str_detect(Role, "senior") ~ "Manager",
    str_detect(Role, "managing") ~ "Manager",
    str_detect(Role, "consultant") ~ "Corporate",
    str_detect(Role, "admin") ~ "Corporate",
    str_detect(Role, "vice pres") ~ "Director",
    str_detect(Role, "svp") ~ "Director",
    str_detect(Role, "research") ~ "ESP",
    str_detect(Role, "lead") ~ "Manager",
    str_detect(Role, "writer") ~ "Corporate",
    str_detect(Role, "chief") ~ "Director",
    str_detect(Role, "tech") ~ "AESP",
    str_detect(Role, "client rep") ~ "Sales",
    str_detect(Role, "service rep") ~ "Sales",
    str_detect(Role, "account rep") ~ "Sales",
    str_detect(Role, "support serv") ~ "Sales",
    str_detect(Role, "acquisition") ~ "Corporate",
    str_detect(Role, "president") ~ "Director",
    str_detect(Role, "legal") ~ "Corporate",
    str_detect(Role, "business") ~ "Corporate",
    str_detect(Role, "manage") ~ "Manager",
    str_detect(Role, "product") ~ "Manager",
    str_detect(Role, "procurement") ~ "Corporate",
    str_detect(Role, "market") ~ "Corporate",
    str_detect(Role, "finance") ~ "Corporate",
    str_detect(Role, "people") ~ "Corporate",
    str_detect(Role, "rep") ~ "Corporate",
    str_detect(Role, "dev") ~ "ESP",
    str_detect(Role, "agent") ~ "Corporate",
    str_detect(Role, "bdr") ~ "Corporate",
    str_detect(Role, "quality") ~ "AESP",
    str_detect(Role, "support") ~ "AESP",
    str_detect(Role, "attorney") ~ "Corporate",
    str_detect(Role, "social") ~ "Corporate",
    str_detect(Role, "relations") ~ "Corporate",
    str_detect(Role, "content") ~ "Corporate",
    str_detect(Role, "hardware") ~ "AESP",
    str_detect(Role, "scrum") ~ "Corporate",
    str_detect(Role, "sdm") ~ "Manager",
    str_detect(Role, "rsm") ~ "Manager",
    TRUE ~ Role
  ))

# Discard all other rows that do not have well defined roles
dat_gd_filtered <- dat_gd_newroles %>%
  filter(Role == "ESP" | Role == "Manager" | Role == "Corporate" |
           Role == "Sales" | Role == "AESP" | Role == "Director")

# Convert Role to factor
dat_gd_filtered$Role <- as.factor(dat_gd_filtered$Role)

levels(dat_gd_filtered$Role)

summary(dat_gd_filtered$Role)

## Sentiment analysis

## Pros
# Avg sentiment scores summarized by Role
dat_gd_avgsentiment_pros <- dat_gd_filtered %>%
  select(-Cons) %>%
  get_sentences() %>%
  sentiment_by("Role")

dat_gd_avgsentiment_pros

# Sentiment scores of Pros reviews faceted by Role
dat_gd_filtered %>%
  select(-Cons) %>%
  get_sentences() %>%
  sentiment() %>%
  group_by(element_id, Role) %>%
  summarise(avg_sentiment = mean(sentiment)) %>%
  ungroup() %>%
  left_join(dat_gd_avgsentiment_pros, by="Role") %>%
  ggplot(aes(x=avg_sentiment, fill=Role)) +
  geom_density(alpha=0.6) +
  facet_wrap(~Role) +
  geom_vline(aes(xintercept=ave_sentiment), size=1, color="red") +
  geom_text(aes(x=ave_sentiment+0.6, label=paste0("Mean\n",round(ave_sentiment,3)), y=1.2)) +
  ggtitle("Distribution of Sentiment Scores for Reviews in Pros") +
  labs(x="Sentiment Score", y="Density") +
  theme_classic()

## Cons
# Avg sentiment scores summarized by Role
dat_gd_avgsentiment_cons <- dat_gd_filtered %>%
  select(-Pros) %>%
  get_sentences() %>%
  sentiment_by("Role")

dat_gd_avgsentiment_cons

# Sentiment scores of Cons reviews faceted by Role
dat_gd_filtered %>%
  select(-Pros) %>%
  get_sentences() %>%
  sentiment() %>%
  group_by(element_id, Role) %>%
  summarise(avg_sentiment = mean(sentiment)) %>%
  ungroup() %>%
  left_join(dat_gd_avgsentiment_cons, by="Role") %>%
  ggplot(aes(x=avg_sentiment, fill=Role)) +
  geom_density(alpha=0.6) +
  facet_wrap(~Role) +
  geom_vline(aes(xintercept=ave_sentiment), size=1, color="red") +
  geom_text(aes(x=ave_sentiment+0.6, label=paste0("Mean\n",round(ave_sentiment,3)), y=1.2)) +
  ggtitle("Distribution of Sentiment Scores for Reviews in Cons") +
  labs(x="Sentiment Score", y="Density") +
  theme_classic()