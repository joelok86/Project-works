# ============================================================
# Longitudinal Pediatric Nutrition Analysis
# R Script 01: Longitudinal Data Preparation
#
# Purpose:
# - Import follow-up and dietary recall data
# - Standardize participant and visit variables
# - Derive age in months
# - Merge repeated dietary and follow-up records
# - Check participant-level follow-up structure
# - Remove duplicate participant-month observations
#
# Note:
# The original research data are not included in this repository.
# File paths and identifiers have been generalized to protect
# restricted research data.
# ============================================================


library(dplyr)
library(readr)
library(lubridate)


# ------------------------------------------------------------
# 1. Import source datasets
# ------------------------------------------------------------

followup <- read_csv("data/followup_data.csv")
diet_recall <- read_csv("data/dietary_recall_data.csv")


# ------------------------------------------------------------
# 2. Standardize participant identifiers
# ------------------------------------------------------------

followup <- followup %>%
  rename(
    participant_id = participant_identifier
  )

diet_recall <- diet_recall %>%
  rename(
    participant_id = participant_identifier
  )


# ------------------------------------------------------------
# 3. Prepare visit dates and age
# ------------------------------------------------------------

followup <- followup %>%
  mutate(
    visit_date = as.Date(visit_date),
    followup_month = month(visit_date),
    age_months = (age_years * 12) + additional_months
  )


# ------------------------------------------------------------
# 4. Examine participant-level follow-up
# ------------------------------------------------------------

participant_summary <- followup %>%
  group_by(participant_id) %>%
  summarise(
    number_of_visits = n(),
    number_of_unique_months = n_distinct(followup_month),
    .groups = "drop"
  )

summary(participant_summary$number_of_visits)


# ------------------------------------------------------------
# 5. Prepare dietary recall records
# ------------------------------------------------------------

diet_recall <- diet_recall %>%
  mutate(
    followup_month = as.integer(followup_month)
  ) %>%
  arrange(participant_id, followup_month)


# ------------------------------------------------------------
# 6. Prepare follow-up records
# ------------------------------------------------------------

followup <- followup %>%
  mutate(
    followup_month = as.integer(followup_month)
  ) %>%
  arrange(participant_id, followup_month)


# ------------------------------------------------------------
# 7. Merge dietary recall and follow-up data
# ------------------------------------------------------------

longitudinal_data <- diet_recall %>%
  inner_join(
    followup,
    by = c("participant_id", "followup_month")
  )


# ------------------------------------------------------------
# 8. Check participant and visit counts
# ------------------------------------------------------------

longitudinal_data %>%
  summarise(
    participants = n_distinct(participant_id),
    observations = n(),
    followup_months = n_distinct(followup_month)
  )


# ------------------------------------------------------------
# 9. Remove duplicate participant-month observations
# ------------------------------------------------------------

longitudinal_data <- longitudinal_data %>%
  arrange(participant_id, followup_month) %>%
  group_by(participant_id, followup_month) %>%
  slice_tail(n = 1) %>%
  ungroup()


# ------------------------------------------------------------
# 10. Convert dietary variables to numeric
# ------------------------------------------------------------

longitudinal_data <- longitudinal_data %>%
  mutate(
    dietary_carbohydrate = as.numeric(dietary_carbohydrate),
    dietary_fat = as.numeric(dietary_fat),
    dietary_protein = as.numeric(dietary_protein),
    total_calories = as.numeric(total_calories)
  )


# ------------------------------------------------------------
# 11. Review missingness in key analysis variables
# ------------------------------------------------------------

longitudinal_data %>%
  summarise(
    missing_carbohydrate = sum(is.na(dietary_carbohydrate)),
    missing_fat = sum(is.na(dietary_fat)),
    missing_protein = sum(is.na(dietary_protein)),
    missing_calories = sum(is.na(total_calories))
  )


# ------------------------------------------------------------
# 12. Prepare analysis dataset
# ------------------------------------------------------------

analysis_data <- longitudinal_data %>%
  filter(
    !is.na(participant_id),
    !is.na(followup_month)
  )


# ------------------------------------------------------------
# 13. Final structure check
# ------------------------------------------------------------

glimpse(analysis_data)
