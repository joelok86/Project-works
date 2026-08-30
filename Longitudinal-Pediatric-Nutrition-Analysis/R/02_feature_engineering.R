# ============================================================
# Longitudinal Pediatric Nutrition Analysis
# R Script 02: Feature Engineering
#
# Purpose:
# - Standardize malnutrition classifications
# - Construct food-security and household variables
# - Derive maternal education categories
# - Create nutritional supplementation measures
# - Derive breastfeeding and recent illness indicators
# - Create age groups for stratified longitudinal analysis
#
# Note:
# Original response categories and free-text values have been
# simplified or generalized to protect restricted research data.
# ============================================================


library(dplyr)
library(stringr)
library(tidyr)


# ------------------------------------------------------------
# 1. Standardize malnutrition classifications
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    acute_malnutrition = case_when(
      acute_nutrition_status %in% c(
        "moderate",
        "severe"
      ) ~ 1,
      acute_nutrition_status == "normal" ~ 0,
      TRUE ~ NA_real_
    ),

    chronic_malnutrition = case_when(
      chronic_nutrition_status %in% c(
        "moderate",
        "severe"
      ) ~ 1,
      chronic_nutrition_status == "normal" ~ 0,
      TRUE ~ NA_real_
    ),

    underweight = case_when(
      weight_age_status %in% c(
        "moderate",
        "severe"
      ) ~ 1,
      weight_age_status == "normal" ~ 0,
      TRUE ~ NA_real_
    )
  )


# ------------------------------------------------------------
# 2. Propagate participant-level baseline classifications
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  group_by(participant_id) %>%
  fill(
    acute_malnutrition,
    chronic_malnutrition,
    underweight,
    .direction = "downup"
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 3. Construct housing score
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    housing_score = rowSums(
      across(
        c(
          floor_score,
          roof_score,
          wall_score
        )
      ),
      na.rm = TRUE
    )
  )


# ------------------------------------------------------------
# 4. Construct food-security variables
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    food_availability = case_when(
      difficulty_obtaining_food == "yes" ~ 1,
      purchased_enough_food == "yes" ~ 0,
      TRUE ~ NA_real_
    ),

    food_sufficiency = case_when(
      no_meal_day == "yes" ~ 3,
      one_meal_day == "yes" ~ 2,
      ran_out_of_food == "yes" |
        missed_meal == "yes" ~ 1,
      TRUE ~ 0
    )
  )


# ------------------------------------------------------------
# 5. Carry participant-level food-security information
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  group_by(participant_id) %>%
  fill(
    food_availability,
    food_sufficiency,
    .direction = "downup"
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 6. Maternal education
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    maternal_education_group = case_when(
      maternal_education %in% c(
        "none",
        "primary"
      ) ~ "Primary or less",

      maternal_education %in% c(
        "secondary",
        "high school"
      ) ~ "Secondary",

      maternal_education %in% c(
        "technical",
        "university"
      ) ~ "Post-secondary",

      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 7. Nutritional supplementation
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    supplement_count = as.numeric(supplement_count),

    supplement_carbohydrate =
      supplement_carbohydrate_per_serving *
      supplement_count,

    supplement_protein =
      supplement_protein_per_serving *
      supplement_count,

    supplement_fat =
      supplement_fat_per_serving *
      supplement_count
  )


# Convert monthly supplement totals to approximate daily intake
analysis_data <- analysis_data %>%
  mutate(
    supplement_carbohydrate_daily =
      supplement_carbohydrate / 30,

    supplement_protein_daily =
      supplement_protein / 30,

    supplement_fat_daily =
      supplement_fat / 30
  )


# ------------------------------------------------------------
# 8. Recent illness indicator
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    recent_diarrhea = case_when(
      diarrhea_reported == "yes" ~ 1,
      diarrhea_reported == "no" ~ 0,
      TRUE ~ NA_real_
    )
  )


# ------------------------------------------------------------
# 9. Breastfeeding exposure
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    breastfeeding_duration =
      as.numeric(
        str_extract(
          breastfeeding_duration_text,
          "\\d+"
        )
      ),

    breastfeeding_frequency =
      as.numeric(
        str_extract(
          breastfeeding_frequency_text,
          "\\d+"
        )
      ),

    breastfeeding_exposure =
      breastfeeding_duration *
      breastfeeding_frequency
  )


# ------------------------------------------------------------
# 10. Age group for stratified analyses
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    age_group = case_when(
      age_months < 24 ~ "<24 months",
      age_months >= 24 ~ ">=24 months",
      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 11. Standardize continuous predictors
# ------------------------------------------------------------

continuous_predictors <- c(
  "dietary_carbohydrate",
  "dietary_fat",
  "dietary_protein",
  "total_calories",
  "supplement_carbohydrate_daily",
  "supplement_protein_daily",
  "supplement_fat_daily",
  "breastfeeding_exposure"
)

analysis_data <- analysis_data %>%
  mutate(
    across(
      all_of(continuous_predictors),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_z"
    )
  )


# ------------------------------------------------------------
# 12. Final checks
# ------------------------------------------------------------

analysis_data %>%
  select(
    participant_id,
    age_months,
    age_group,
    acute_malnutrition,
    chronic_malnutrition,
    underweight,
    food_availability,
    food_sufficiency,
    maternal_education_group,
    recent_diarrhea
  ) %>%
  glimpse()
