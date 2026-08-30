# ============================================================
# Longitudinal Pediatric Nutrition Analysis
# R Script 03: Mixed-Effects Models
#
# Purpose:
# - Fit age-stratified longitudinal mixed-effects models
# - Account for repeated observations within participants
# - Evaluate dietary, supplementation, and health predictors
# - Extract model estimates and confidence intervals
# - Assess multicollinearity
#
# Note:
# Variable names have been generalized for public sharing.
# The original research data are not included.
# ============================================================


library(dplyr)
library(lme4)
library(car)
library(broom.mixed)


# ------------------------------------------------------------
# 1. Create age-stratified analysis datasets
# ------------------------------------------------------------

analysis_under_24 <- analysis_data %>%
  filter(age_months < 24)

analysis_24_plus <- analysis_data %>%
  filter(age_months >= 24)


# ------------------------------------------------------------
# 2. Define a reusable model formula
# ------------------------------------------------------------

model_predictors <- c(
  "dietary_carbohydrate_z",
  "dietary_fat_z",
  "dietary_protein_z",
  "recent_diarrhea",
  "supplement_carbohydrate_daily_z",
  "supplement_protein_daily_z",
  "supplement_fat_daily_z",
  "breastfeeding_exposure_z"
)


# ------------------------------------------------------------
# 3. Mixed model: weight-for-height z-score
#    Participants <24 months
# ------------------------------------------------------------

model_whz_under_24 <- lmer(
  weight_height_z ~
    dietary_carbohydrate_z +
    dietary_fat_z +
    dietary_protein_z +
    recent_diarrhea +
    supplement_carbohydrate_daily_z +
    supplement_protein_daily_z +
    supplement_fat_daily_z +
    breastfeeding_exposure_z +
    (1 | participant_id),
  data = analysis_under_24,
  REML = TRUE
)

summary(model_whz_under_24)


# ------------------------------------------------------------
# 4. Mixed model: weight-for-age z-score
#    Participants <24 months
# ------------------------------------------------------------

model_waz_under_24 <- lmer(
  weight_age_z ~
    dietary_carbohydrate_z +
    dietary_fat_z +
    dietary_protein_z +
    recent_diarrhea +
    supplement_carbohydrate_daily_z +
    supplement_protein_daily_z +
    supplement_fat_daily_z +
    breastfeeding_exposure_z +
    (1 | participant_id),
  data = analysis_under_24,
  REML = TRUE
)

summary(model_waz_under_24)


# ------------------------------------------------------------
# 5. Mixed model: height-for-age z-score
#    Participants <24 months
# ------------------------------------------------------------

model_haz_under_24 <- lmer(
  height_age_z ~
    dietary_carbohydrate_z +
    dietary_fat_z +
    dietary_protein_z +
    recent_diarrhea +
    supplement_carbohydrate_daily_z +
    supplement_protein_daily_z +
    supplement_fat_daily_z +
    breastfeeding_exposure_z +
    (1 | participant_id),
  data = analysis_under_24,
  REML = TRUE
)

summary(model_haz_under_24)


# ------------------------------------------------------------
# 6. Mixed model: weight-for-height z-score
#    Participants >=24 months
# ------------------------------------------------------------

model_whz_24_plus <- lmer(
  weight_height_z ~
    dietary_carbohydrate_z +
    dietary_fat_z +
    dietary_protein_z +
    recent_diarrhea +
    supplement_carbohydrate_daily_z +
    supplement_protein_daily_z +
    supplement_fat_daily_z +
    housing_score +
    (1 | participant_id),
  data = analysis_24_plus,
  REML = TRUE
)

summary(model_whz_24_plus)


# ------------------------------------------------------------
# 7. Mixed model: weight-for-age z-score
#    Participants >=24 months
# ------------------------------------------------------------

model_waz_24_plus <- lmer(
  weight_age_z ~
    dietary_carbohydrate_z +
    dietary_fat_z +
    dietary_protein_z +
    recent_diarrhea +
    supplement_carbohydrate_daily_z +
    supplement_protein_daily_z +
    supplement_fat_daily_z +
    housing_score +
    (1 | participant_id),
  data = analysis_24_plus,
  REML = TRUE
)

summary(model_waz_24_plus)


# ------------------------------------------------------------
# 8. Mixed model: height-for-age z-score
#    Participants >=24 months
# ------------------------------------------------------------

model_haz_24_plus <- lmer(
  height_age_z ~
    dietary_carbohydrate_z +
    dietary_fat_z +
    dietary_protein_z +
    recent_diarrhea +
    supplement_carbohydrate_daily_z +
    supplement_protein_daily_z +
    supplement_fat_daily_z +
    housing_score +
    (1 | participant_id),
  data = analysis_24_plus,
  REML = TRUE
)

summary(model_haz_24_plus)


# ------------------------------------------------------------
# 9. Extract fixed-effect estimates and confidence intervals
# ------------------------------------------------------------

extract_model_results <- function(model, model_name) {

  results <- broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE
  )

  results %>%
    mutate(
      model = model_name
    ) %>%
    select(
      model,
      term,
      estimate,
      std.error,
      statistic,
      conf.low,
      conf.high
    )
}


model_results <- bind_rows(
  extract_model_results(
    model_whz_under_24,
    "WHZ: <24 months"
  ),
  extract_model_results(
    model_waz_under_24,
    "WAZ: <24 months"
  ),
  extract_model_results(
    model_haz_under_24,
    "HAZ: <24 months"
  ),
  extract_model_results(
    model_whz_24_plus,
    "WHZ: >=24 months"
  ),
  extract_model_results(
    model_waz_24_plus,
    "WAZ: >=24 months"
  ),
  extract_model_results(
    model_haz_24_plus,
    "HAZ: >=24 months"
  )
)

model_results


# ------------------------------------------------------------
# 10. Multicollinearity assessment
# ------------------------------------------------------------

# VIF is assessed using an equivalent fixed-effects model.
# Random effects are not included in this diagnostic model.

vif_model <- lm(
  weight_height_z ~
    dietary_carbohydrate_z +
    dietary_fat_z +
    dietary_protein_z +
    recent_diarrhea +
    supplement_carbohydrate_daily_z +
    supplement_protein_daily_z +
    supplement_fat_daily_z +
    breastfeeding_exposure_z,
  data = analysis_under_24
)

car::vif(vif_model)


# ------------------------------------------------------------
# 11. Review participant-level random-effect variation
# ------------------------------------------------------------

VarCorr(model_whz_under_24)


# ------------------------------------------------------------
# 12. Model comparison summary
# ------------------------------------------------------------

model_summary <- tibble(
  model = c(
    "WHZ <24",
    "WAZ <24",
    "HAZ <24",
    "WHZ >=24",
    "WAZ >=24",
    "HAZ >=24"
  ),

  AIC = c(
    AIC(model_whz_under_24),
    AIC(model_waz_under_24),
    AIC(model_haz_under_24),
    AIC(model_whz_24_plus),
    AIC(model_waz_24_plus),
    AIC(model_haz_24_plus)
  )
)

model_summary
