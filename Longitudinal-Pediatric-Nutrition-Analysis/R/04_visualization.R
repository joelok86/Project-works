# ============================================================
# Longitudinal Pediatric Nutrition Analysis
# R Script 04: Visualization
#
# Purpose:
# - Visualize longitudinal dietary intake
# - Examine anthropometric trajectories across follow-up
# - Plot mixed-model coefficient estimates and confidence intervals
# - Explore variation across age groups
#
# Note:
# The original research data are not included in this repository.
# Variable names have been generalized for public sharing.
# ============================================================


library(dplyr)
library(tidyr)
library(ggplot2)


# ------------------------------------------------------------
# 1. Dietary macronutrient trends across follow-up
# ------------------------------------------------------------

diet_long <- analysis_data %>%
  select(
    participant_id,
    followup_month,
    dietary_carbohydrate,
    dietary_protein,
    dietary_fat
  ) %>%
  pivot_longer(
    cols = c(
      dietary_carbohydrate,
      dietary_protein,
      dietary_fat
    ),
    names_to = "nutrient",
    values_to = "intake"
  )


ggplot(
  diet_long,
  aes(
    x = followup_month,
    y = intake
  )
) +
  geom_point(alpha = 0.25) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  facet_wrap(
    ~ nutrient,
    scales = "free_y"
  ) +
  labs(
    title = "Dietary Intake Across Follow-up",
    x = "Follow-up Month",
    y = "Reported Intake"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 2. Anthropometric trajectories
# ------------------------------------------------------------

anthropometric_long <- analysis_data %>%
  select(
    participant_id,
    followup_month,
    age_group,
    weight_height_z,
    weight_age_z,
    height_age_z
  ) %>%
  pivot_longer(
    cols = c(
      weight_height_z,
      weight_age_z,
      height_age_z
    ),
    names_to = "anthropometric_measure",
    values_to = "z_score"
  )


ggplot(
  anthropometric_long,
  aes(
    x = followup_month,
    y = z_score
  )
) +
  geom_point(alpha = 0.25) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  facet_grid(
    age_group ~ anthropometric_measure,
    scales = "free_y"
  ) +
  labs(
    title = "Anthropometric Trajectories Across Follow-up",
    x = "Follow-up Month",
    y = "Anthropometric Z-score"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 3. Participant-level longitudinal trajectories
# ------------------------------------------------------------

ggplot(
  analysis_data,
  aes(
    x = followup_month,
    y = weight_height_z,
    group = participant_id
  )
) +
  geom_line(alpha = 0.15) +
  geom_smooth(
    aes(group = 1),
    method = "loess",
    se = TRUE
  ) +
  facet_wrap(
    ~ age_group
  ) +
  labs(
    title = "Participant-Level Weight-for-Height Trajectories",
    x = "Follow-up Month",
    y = "Weight-for-Height Z-score"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 4. Prepare mixed-model coefficients for visualization
# ------------------------------------------------------------

coefficient_data <- model_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      dietary_carbohydrate_z =
        "Dietary carbohydrate",
      dietary_fat_z =
        "Dietary fat",
      dietary_protein_z =
        "Dietary protein",
      recent_diarrhea =
        "Recent diarrhea",
      supplement_carbohydrate_daily_z =
        "Supplement carbohydrate",
      supplement_protein_daily_z =
        "Supplement protein",
      supplement_fat_daily_z =
        "Supplement fat",
      breastfeeding_exposure_z =
        "Breastfeeding exposure",
      housing_score =
        "Housing score"
    )
  )


# ------------------------------------------------------------
# 5. Coefficient plots
# ------------------------------------------------------------

ggplot(
  coefficient_data,
  aes(
    x = estimate,
    y = reorder(term, estimate)
  )
) +
  geom_point() +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0.2
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ model,
    scales = "free_y"
  ) +
  labs(
    title = "Mixed-Effects Model Estimates",
    x = "Estimated Association",
    y = NULL
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 6. Exploratory visualization by recent diarrhea
# ------------------------------------------------------------

ggplot(
  analysis_data,
  aes(
    x = dietary_protein,
    y = weight_height_z,
    linetype = factor(recent_diarrhea)
  )
) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Dietary Protein and Anthropometric Outcome",
    subtitle = "Exploratory visualization by recent diarrhea status",
    x = "Dietary Protein",
    y = "Weight-for-Height Z-score",
    linetype = "Recent diarrhea"
  ) +
  theme_minimal()
