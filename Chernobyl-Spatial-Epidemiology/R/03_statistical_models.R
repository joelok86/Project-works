# ============================================================
# Chernobyl Spatial Epidemiology
# R Script 03: Statistical Models
#
# Purpose:
# - Examine ecological associations between environmental
#   Cs-137 measurements and pediatric thyroid cancer outcomes
# - Evaluate relationships between soil contamination,
#   environmental samples, and distance from Chernobyl
# - Extract model estimates and confidence intervals
#
# Note:
# These are ecological and observational analyses.
# Model coefficients should not be interpreted as individual-
# level causal effects.
# ============================================================


library(dplyr)
library(sf)
library(broom)


# ------------------------------------------------------------
# 1. Soil Cs-137 and pediatric thyroid cancer incidence
# ------------------------------------------------------------

model_incidence <- lm(
  thyroid_incidence_per_1000 ~ cs137_ci_km2,
  data = cancer_soil_analysis
)

summary(model_incidence)


incidence_results <- tidy(
  model_incidence,
  conf.int = TRUE
)

incidence_results


# ------------------------------------------------------------
# 2. Soil Cs-137 and relative risk
# ------------------------------------------------------------

model_relative_risk <- lm(
  relRisk ~ cs137_ci_km2,
  data = cancer_soil_analysis
)

summary(model_relative_risk)


relative_risk_results <- tidy(
  model_relative_risk,
  conf.int = TRUE
)

relative_risk_results


# ------------------------------------------------------------
# 3. Soil Cs-137 and berry Cs-137 contamination
# ------------------------------------------------------------

model_berry <- lm(
  cs137_ci_kg ~ cs137_ci_km2,
  data = berry_soil_analysis
)

summary(model_berry)


berry_results <- tidy(
  model_berry,
  conf.int = TRUE
)

berry_results


# ------------------------------------------------------------
# 4. Distance from Chernobyl and soil Cs-137 contamination
# ------------------------------------------------------------

distance_analysis <- monitoring_60km_sf %>%
  st_drop_geometry() %>%
  filter(
    !is.na(distance_km),
    !is.na(cs137_ci_km2)
  )


model_distance <- lm(
  cs137_ci_km2 ~ distance_km,
  data = distance_analysis
)

summary(model_distance)


distance_results <- tidy(
  model_distance,
  conf.int = TRUE
)

distance_results


# ------------------------------------------------------------
# 5. Collect model results
# ------------------------------------------------------------

model_results <- bind_rows(

  incidence_results %>%
    mutate(
      model = "Thyroid incidence vs soil Cs-137"
    ),

  relative_risk_results %>%
    mutate(
      model = "Relative risk vs soil Cs-137"
    ),

  berry_results %>%
    mutate(
      model = "Berry Cs-137 vs soil Cs-137"
    ),

  distance_results %>%
    mutate(
      model = "Soil Cs-137 vs distance"
    )
)


model_results <- model_results %>%
  select(
    model,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high
  )


model_results


# ------------------------------------------------------------
# 6. Basic model diagnostics
# ------------------------------------------------------------

par(mfrow = c(2, 2))
plot(model_incidence)

par(mfrow = c(1, 1))


# ------------------------------------------------------------
# 7. Review model fit
# ------------------------------------------------------------

model_fit <- tibble(
  model = c(
    "Incidence",
    "Relative risk",
    "Berry contamination",
    "Distance"
  ),

  r_squared = c(
    summary(model_incidence)$r.squared,
    summary(model_relative_risk)$r.squared,
    summary(model_berry)$r.squared,
    summary(model_distance)$r.squared
  ),

  adjusted_r_squared = c(
    summary(model_incidence)$adj.r.squared,
    summary(model_relative_risk)$adj.r.squared,
    summary(model_berry)$adj.r.squared,
    summary(model_distance)$adj.r.squared
  )
)

model_fit
