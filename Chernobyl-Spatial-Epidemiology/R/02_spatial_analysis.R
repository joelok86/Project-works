# ============================================================
# Chernobyl Spatial Epidemiology
# R Script 02: Spatial Analysis
#
# Purpose:
# - Validate epidemiologic and environmental geometries
# - Calculate expected case counts and relative risk
# - Integrate disease and environmental data spatially
# - Create combined layers for downstream modeling
#
# Note:
# Dataset names have been generalized for public sharing.
# ============================================================


library(sf)
library(dplyr)
library(spdep)


# ------------------------------------------------------------
# 1. Validate epidemiologic polygons
# ------------------------------------------------------------

thyroid_cancer <- st_make_valid(
  thyroid_cancer
)


# ------------------------------------------------------------
# 2. Calculate expected counts and relative risk
# ------------------------------------------------------------

risk_map <- probmap(
  thyroid_cancer$observed_cases,
  thyroid_cancer$population
)

thyroid_risk <- cbind(
  thyroid_cancer,
  risk_map
)


# ------------------------------------------------------------
# 3. Review calculated epidemiologic measures
# ------------------------------------------------------------

thyroid_risk %>%
  st_drop_geometry() %>%
  select(
    observed_cases,
    population,
    expCount,
    relRisk
  ) %>%
  summary()


# ------------------------------------------------------------
# 4. Intersect thyroid-cancer polygons with soil Cs-137 data
# ------------------------------------------------------------

soil_thyroid_intersection <- st_intersection(
  thyroid_risk,
  soil_cs137
)


# ------------------------------------------------------------
# 5. Inspect the integrated exposure-outcome layer
# ------------------------------------------------------------

soil_thyroid_intersection %>%
  st_drop_geometry() %>%
  select(
    expCount,
    relRisk,
    cs137_ci_km2
  ) %>%
  summary()


# ------------------------------------------------------------
# 6. Join berry Cs-137 measurements to thyroid polygons
# ------------------------------------------------------------

berry_thyroid <- st_join(
  berry_cs137,
  thyroid_risk,
  join = st_within
)


# ------------------------------------------------------------
# 7. Attach soil Cs-137 measurements while preserving
#    district polygons
# ------------------------------------------------------------

district_soil <- st_join(
  thyroid_risk,
  soil_cs137,
  join = st_contains,
  left = TRUE
)


# ------------------------------------------------------------
# 8. Add berry contamination measurements
# ------------------------------------------------------------

district_environment <- st_join(
  district_soil,
  berry_cs137,
  join = st_contains,
  left = TRUE
)


# ------------------------------------------------------------
# 9. Remove records without environmental measurements
# ------------------------------------------------------------

complete_environment <- district_environment %>%
  filter(
    !is.na(cs137_ci_km2),
    !is.na(cs137_ci_kg)
  ) %>%
  st_make_valid()


# ------------------------------------------------------------
# 10. Create simplified analysis datasets
# ------------------------------------------------------------

cancer_soil_analysis <- soil_thyroid_intersection %>%
  select(
    observed_cases,
    population,
    expCount,
    relRisk,
    cs137_ci_km2,
    geometry
  )


berry_soil_analysis <- complete_environment %>%
  select(
    cs137_ci_km2,
    cs137_ci_kg,
    geometry
  )


# ------------------------------------------------------------
# 11. Check spatial compatibility
# ------------------------------------------------------------

st_crs(cancer_soil_analysis)
st_crs(berry_soil_analysis)

st_geometry_type(cancer_soil_analysis)
st_geometry_type(berry_soil_analysis)


# ------------------------------------------------------------
# 12. Review number of spatial features retained
# ------------------------------------------------------------

nrow(cancer_soil_analysis)
nrow(berry_soil_analysis)
