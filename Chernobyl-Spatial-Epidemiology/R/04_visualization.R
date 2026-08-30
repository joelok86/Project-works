# ============================================================
# Chernobyl Spatial Epidemiology
# R Script 04: Visualization
#
# Purpose:
# - Map environmental Cs-137 contamination
# - Visualize pediatric thyroid cancer incidence
# - Display integrated exposure-outcome spatial patterns
# - Create exploratory regression plots
#
# Note:
# Variable names have been generalized for public sharing.
# ============================================================


library(sf)
library(dplyr)
library(ggplot2)
library(tmap)


# ------------------------------------------------------------
# 1. Soil Cs-137 contamination within the study region
# ------------------------------------------------------------

ggplot() +
  geom_sf(
    data = belarus_border,
    fill = NA
  ) +
  geom_sf(
    data = soil_cs137,
    aes(color = cs137_ci_km2),
    size = 2
  ) +
  geom_sf(
    data = chernobyl_site,
    shape = 8,
    size = 3
  ) +
  labs(
    title = "Spatial Distribution of Soil Cs-137 Contamination",
    color = "Cs-137\n(Ci/km²)"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 2. Pediatric thyroid cancer incidence by district
# ------------------------------------------------------------

ggplot(thyroid_risk) +
  geom_sf(
    aes(fill = thyroid_incidence_per_1000)
  ) +
  labs(
    title = "Pediatric Thyroid Cancer Incidence by District",
    fill = "Incidence\nper 1,000"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 3. Relative risk of pediatric thyroid cancer
# ------------------------------------------------------------

ggplot(thyroid_risk) +
  geom_sf(
    aes(fill = relRisk)
  ) +
  labs(
    title = "Relative Risk of Pediatric Thyroid Cancer",
    fill = "Relative Risk"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 4. Combine cancer incidence and soil Cs-137 measurements
# ------------------------------------------------------------

ggplot() +
  geom_sf(
    data = thyroid_risk,
    aes(fill = thyroid_incidence_per_1000)
  ) +
  geom_sf(
    data = soil_cs137,
    aes(size = cs137_ci_km2),
    alpha = 0.6
  ) +
  geom_sf(
    data = chernobyl_site,
    shape = 8,
    size = 3
  ) +
  labs(
    title = "Pediatric Thyroid Cancer and Soil Cs-137",
    fill = "Cancer incidence\nper 1,000",
    size = "Soil Cs-137\n(Ci/km²)"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 5. Berry Cs-137 and thyroid cancer
# ------------------------------------------------------------

ggplot() +
  geom_sf(
    data = thyroid_risk,
    aes(fill = thyroid_incidence_per_1000)
  ) +
  geom_sf(
    data = berry_cs137,
    aes(size = cs137_ci_kg),
    alpha = 0.6
  ) +
  labs(
    title = "Berry Cs-137 and Pediatric Thyroid Cancer",
    fill = "Cancer incidence\nper 1,000",
    size = "Berry Cs-137\n(Ci/kg)"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 6. Soil Cs-137 vs thyroid cancer incidence
# ------------------------------------------------------------

cancer_soil_plot_data <- cancer_soil_analysis %>%
  st_drop_geometry()


ggplot(
  cancer_soil_plot_data,
  aes(
    x = cs137_ci_km2,
    y = thyroid_incidence_per_1000
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Soil Cs-137 and Pediatric Thyroid Cancer Incidence",
    x = "Soil Cs-137 (Ci/km²)",
    y = "Thyroid Cancer Incidence per 1,000"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 7. Soil Cs-137 vs relative risk
# ------------------------------------------------------------

ggplot(
  cancer_soil_plot_data,
  aes(
    x = cs137_ci_km2,
    y = relRisk
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Soil Cs-137 and Relative Risk",
    x = "Soil Cs-137 (Ci/km²)",
    y = "Relative Risk"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 8. Distance from Chernobyl vs Cs-137 contamination
# ------------------------------------------------------------

distance_plot_data <- monitoring_60km_sf %>%
  st_drop_geometry()


ggplot(
  distance_plot_data,
  aes(
    x = distance_km,
    y = cs137_ci_km2
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Cs-137 Contamination by Distance from Chernobyl",
    x = "Distance from Chernobyl (km)",
    y = "Soil Cs-137 (Ci/km²)"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 9. Optional interactive-style map with tmap
# ------------------------------------------------------------

tm_shape(thyroid_risk) +
  tm_polygons(
    col = "thyroid_incidence_per_1000",
    title = "Cancer incidence"
  ) +
  tm_shape(soil_cs137) +
  tm_dots(
    col = "cs137_ci_km2",
    size = 0.05,
    title = "Cs-137"
  ) +
  tm_shape(chernobyl_site) +
  tm_symbols(
    size = 0.5
  ) +
  tm_layout(
    title = "Environmental Cs-137 and Pediatric Thyroid Cancer"
  )
