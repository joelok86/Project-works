# ============================================================
# Chernobyl Spatial Epidemiology
# R Script 01: Data Preparation
#
# Purpose:
# - Import environmental and epidemiologic spatial datasets
# - Validate geometries
# - Standardize coordinate reference systems
# - Convert environmental measurements into consistent units
# - Reconstruct spatial coordinates from distance/angle data
#
# Note:
# Local file paths and project-specific filenames have been
# generalized for public sharing.
# ============================================================


library(sf)
library(dplyr)
library(readr)


# ------------------------------------------------------------
# 1. Import spatial datasets
# ------------------------------------------------------------

belarus_border <- st_read("data/belarus_border.gpkg")

chernobyl_site <- st_read("data/chernobyl_site.gpkg")

soil_cs137 <- st_read("data/soil_cs137.gpkg")

berry_cs137 <- st_read("data/berry_cs137.gpkg")

thyroid_cancer <- st_read("data/thyroid_cancer.gpkg")


# ------------------------------------------------------------
# 2. Validate spatial geometries
# ------------------------------------------------------------

belarus_border <- st_make_valid(belarus_border)

soil_cs137 <- st_make_valid(soil_cs137)

berry_cs137 <- st_make_valid(berry_cs137)

thyroid_cancer <- st_make_valid(thyroid_cancer)


# ------------------------------------------------------------
# 3. Inspect coordinate reference systems
# ------------------------------------------------------------

st_crs(belarus_border)
st_crs(chernobyl_site)
st_crs(soil_cs137)
st_crs(berry_cs137)
st_crs(thyroid_cancer)


# ------------------------------------------------------------
# 4. Transform datasets to a common CRS
# ------------------------------------------------------------

target_crs <- st_crs(thyroid_cancer)

belarus_border <- st_transform(
  belarus_border,
  target_crs
)

chernobyl_site <- st_transform(
  chernobyl_site,
  target_crs
)

soil_cs137 <- st_transform(
  soil_cs137,
  target_crs
)

berry_cs137 <- st_transform(
  berry_cs137,
  target_crs
)


# ------------------------------------------------------------
# 5. Import 60-km environmental monitoring data
# ------------------------------------------------------------

monitoring_60km <- read_csv(
  "data/cs137_monitoring_60km.csv"
)


# ------------------------------------------------------------
# 6. Convert distance from kilometers to meters
# ------------------------------------------------------------

monitoring_60km <- monitoring_60km %>%
  mutate(
    distance_m = distance_km * 1000
  )


# ------------------------------------------------------------
# 7. Reconstruct coordinates from angle and distance
# ------------------------------------------------------------

chernobyl_coordinates <- st_coordinates(
  chernobyl_site
)

chernobyl_x <- chernobyl_coordinates[1, 1]
chernobyl_y <- chernobyl_coordinates[1, 2]


calculate_coordinates <- function(
  origin_x,
  origin_y,
  angle_degrees,
  distance_m
) {

  angle_radians <- angle_degrees * pi / 180

  x <- origin_x +
    distance_m * sin(angle_radians)

  y <- origin_y +
    distance_m * cos(angle_radians)

  c(x, y)
}


coordinate_matrix <- t(
  mapply(
    calculate_coordinates,
    angle_degrees = monitoring_60km$angle_degrees,
    distance_m = monitoring_60km$distance_m,
    MoreArgs = list(
      origin_x = chernobyl_x,
      origin_y = chernobyl_y
    )
  )
)

monitoring_60km$X <- coordinate_matrix[, 1]
monitoring_60km$Y <- coordinate_matrix[, 2]


# ------------------------------------------------------------
# 8. Convert reconstructed coordinates to an sf object
# ------------------------------------------------------------

monitoring_60km_sf <- st_as_sf(
  monitoring_60km,
  coords = c("X", "Y"),
  crs = target_crs
)


# ------------------------------------------------------------
# 9. Convert Cs-137 units
# ------------------------------------------------------------

# Example conversion used in the original workflow:
# Bq/m^2 to Ci/km^2

bq_m2_to_ci_km2 <- 2.7027e-5

monitoring_60km_sf <- monitoring_60km_sf %>%
  mutate(
    cs137_ci_km2 =
      cs137_bq_m2 *
      bq_m2_to_ci_km2
  )


# ------------------------------------------------------------
# 10. Convert berry Cs-137 measurements
# ------------------------------------------------------------

# Bq/kg to Ci/kg

bq_to_ci <- 3.7e10

berry_cs137 <- berry_cs137 %>%
  mutate(
    cs137_ci_kg =
      cs137_bq_kg /
      bq_to_ci
  )


# ------------------------------------------------------------
# 11. Final structure checks
# ------------------------------------------------------------

glimpse(monitoring_60km_sf)
glimpse(soil_cs137)
glimpse(berry_cs137)
glimpse(thyroid_cancer)
