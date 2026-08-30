# Chernobyl Spatial Epidemiology

## Overview

This project presents a spatial epidemiologic analysis examining environmental Cesium-137 (Cs-137) contamination and pediatric thyroid cancer incidence following the Chernobyl nuclear disaster.

The analysis integrates environmental radiation measurements, geographic data, and population-level thyroid cancer data from Belarus. Spatial data processing and statistical analyses were performed in R to examine geographic patterns in Cs-137 contamination and explore associations between environmental contamination and pediatric thyroid cancer incidence.

The project was originally developed as part of an Environmental Data Science course and has been reorganized here as a public research and data-science portfolio project.

## Research Questions

The analysis explored several related questions:

- How were soil Cs-137 contamination levels geographically distributed around the Chernobyl region?
- How did pediatric thyroid cancer incidence vary geographically across Belarus?
- Were higher soil Cs-137 levels associated with higher pediatric thyroid cancer incidence?
- How were soil Cs-137 levels related to distance from the Chernobyl reactor?
- Were soil Cs-137 concentrations associated with Cs-137 measurements in berries?

## Analytical Workflow

### 1. Spatial Data Preparation

Multiple environmental and epidemiologic spatial datasets were imported and prepared in R.

The workflow included: 

- Importing vector and tabular spatial datasets
- Creating `sf` spatial objects
- Inspecting and assigning coordinate reference systems
- Transforming datasets into compatible projections
- Converting environmental measurements into consistent units
- Constructing spatial coordinates from distance and angle measurements

### 2. Environmental Exposure Mapping

Cs-137 soil contamination was mapped to examine geographic variation in radioactive deposition following the Chernobyl disaster.

The analysis included contamination measurements within approximately 60 km of the Chernobyl site as well as additional Cs-137 measurements available for Belarus.

### 3. Disease Mapping

District-level pediatric thyroid cancer data were used to examine geographic variation in disease incidence.

Expected case counts and relative-risk measures were calculated from observed case counts and population data and incorporated into the spatial analysis.

### 4. Spatial Data Integration

Environmental and epidemiologic datasets were integrated using spatial operations including:

- Coordinate transformation
- Spatial intersection
- Spatial joins
- Point-in-polygon relationships
- Geometry validation

These operations allowed environmental Cs-137 measurements to be related geographically to district-level thyroid cancer measures.

### 5. Statistical Analysis

Regression models were used to explore associations between:

- Soil Cs-137 levels and pediatric thyroid cancer incidence
- Soil Cs-137 levels and relative risk
- Soil Cs-137 levels and Cs-137 concentrations in berries
- Distance from the Chernobyl site and environmental contamination

The analyses are interpreted as observational and ecological associations rather than evidence of individual-level causal effects.

## Selected Methods

**Spatial analysis**
- Coordinate reference system management
- Spatial transformations
- Spatial joins and intersections
- Point and polygon data integration
- Environmental exposure mapping

**Epidemiologic analysis**
- Incidence mapping
- Expected case calculations
- Relative-risk estimation
- Ecological exposure-outcome analysis

**Statistical analysis**
- Linear regression
- Confidence intervals
- Model coefficient extraction
- Exploratory association analysis

## Software

The project was conducted in **R** using packages including:

- `sf`
- `dplyr`
- `tmap`
- `spdep`
- `raster`
- `broom`

The public portfolio version emphasizes current and relevant components of the original analytical workflow and removes redundant exploratory code.

## Repository Structure

```text
Chernobyl-Spatial-Epidemiology/
├── README.md
└── R/
    ├── 01_data_preparation.R
    ├── 02_spatial_analysis.R
    ├── 03_statistical_models.R
    └── 04_visualization.R
```

## Data Sources

The original analysis used publicly available or secondary environmental and epidemiologic datasets, including data relating to:

- Soil Cs-137 contamination following the Chernobyl disaster
- Cs-137 measurements in environmental samples
- Geographic boundaries and locations
- Pediatric thyroid cancer incidence and population estimates

The original research report documents the individual data sources and supporting references.

## Interpretation and Limitations

This project is an ecological spatial analysis. Geographic associations between environmental Cs-137 measurements and population-level thyroid cancer incidence should not be interpreted as individual-level exposure-response estimates or causal effects.

Important limitations include differences in the timing of exposure and disease measurements, population movement following the Chernobyl disaster, uncertainty in historical exposure reconstruction, and limitations in the geographic metadata available for some source datasets.

## Portfolio Note

The scripts in this repository are cleaned and organized versions of the original analytical workflow. Redundant exploratory code, local file paths, obsolete package dependencies, and intermediate troubleshooting steps have been removed to improve readability and responsible public presentation.
