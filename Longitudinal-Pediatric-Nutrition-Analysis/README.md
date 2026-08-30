# Longitudinal Pediatric Nutrition Analysis

## Overview

This project showcases the quantitative analysis component of a research capstone examining longitudinal pediatric nutrition and anthropometric outcomes using real-world health data.

The analytical workflow was conducted across **SAS and R**. SAS was used for initial data preparation, recoding, integration of longitudinal records, and exploratory mixed-effects modeling. R was subsequently used for additional data cleaning and feature engineering, longitudinal data preparation, statistical modeling, model diagnostics, and visualization.

The analysis involved repeated observations for individual participants, requiring integration of dietary recall and follow-up records across time and statistical methods that account for within-participant correlation.

## Analytical Workflow

### 1. Data Preparation and Integration
- Cleaned and standardized longitudinal health and nutrition variables
- Derived participant age in months and visit-related variables
- Integrated dietary recall and follow-up records using participant and time identifiers
- Evaluated missingness, duplicate observations, and longitudinal follow-up patterns

### 2. Feature Engineering
- Constructed housing and food-security measures from questionnaire responses
- Standardized nutritional and anthropometric classifications
- Derived nutritional supplementation variables
- Constructed breastfeeding and recent illness-related variables
- Created age groups for age-stratified analyses

### 3. Longitudinal Statistical Analysis
- Evaluated repeated anthropometric measurements across follow-up visits
- Applied linear mixed-effects models to account for repeated observations within participants
- Examined dietary macronutrients, nutritional supplementation, and other health-related predictors
- Conducted age-stratified analyses
- Evaluated model coefficients, confidence intervals, and multicollinearity diagnostics

### 4. Visualization
- Examined longitudinal patterns in dietary intake
- Visualized anthropometric trajectories across repeat visits
- Created coefficient plots with confidence intervals
- Explored differences in associations across participant characteristics

## Software and Methods

**SAS**
- DATA step processing
- Dataset sorting and merging
- Variable recoding and transformation
- PROC MIXED

**R**
- dplyr
- tidyr
- lme4
- ggplot2
- lubridate
- stringr
- car

**Methods**
- Longitudinal data management
- Feature engineering
- Repeated-measures analysis
- Linear mixed-effects modeling
- Age-stratified analysis
- Model diagnostics
- Data visualization

## Privacy and Data Availability

This project was conducted using real-world longitudinal pediatric nutrition data. The original research dataset is not publicly available and is not included in this repository.

Code shared in this repository has been cleaned and generalized to remove participant identifiers, local file paths, project-specific identifiers, and potentially sensitive or restricted information. Variable names may also be generalized where appropriate to protect the underlying research data.

The repository is intended to demonstrate the analytical workflow, and programming approaches used in the project without exposing individual-level research data.

## Reproducibility

Because the original research data cannot be publicly distributed, the scripts are provided as demonstrations of the analytical workflow rather than as a fully reproducible copy of the original study analysis.

## Repository Structure

```text
Longitudinal-Pediatric-Nutrition-Analysis/
├── README.md
├── SAS/
│   ├── 01_data_preparation.sas
│   └── 02_initial_mixed_models.sas
└── R/
    ├── 01_longitudinal_data_preparation.R
    ├── 02_feature_engineering.R
    ├── 03_mixed_effects_models.R
    └── 04_visualization.R
