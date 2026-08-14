# GDC Glioblastoma Clinical Data Analysis

## Overview

This project demonstrates an epidemiologic analysis of publicly available clinical data from the National Cancer Institute (NCI) Genomic Data Commons (GDC).

The analysis uses the Foundation Medicine Adult Cancer Clinical Dataset (FM-AD) and examines demographic and diagnostic characteristics of adult central nervous system (CNS) tumor cases, with a particular focus on glioblastoma.

## Objectives

- Assess data quality and completeness
- Characterize the study cohort
- Examine the distribution of CNS tumor diagnoses
- Compare age distributions across diagnosis groups
- Examine sex distribution across diagnosis groups
- Evaluate the association of age and sex with glioblastoma diagnosis

## Methods

The analysis was conducted in R and includes:

- Data cleaning and quality assessment
- Descriptive statistics
- Data visualization
- Kruskal-Wallis test
- Chi-square test
- Multivariable logistic regression

## Key Findings

The dataset included 971 CNS tumor cases. Glioblastoma was the most common diagnosis, accounting for 57.7% of cases.

Age distributions differed significantly across diagnosis groups. In the multivariable logistic regression model, increasing age at diagnosis was associated with higher odds of glioblastoma diagnosis (OR = 1.05 per year; 95% CI: 1.04–1.06). Sex was not significantly associated with glioblastoma diagnosis.

## Tools and Packages

- R
- tidyverse
- ggplot2
- janitor
- broom
- knitr

## Data Source

Data were obtained from the National Cancer Institute Genomic Data Commons (GDC). The source clinical dataset is not redistributed in this repository.

## Repository Contents

- `gdc_glioblastoma_analysis.Rmd` – R Markdown source code and analysis
- `gdc_glioblastoma_analysis.pdf` – Rendered analysis report

## Disclaimer

This project was developed to demonstrate data management, epidemiologic analysis, visualization, and statistical modeling using publicly available data.
