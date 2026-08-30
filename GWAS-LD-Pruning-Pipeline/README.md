# GWAS Linkage Disequilibrium and SNP Pruning Pipeline

## Overview

This project shows an R-based workflow developed for processing genome-wide association study (GWAS) summary statistics and identifying independent genetic variants for genetic epidemiology analyses.

The workflow includes data preparation, SNP filtering, chromosome-based processing, linkage disequilibrium (LD) assessment, and LD pruning/clumping.

The original analysis was developed as part of genomic epidemiology research involving genetic variants associated with complex traits. Research datasets are not included in this repository.

## Objectives

The workflow was developed to:

- Import and standardize GWAS summary statistics
- Filter SNPs based on statistical significance
- Organize variants by chromosome and genomic position
- Assess linkage disequilibrium between genetic variants
- Identify correlated SNP pairs using LD (r²) thresholds
- Select representative variants based on association strength
- Perform LD pruning/clumping to generate sets of approximately independent SNPs

## Tools and Technologies

- R
- LDlinkR
- ieugwasr
- bigsnpr
- snpStats
- data.table
- PLINK-compatible workflows

## Workflow

1. **GWAS data preparation**  
   Import and standardize SNP identifiers, effect alleles, alternate alleles, effect sizes, p-values, chromosome numbers, and genomic positions.

2. **SNP filtering**  
   Filter variants according to predefined statistical significance thresholds.

3. **Chromosome-based processing**  
   Organize SNPs by chromosome and genomic position for efficient LD analysis.

4. **Linkage disequilibrium analysis**  
   Estimate pairwise LD and identify correlated variants using r² thresholds.

5. **SNP selection and clumping**  
   Compare association statistics among correlated variants and retain representative SNPs for downstream analyses.

## Repository Structure

```text
GWAS-LD-Pruning-Pipeline/
├── README.md
└── R/
    ├── 01_prepare_gwas_data.R
    ├── 02_ld_matrix_generation.R
    ├── 03_ld_pair_filtering.R
    └── 04_ld_clumping.R
```

## Data Availability

The original research data are not included because they were used within a research environment and are not intended for public redistribution.

The code in this repository is presented as a reproducible demonstration of the analytical workflow.

## Skills Demonstrated

- Genetic epidemiology
- GWAS summary-statistics processing
- Genomic data cleaning and harmonization
- Linkage disequilibrium analysis
- SNP pruning and clumping
- R programming
- Reproducible research workflows
- Large-scale biological data processing
