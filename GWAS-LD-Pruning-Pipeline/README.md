## Overview

This project presents an R-based workflow developed during genetic epidemiology research for processing genome-wide association study (GWAS) summary statistics and identifying approximately independent genetic variants for downstream analyses.

The workflow includes preparation and filtering of GWAS summary statistics, chromosome-based organization of variants, linkage disequilibrium (LD) matrix generation, processing of large SNP sets in batches, identification of correlated SNP pairs using r², comparison of association p-values within correlated pairs, and LD-based SNP pruning/clumping.

A key computational challenge in the original analysis was processing large numbers of variants within the input limits of LDlinkR. Variants were organized by chromosome and genomic position and processed in smaller batches before LD relationships were evaluated.

The repository contains cleaned and organized versions of the analytical code. The original research datasets are not included because they are not publicly shareable.

## Workflow

### 1. GWAS Data Preparation
GWAS summary statistics are imported, standardized, converted to appropriate data types, filtered using association p-values, and organized by chromosome and genomic position.

### 2. LD Matrix Generation
SNPs are processed using LDlinkR to generate pairwise linkage disequilibrium matrices. Large chromosome-specific SNP sets are divided into smaller batches when necessary to accommodate LDmatrix input limits.

### 3. LD Pair Filtering and SNP Selection
LD matrices are transformed into SNP-pair data. Correlated SNP pairs are identified using an r² threshold and linked back to GWAS association statistics. P-values are compared to identify representative variants among correlated SNPs.

### 4. LD Clumping
LD-based clumping approaches are explored using genomic analysis tools including ieugwasr and bigsnpr to identify approximately independent variants for downstream analyses.

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

## Skills

- Genetic epidemiology
- GWAS summary-statistics processing
- Genomic data cleaning and harmonization
- Linkage disequilibrium analysis
- SNP pruning and clumping
- R programming
- Reproducible research workflows
- Large-scale biological data processing
