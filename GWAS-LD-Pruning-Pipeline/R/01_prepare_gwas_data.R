# GWAS Summary Statistics Preparation
# -----------------------------------
# This script imports GWAS summary statistics, standardizes key variables,
# converts columns to appropriate data types, and filters SNPs based on
# statistical significance.

library(readr)
library(dplyr)

# Import GWAS summary statistics
# Research data are not included in this public repository.
gwas_data <- read_table("path/to/gwas_summary_statistics.txt")

# Standardize key GWAS variables
gwas_1 <- gwas_data |>
  transmute(
    SNP = as.character(SNPID),
    effect_allele = as.character(EffectAllele),
    alternate_allele = as.character(AlternateAllele),
    effect_allele_frequency = as.numeric(EffectAlleleFrequency),
    beta = as.numeric(EffectSize.Beta),
    p_value = as.numeric(Pvalue),
    sample_size = as.numeric(SampleSize),
    chromosome = as.integer(Chromosome),
    position = as.integer(Position),
    odds_ratio = as.numeric(EffectSize.OR)
  )

# Review the cleaned dataset
glimpse(gwas_1)

# Example significance threshold
# Modify this value depending on the analytical objective.
p_threshold <- 5e-8

significant_snps <- gwas_1 |>
  filter(!is.na(p_value), p_value < p_threshold)

# Arrange variants by chromosome and genomic position
significant_snps <- significant_snps |>
  arrange(chromosome, position)

# Save processed data
# Output files are not included in this repository.
write_csv(
  significant_snps,
  "significant_snps_final.csv"
)
