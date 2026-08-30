# LD Pair Filtering and Representative SNP Selection
# --------------------------------------------------
# This script converts LD matrices into SNP-pair format,
# applies an r-squared threshold, links SNPs back to GWAS
# association statistics, and retains the SNP with the
# stronger association within each correlated pair.

library(dplyr)
library(tidyr)

# Load GWAS summary statistics.
# Research data are not included in this public repository.
gwas_snps <- read.csv("path/to/gwas_summary_statistics.csv")

# Load previously generated LD matrices.
# Replace this object with the LD matrices created in
# 02_ld_matrix_generation.R when running locally.
# ld_matrices <- readRDS("path/to/ld_matrices.rds")

# Example LD threshold.
# Replace with the threshold used for the intended analysis.
ld_threshold <- 0.1

# ------------------------------------------------
# 1. Convert LD matrices to long SNP-pair format
# ------------------------------------------------

ld_pair_list <- list()

for (i in seq_along(ld_matrices)) {

  current_matrix <- as.data.frame(ld_matrices[[i]])

  current_matrix$SNP_1 <- rownames(current_matrix)

  current_pairs <- current_matrix |>
    pivot_longer(
      cols = -SNP_1,
      names_to = "SNP_2",
      values_to = "r2"
    ) |>
    mutate(r2 = as.numeric(r2)) |>
    filter(SNP_1 != SNP_2)

  ld_pair_list[[i]] <- current_pairs
}

ld_pairs <- bind_rows(ld_pair_list)

# ------------------------------------------------
# 2. Separate correlated and weakly correlated pairs
# ------------------------------------------------

correlated_pairs <- ld_pairs |>
  filter(!is.na(r2), r2 >= ld_threshold)

weakly_correlated_pairs <- ld_pairs |>
  filter(!is.na(r2), r2 < ld_threshold)

# ------------------------------------------------
# 3. Add GWAS p-values to each SNP in correlated pairs
# ------------------------------------------------

correlated_pairs <- correlated_pairs |>
  left_join(
    gwas_snps |>
      select(SNP, p_value),
    by = c("SNP_1" = "SNP")
  ) |>
  rename(p_value_1 = p_value) |>
  left_join(
    gwas_snps |>
      select(SNP, p_value),
    by = c("SNP_2" = "SNP")
  ) |>
  rename(p_value_2 = p_value)

# ------------------------------------------------
# 4. Retain the SNP with the stronger association
# ------------------------------------------------

selected_snps <- correlated_pairs |>
  filter(
    !is.na(p_value_1),
    !is.na(p_value_2)
  ) |>
  mutate(
    retained_snp = if_else(
      p_value_1 <= p_value_2,
      SNP_1,
      SNP_2
    ),
    retained_p_value = pmin(
      p_value_1,
      p_value_2
    )
  ) |>
  select(
    retained_snp,
    retained_p_value
  ) |>
  distinct()

# Review retained variants
head(selected_snps)
