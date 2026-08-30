# Linkage Disequilibrium Analysis and SNP Clumping
# ------------------------------------------------
# This script demonstrates an R workflow for evaluating linkage disequilibrium
# among GWAS variants and selecting approximately independent SNPs for
# downstream genetic epidemiology analyses.

library(LDlinkR)
library(dplyr)
library(ieugwasr)

# Read GWAS variants file
gwas_snps <- read.csv("significant_snps_final.csv")

# Sort variants ordered by chromosome and genomic position
gwas_snps <- gwas_snps |>
  arrange(chromosome, position)

# ------------------------------------------------
# 1. Linkage disequilibrium matrix
# ------------------------------------------------

# LDlinkR can be used to estimate pairwise linkage disequilibrium
# among rsIDs. Population, genome build, and API token should be
# supplied by the user when running the analysis.

example_snps <- head(gwas_snps$SNP, 50)

ld_matrix <- LDmatrix(
  snps = example_snps,
  pop = "EUR",
  r2d = "r2",
  token = Sys.getenv("LDLINK_TOKEN"),
  genome_build = "grch37"
)

# ------------------------------------------------
# 2. Convert LD matrix into SNP-pair format
# ------------------------------------------------

ld_pairs <- as.data.frame(ld_matrix)

ld_pairs$SNP_1 <- rownames(ld_pairs)

ld_pairs_long <- ld_pairs |>
  tidyr::pivot_longer(
    cols = -SNP_1,
    names_to = "SNP_2",
    values_to = "r2"
  ) |>
  filter(SNP_1 != SNP_2) |>
  mutate(r2 = as.numeric(r2))

# ------------------------------------------------
# 3. Identify correlated SNP pairs
# ------------------------------------------------

ld_threshold <- 0.1

correlated_pairs <- ld_pairs_long |>
  filter(!is.na(r2), r2 >= ld_threshold)

# ------------------------------------------------
# 4. Attach GWAS association statistics
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
# 5. Select representative SNP from each correlated pair
# ------------------------------------------------

representative_snps <- correlated_pairs |>
  mutate(
    retained_snp = if_else(
      p_value_1 <= p_value_2,
      SNP_1,
      SNP_2
    ),
    retained_p_value = pmin(
      p_value_1,
      p_value_2,
      na.rm = TRUE
    )
  ) |>
  select(retained_snp, retained_p_value) |>
  distinct()

# ------------------------------------------------
# 6. Alternative LD clumping workflow
# ------------------------------------------------

# ieugwasr::ld_clump() can also be used to select approximately
# independent variants based on genomic distance, LD and p-values.

clump_input <- gwas_snps |>
  transmute(
    rsid = SNP,
    pval = p_value
  )

# Example only:
#
# clumped_snps <- ld_clump(
#   dat = clump_input,
#   clump_kb = 10000,
#   clump_r2 = 0.1,
#   clump_p = 5e-8,
#   pop = "EUR"
# )

# ------------------------------------------------
# 7. Export representative variants
# ------------------------------------------------

write.csv(
  representative_snps,
  "representative_snps_example.csv",
  row.names = FALSE
)
