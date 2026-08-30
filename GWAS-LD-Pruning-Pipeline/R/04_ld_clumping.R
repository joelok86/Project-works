# LD Clumping
# -----------
# This script demonstrates LD clumping approaches explored during
# the genomic analysis workflow using bigsnpr and ieugwasr.

library(dplyr)
library(bigsnpr)
library(ieugwasr)

# Load prepared GWAS summary statistics.
# Research data are not included in this public repository.
gwas_snps <- read.csv("path/to/gwas_summary_statistics.csv")

# ------------------------------------------------
# 1. Prepare clumping input
# ------------------------------------------------

clump_input <- gwas_snps |>
  transmute(
    rsid = SNP,
    pval = p_value,
    chr = chromosome,
    pos = position
  )

# ------------------------------------------------
# 2. LD clumping with ieugwasr
# ------------------------------------------------

# Parameters below are placeholders.
# Replace them with values appropriate for the intended analysis.

# clumped_snps <- ld_clump(
#   dat = clump_input,
#   clump_kb = CLUMP_WINDOW_KB,
#   clump_r2 = LD_R2_THRESHOLD,
#   clump_p = P_VALUE_THRESHOLD,
#   pop = "POPULATION"
# )

# ------------------------------------------------
# 3. Alternative clumping approach with bigsnpr
# ------------------------------------------------

# bigsnpr::snp_clumping() can be used when genotype or
# LD-reference information is available locally.

# Example structure:
#
# retained_indices <- snp_clumping(
#   G,
#   infos.chr = chromosome_vector,
#   infos.pos = position_vector,
#   thr.r2 = LD_R2_THRESHOLD,
#   size = CLUMP_WINDOW_SIZE,
#   ncores = 1
# )

# ------------------------------------------------
# 4. Save clumped variants
# ------------------------------------------------

# write.csv(
#   clumped_snps,
#   "path/to/clumped_snps.csv",
#   row.names = FALSE
# )
