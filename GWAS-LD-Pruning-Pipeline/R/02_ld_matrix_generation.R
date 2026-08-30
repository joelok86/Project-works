# LD Matrix Generation
# --------------------
# This script generates linkage disequilibrium (LD) matrices for GWAS SNPs
# using LDlinkR. Large SNP sets are processed in smaller batches to account
# for LDmatrix input-size limitations.

library(LDlinkR)
library(dplyr)

# Load prepared GWAS variants.
# Research data are not included in this public repository.
gwas_snps <- read.csv("path/to/gwas_summary_statistics.csv")

# Sort SNPs by chromosome and genomic position
gwas_snps <- gwas_snps |>
  arrange(chromosome, position)

# Example: select variants from one chromosome
chr_snps <- gwas_snps |>
  filter(chromosome == 1) |>
  select(SNP, chromosome, position, p_value)

# ------------------------------------------------
# LDlinkR matrix generation
# ------------------------------------------------

# LDmatrix accepts a limited number of rsIDs per request.
# For larger chromosome-specific SNP sets, variants can be divided
# into smaller batches and processed separately.

batch_size <- 500

snp_batches <- split(
  chr_snps$SNP,
  ceiling(seq_along(chr_snps$SNP) / batch_size)
)

# Store LD matrices generated for each batch
ld_matrices <- vector("list", length(snp_batches))

for (i in seq_along(snp_batches)) {

  message("Processing batch ", i, " of ", length(snp_batches))

  ld_matrices[[i]] <- LDmatrix(
    snps = snp_batches[[i]],
    pop = "POPULATION",
    r2d = "r2",
    token = Sys.getenv("LDLINK_TOKEN"),
    genome_build = "GENOME_BUILD"
  )
}

# Inspect number of LD matrices generated
length(ld_matrices)

# Review dimensions of each matrix
matrix_dimensions <- lapply(ld_matrices, dim)
matrix_dimensions
