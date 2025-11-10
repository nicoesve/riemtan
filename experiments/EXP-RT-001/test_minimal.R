# EXP-RT-001 Phase 1: Minimal Local Test
# Purpose: Verify timing measurement code works for single condition
# Date: 2025-10-24
# Agent: Toby Esterhase

# Load required packages
library(Matrix)
library(riemtan)

cat("========================================\n")
cat("EXP-RT-001 Phase 1: Minimal Local Test\n")
cat("========================================\n\n")

# Set seed for reproducibility
set.seed(20251024)

# Parameters
n <- 500  # Sample size
p <- 10   # Matrix dimension
batch_size <- 32  # Test with default batch size

cat("Parameters:\n")
cat("  Sample size (n):", n, "\n")
cat("  Matrix dimension (p):", p, "\n")
cat("  Batch size:", batch_size, "\n\n")

# Load AIRM metric
data(airm)
cat("Metric: AIRM (Affine Invariant Riemannian Metric)\n\n")

# Create reference point: 10x10 identity matrix
cat("Generating reference point (identity matrix)...\n")
ref_pt <- diag(p) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

# Create dispersion matrix: d x d where d = p*(p+1)/2
# For p=10, d=55 (tangent space dimension)
d <- p * (p + 1) / 2
cat("Generating dispersion matrix (0.1 * identity, dim =", d, ")...\n")
disp <- 0.1 * diag(d) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

# Generate sample data
cat("Generating", n, "SPD matrices using rspdnorm()...\n")
timing_gen <- system.time({
  samples <- rspdnorm(n = n, refpt = ref_pt, disp = disp, met = airm)
})
cat("  Data generation time:", round(timing_gen["elapsed"], 2), "seconds\n\n")

# Compute Frechet mean with timing
cat("Computing Frechet mean...\n")
cat("  Batch size:", batch_size, "\n")
cat("  Tolerance: 0.05\n")
cat("  Max iterations: 50\n")
cat("  Learning rate: 0.05\n\n")

timing_fmean <- system.time({
  fmean <- compute_frechet_mean(
    sample = samples,
    tol = 0.05,
    max_iter = 50,
    lr = 0.05,
    batch_size = batch_size
  )
})

cat("\n========================================\n")
cat("Results:\n")
cat("========================================\n")
cat("Computation time:\n")
cat("  Elapsed:", round(timing_fmean["elapsed"], 3), "seconds\n")
cat("  User:", round(timing_fmean["user"], 3), "seconds\n")
cat("  System:", round(timing_fmean["system"], 3), "seconds\n\n")

cat("Frechet mean computed successfully!\n")
cat("Mean matrix dimension:", dim(fmean), "\n\n")

# Create minimal results data frame
results <- data.frame(
  phase = "Phase1_Minimal",
  date = Sys.Date(),
  n = n,
  p = p,
  batch_size = batch_size,
  metric = "AIRM",
  elapsed_seconds = as.numeric(timing_fmean["elapsed"]),
  user_seconds = as.numeric(timing_fmean["user"]),
  system_seconds = as.numeric(timing_fmean["system"]),
  replication = 1,
  platform = .Platform$OS.type,
  r_version = paste(R.version$major, R.version$minor, sep = ".")
)

# Print results
cat("Results data frame:\n")
print(results)
cat("\n")

# Save results
output_file <- "artifacts/minimal_results.csv"
write.csv(results, output_file, row.names = FALSE)
cat("Results saved to:", output_file, "\n")

cat("\n========================================\n")
cat("Phase 1 Complete: ✓\n")
cat("========================================\n")
cat("\nNext step: Run Phase 2 (test_local.R) for subset testing\n")
