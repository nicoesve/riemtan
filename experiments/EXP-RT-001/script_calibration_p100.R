# EXP-RT-001 Phase 4a: Calibration Test for p=100
# Purpose: Measure actual computation time before designing full factorial
# Date: 2025-10-25
# Agent: Toby Esterhase

# Load required packages
library(Matrix)
library(riemtan)
library(parallel)

cat("==================================================\n")
cat("EXP-RT-001 Phase 4a: Calibration Test (p=100)\n")
cat("==================================================\n\n")

# Set seed for reproducibility
set.seed(20251025)

# SCALED PARAMETERS
n <- 500  # Sample size (unchanged)
p <- 100  # Matrix dimension (SCALED UP 10×)
batch_size <- 128  # Known to be efficient from Phase 3
cores <- 16  # Test if parallelization helps at this scale

cat("CALIBRATION TEST PARAMETERS\n")
cat("==================================================\n")
cat("  Sample size (n):", n, "\n")
cat("  Matrix dimension (p):", p, "×", p, "\n")
cat("  Batch size:", batch_size, "\n")
cat("  Cores:", cores, "\n")
cat("  Replications: 1 (calibration run)\n\n")

# Tangent space dimension
d <- p * (p + 1) / 2
cat("  Tangent space dimension (d):", d, "\n")
cat("  Scaling from Phase 3 (p=10):", round(d / 55, 1), "×\n\n")

# System information
cat("SYSTEM INFORMATION\n")
cat("==================================================\n")
cat("  Platform:", .Platform$OS.type, "\n")
cat("  R version:", paste(R.version$major, R.version$minor, sep = "."), "\n")
cat("  Hostname:", Sys.info()["nodename"], "\n")
slurm_job_id <- Sys.getenv("SLURM_JOB_ID", "LOCAL")
slurm_cpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", cores))
cat("  SLURM Job ID:", slurm_job_id, "\n")
cat("  SLURM CPUs allocated:", slurm_cpus, "\n")
cat("  Detected cores:", detectCores(), "\n")
cat("  Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Package versions
cat("PACKAGE VERSIONS\n")
cat("==================================================\n")
cat("  riemtan:", as.character(packageVersion("riemtan")), "\n")
cat("  Matrix:", as.character(packageVersion("Matrix")), "\n")
cat("  parallel:", as.character(packageVersion("parallel")), "\n\n")

# Load AIRM metric
data(airm)
cat("Metric: AIRM loaded successfully\n\n")

# Create reference point and dispersion matrix
cat("DATA GENERATION\n")
cat("==================================================\n")
cat("Generating reference point (", p, "×", p, " identity matrix)...\n", sep = "")
ref_pt <- diag(p) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

cat("Generating dispersion matrix (0.1 × I_", d, ", tangent space)...\n", sep = "")
disp <- 0.1 * diag(d) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

cat("Generating", n, "SPD matrices (", p, "×", p, ") using rspdnorm()...\n", sep = "")
cat("⏱️  This may take several minutes for p=100...\n\n")

timing_gen <- system.time({
  samples <- rspdnorm(n = n, refpt = ref_pt, disp = disp, met = airm)
})

cat("✓ Data generation complete!\n")
cat("  Generation time:", round(timing_gen["elapsed"], 2), "seconds\n")
cat("  Per matrix:", round(timing_gen["elapsed"] / n * 1000, 1), "ms\n\n")

# Set core limit explicitly
old_mc_cores <- getOption("mc.cores", 2L)
options(mc.cores = cores)
Sys.setenv(MC_CORES = cores)

cat("==================================================\n")
cat("FRECHET MEAN COMPUTATION (CALIBRATION RUN)\n")
cat("==================================================\n\n")

cat("⚠️  IMPORTANT: This is a single timing run to estimate cost.\n")
cat("   Based on O(p³) scaling, this could take:\n")
cat("   - Best case: ~1 minute (with optimized BLAS)\n")
cat("   - Worst case: ~30 minutes (without optimization)\n")
cat("   - Extreme case: hours (if algorithm doesn't scale well)\n\n")

cat("Starting Frechet mean computation...\n")
cat("  Parameters: tol=0.05, max_iter=50, lr=0.05, batch_size=", batch_size, "\n", sep = "")
cat("  Cores:", cores, "\n\n")

start_time <- Sys.time()

# Compute Frechet mean with timing
timing <- system.time({
  fmean <- compute_frechet_mean(
    sample = samples,
    tol = 0.05,
    max_iter = 50,
    lr = 0.05,
    batch_size = batch_size
  )
})

end_time <- Sys.time()
total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n")
cat("==================================================\n")
cat("CALIBRATION COMPLETE\n")
cat("==================================================\n\n")

cat("✓ Frechet mean computation successful!\n\n")

cat("TIMING RESULTS:\n")
cat("  Elapsed time:", round(timing["elapsed"], 2), "seconds\n")
cat("  In minutes:", round(timing["elapsed"] / 60, 2), "min\n")
cat("  In hours:", round(timing["elapsed"] / 3600, 3), "hr\n\n")

# Compare to Phase 3 (p=10)
phase3_time <- 66.03  # Phase 3 result: batch=128, cores=4
scaling_observed <- timing["elapsed"] / phase3_time

cat("SCALING ANALYSIS:\n")
cat("  Phase 3 (p=10, batch=128, cores=4):", phase3_time, "sec\n")
cat("  Phase 4a (p=100, batch=128, cores=16):", round(timing["elapsed"], 2), "sec\n")
cat("  Observed scaling:", round(scaling_observed, 1), "×\n")
cat("  Theoretical O(p³) scaling:", (100/10)^3, "×\n\n")

if (scaling_observed < 100) {
  cat("✅ GOOD NEWS: Better than O(p³) scaling!\n")
  cat("   (BLAS optimizations are working)\n\n")
} else if (scaling_observed < 1000) {
  cat("✓ EXPECTED: Close to O(p³) scaling\n\n")
} else {
  cat("⚠️  WORSE than expected: May indicate numerical issues\n\n")
}

# Estimate full Phase 4 cost
cat("PHASE 4 FULL FACTORIAL ESTIMATE:\n")
cat("  Original plan: 6 batch_sizes × 6 cores × 10 reps = 360 runs\n")
cat("  Estimated total time:", round(timing["elapsed"] * 360 / 3600, 1), "hours\n\n")

if (timing["elapsed"] * 360 > 7200) {  # > 2 hours
  cat("⚠️  RECOMMENDATION: Full factorial is too expensive\n")
  cat("   Consider reduced design:\n")
  cat("   - Option 1: Fewer batch sizes [32, 128, 256] × cores [4, 16, 32] × 3 reps = 27 runs\n")
  cat("   - Option 2: Focus on cores [1, 4, 16, 32] × 5 reps = 20 runs (batch=128 fixed)\n")
  cat("   - Option 3: Minimal [4 cores vs 16 cores] × 10 reps = 20 runs\n\n")
} else {
  cat("✓ Full factorial appears feasible in reasonable time\n\n")
}

# Save results
result <- data.frame(
  phase = "Phase4a_Calibration",
  date = Sys.Date(),
  slurm_job_id = slurm_job_id,
  hostname = Sys.info()["nodename"],
  n = n,
  p = p,
  batch_size = batch_size,
  cores = cores,
  metric = "AIRM",
  elapsed_seconds = as.numeric(timing["elapsed"]),
  user_seconds = as.numeric(timing["user"]),
  system_seconds = as.numeric(timing["system"]),
  generation_seconds = as.numeric(timing_gen["elapsed"]),
  replication = 1,
  platform = .Platform$OS.type,
  r_version = paste(R.version$major, R.version$minor, sep = ".")
)

# Save to cluster storage
cluster_exp_dir <- "/N/slate/nescoba/experiments/EXP-RT-001"
cluster_output <- file.path(cluster_exp_dir, "artifacts", "calibration_p100_results.csv")

dir.create(file.path(cluster_exp_dir, "artifacts"),
           showWarnings = FALSE, recursive = TRUE)

write.csv(result, cluster_output, row.names = FALSE)
cat("Results saved to:", cluster_output, "\n")

# Also save local copy
local_output <- "calibration_p100_results.csv"
write.csv(result, local_output, row.names = FALSE)
cat("Local copy:", local_output, "\n\n")

cat("==================================================\n")
cat("NEXT STEPS\n")
cat("==================================================\n\n")

cat("1. Retrieve results from cluster\n")
cat("2. Analyze timing to determine Phase 4 design\n")
cat("3. Based on this run, decide:\n")
cat("   - Full factorial (if time permits)\n")
cat("   - Reduced factorial (if moderate time)\n")
cat("   - Minimal comparison (if long time)\n\n")

cat("Session Info:\n")
print(sessionInfo())

# Restore core setting
options(mc.cores = old_mc_cores)
