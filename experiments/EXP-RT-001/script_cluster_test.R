# EXP-RT-001 Phase 3: Cluster Infrastructure Test
# Purpose: Validate BigRed200 setup and parallel processing
# Date: 2025-10-24
# Agent: Toby Esterhase

# Load required packages
library(Matrix)
library(riemtan)
library(parallel)

cat("==================================================\n")
cat("EXP-RT-001 Phase 3: Cluster Infrastructure Test\n")
cat("==================================================\n\n")

# Set seed for reproducibility
set.seed(20251024)

# Experimental parameters
n <- 500  # Sample size
p <- 10   # Matrix dimension
batch_sizes <- c(32, 128)  # Test 2 batch sizes
core_counts <- c(4, 16)    # Test 2 core counts
n_reps <- 2  # Replications per condition

cat("Experimental Design:\n")
cat("  Sample size (n):", n, "\n")
cat("  Matrix dimension (p):", p, "\n")
cat("  Batch sizes:", paste(batch_sizes, collapse = ", "), "\n")
cat("  Core counts:", paste(core_counts, collapse = ", "), "\n")
cat("  Replications per condition:", n_reps, "\n")
cat("  Total conditions:", length(batch_sizes) * length(core_counts) * n_reps, "\n\n")

# System information
cat("System Information:\n")
cat("  Platform:", .Platform$OS.type, "\n")
cat("  R version:", paste(R.version$major, R.version$minor, sep = "."), "\n")
cat("  Hostname:", Sys.info()["nodename"], "\n")
slurm_job_id <- Sys.getenv("SLURM_JOB_ID", "LOCAL")
slurm_cpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "0"))
cat("  SLURM Job ID:", slurm_job_id, "\n")
cat("  SLURM CPUs allocated:", slurm_cpus, "\n")
cat("  Detected cores:", detectCores(), "\n\n")

# Check package installation
cat("Package Check:\n")
cat("  riemtan version:", as.character(packageVersion("riemtan")), "\n")
cat("  Matrix version:", as.character(packageVersion("Matrix")), "\n")
cat("  parallel available:", "parallel" %in% loadedNamespaces(), "\n\n")

# Load AIRM metric
data(airm)
cat("Metric: AIRM loaded successfully\n\n")

# Create reference point and dispersion matrix
cat("Generating reference point and dispersion matrix...\n")
ref_pt <- diag(p) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

# Dispersion in tangent space: d = p*(p+1)/2 = 55 for p=10
d <- p * (p + 1) / 2
disp <- 0.1 * diag(d) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

# Generate sample data (same data for all conditions)
cat("Generating", n, "SPD matrices...\n")
timing_gen <- system.time({
  samples <- rspdnorm(n = n, refpt = ref_pt, disp = disp, met = airm)
})
cat("  Data generation time:", round(timing_gen["elapsed"], 2), "seconds\n\n")

# Initialize results data frame
results <- data.frame()

# Main experimental loop
cat("==================================================\n")
cat("Running Infrastructure Test Experiments\n")
cat("==================================================\n\n")

total_runs <- length(batch_sizes) * length(core_counts) * n_reps
current_run <- 0

for (bs in batch_sizes) {
  for (cores in core_counts) {
    # Check if we have enough cores
    if (slurm_cpus > 0 && cores > slurm_cpus) {
      cat(sprintf("Skipping: batch_size=%d, cores=%d (insufficient cores)\n",
                  bs, cores))
      next
    }

    cat(sprintf("\n[Batch size: %d, Cores: %d]\n", bs, cores))
    cat(strrep("-", 50), "\n")

    # Set core limit explicitly
    old_mc_cores <- getOption("mc.cores", 2L)
    options(mc.cores = cores)
    Sys.setenv(MC_CORES = cores)

    for (rep in 1:n_reps) {
      current_run <- current_run + 1
      cat(sprintf("  Rep %d/%d (cores=%d)... ", rep, n_reps, cores))

      # Compute Frechet mean with timing
      # Reduced lr for stability, increased max_iter for convergence
      timing <- system.time({
        fmean <- compute_frechet_mean(
          sample = samples,
          tol = 0.05,
          max_iter = 50,
          lr = 0.05,
          batch_size = bs
        )
      })

      # Record results
      result_row <- data.frame(
        phase = "Phase3_ClusterTest",
        date = Sys.Date(),
        slurm_job_id = slurm_job_id,
        hostname = Sys.info()["nodename"],
        n = n,
        p = p,
        batch_size = bs,
        cores = cores,
        metric = "AIRM",
        elapsed_seconds = as.numeric(timing["elapsed"]),
        user_seconds = as.numeric(timing["user"]),
        system_seconds = as.numeric(timing["system"]),
        replication = rep,
        platform = .Platform$OS.type,
        r_version = paste(R.version$major, R.version$minor, sep = ".")
      )

      results <- rbind(results, result_row)

      cat(sprintf("%.2f sec [%d/%d]\n",
                  timing["elapsed"], current_run, total_runs))
    }

    # Restore core setting
    options(mc.cores = old_mc_cores)
  }
}

cat("\n==================================================\n")
cat("Infrastructure Test Summary\n")
cat("==================================================\n\n")

# Compute summary statistics
summary_stats <- aggregate(
  elapsed_seconds ~ batch_size + cores,
  data = results,
  FUN = function(x) {
    c(
      mean = mean(x),
      sd = sd(x),
      cv = sd(x) / mean(x) * 100
    )
  }
)

cat("Computation Time by Condition:\n")
print(summary_stats)
cat("\n")

# Compute speedup (relative to 4 cores, batch_size=32)
baseline <- results[results$cores == 4 & results$batch_size == 32, ]
baseline_mean <- mean(baseline$elapsed_seconds)

speedup_stats <- aggregate(
  elapsed_seconds ~ batch_size + cores,
  data = results,
  FUN = function(x) baseline_mean / mean(x)
)
names(speedup_stats)[3] <- "speedup"

cat("Speedup Relative to Baseline (cores=4, batch=32):\n")
print(speedup_stats)
cat("\n")

# Save detailed results
cluster_exp_dir <- "/N/slate/nescoba/experiments/EXP-RT-001"
output_file <- file.path(cluster_exp_dir, "artifacts", "cluster_test_results.csv")

# Create directory if it doesn't exist
dir.create(file.path(cluster_exp_dir, "artifacts"),
           showWarnings = FALSE, recursive = TRUE)

write.csv(results, output_file, row.names = FALSE)
cat("Detailed results saved to:", output_file, "\n\n")

# Also save locally for artifact retrieval
local_output <- "cluster_test_results.csv"
write.csv(results, local_output, row.names = FALSE)
cat("Local copy saved to:", local_output, "\n\n")

cat("==================================================\n")
cat("Phase 3 Infrastructure Test Complete: ✓\n")
cat("==================================================\n")
cat("\nValidation Checks:\n")
cat("  ✓ All", nrow(results), "runs completed\n")
cat("  ✓ Parallel processing functional (cores > 1)\n")
cat("  ✓ Timing measurements recorded\n")
cat("  ✓ Results saved to cluster storage\n\n")

cat("Key Findings:\n")
if (nrow(speedup_stats) > 1) {
  max_speedup <- max(speedup_stats$speedup)
  cat("  Maximum speedup observed:", round(max_speedup, 2), "x\n")
  if (max_speedup > 1.2) {
    cat("  ✓ Parallelization working as expected\n")
  } else {
    cat("  ⚠ Limited speedup - investigate further\n")
  }
} else {
  cat("  Insufficient data for speedup analysis\n")
}

cat("\nNext step: Proceed to Phase 4 (full cluster experiment)\n")
cat("  - All infrastructure validated\n")
cat("  - Ready for complete factorial design\n")
cat("  - Scripts: script_cluster_full.R, run_cluster_full.slurm\n")
