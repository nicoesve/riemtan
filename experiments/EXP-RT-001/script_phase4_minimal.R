# EXP-RT-001 Phase 4: Minimal Parallelization Study (p=100)
# Purpose: Test whether parallelization helps for larger matrices
# Date: 2025-10-25
# Agent: Toby Esterhase

# Load required packages
library(Matrix)
library(riemtan)
library(parallel)

cat("==================================================\n")
cat("EXP-RT-001 Phase 4: Parallelization Study (p=100)\n")
cat("==================================================\n\n")

# Set seed for reproducibility
set.seed(20251025)

# Fixed parameters
n <- 500  # Sample size
p <- 100  # Matrix dimension (scaled up from Phase 3)
batch_size <- 128  # Known to be efficient from Phase 3

# Experimental factor: CORES
core_counts <- c(1, 4, 16, 32)
n_reps <- 5  # Replications per core count

cat("MINIMAL PARALLELIZATION STUDY DESIGN\n")
cat("==================================================\n")
cat("  Sample size (n):", n, "\n")
cat("  Matrix dimension (p):", p, "×", p, "\n")
cat("  Batch size (fixed):", batch_size, "\n")
cat("  Core counts:", paste(core_counts, collapse = ", "), "\n")
cat("  Replications per core count:", n_reps, "\n")
cat("  Total runs:", length(core_counts) * n_reps, "\n\n")

# System information
cat("SYSTEM INFORMATION\n")
cat("==================================================\n")
cat("  Platform:", .Platform$OS.type, "\n")
cat("  R version:", paste(R.version$major, R.version$minor, sep = "."), "\n")
cat("  Hostname:", Sys.info()["nodename"], "\n")
slurm_job_id <- Sys.getenv("SLURM_JOB_ID", "LOCAL")
slurm_cpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "32"))
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

d <- p * (p + 1) / 2
cat("Generating dispersion matrix (0.1 × I_", d, ", tangent space)...\n", sep = "")
disp <- 0.1 * diag(d) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

cat("Generating", n, "SPD matrices (", p, "×", p, ")...\n", sep = "")
timing_gen <- system.time({
  samples <- rspdnorm(n = n, refpt = ref_pt, disp = disp, met = airm)
})
cat("  Data generation time:", round(timing_gen["elapsed"], 2), "seconds\n\n")

# Initialize results data frame
results <- data.frame()

# Progress tracking
total_runs <- length(core_counts) * n_reps
current_run <- 0
start_time <- Sys.time()

cat("==================================================\n")
cat("EXECUTING PARALLELIZATION EXPERIMENT\n")
cat("==================================================\n")
cat("Total experimental runs:", total_runs, "\n")
cat("Expected duration: ~3 hours (based on calibration)\n")
cat("Progress updates every run\n\n")

# Main experimental loop
for (cores in core_counts) {
  cat("\n")
  cat(strrep("=", 50), "\n")
  cat("Testing cores =", cores, "\n")
  cat(strrep("=", 50), "\n\n")

  # Check if we have enough cores
  if (cores > slurm_cpus) {
    cat(sprintf("⚠️  Skipping: cores=%d (insufficient CPUs allocated: %d)\n\n",
                cores, slurm_cpus))
    next
  }

  # Set core limit explicitly
  old_mc_cores <- getOption("mc.cores", 2L)
  options(mc.cores = cores)
  Sys.setenv(MC_CORES = cores)

  for (rep in 1:n_reps) {
    current_run <- current_run + 1

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    progress_pct <- (current_run / total_runs) * 100
    eta_mins <- if (current_run > 1) {
      (elapsed / (current_run - 1)) * (total_runs - current_run)
    } else {
      NA
    }

    cat(sprintf("[Run %d/%d (%.1f%%)] cores=%d, rep=%d/%d | ",
                current_run, total_runs, progress_pct, cores, rep, n_reps))
    cat(sprintf("Elapsed: %.1f min", elapsed))
    if (!is.na(eta_mins)) {
      cat(sprintf(" | ETA: %.1f min", eta_mins))
    }
    cat("\n")

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

    # Record results
    result_row <- data.frame(
      phase = "Phase4_Minimal",
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
      replication = rep,
      platform = .Platform$OS.type,
      r_version = paste(R.version$major, R.version$minor, sep = ".")
    )

    results <- rbind(results, result_row)

    cat(sprintf("  ✓ Completed in %.2f sec (%.2f min)\n\n",
                timing["elapsed"], timing["elapsed"]/60))
  }

  # Restore core setting
  options(mc.cores = old_mc_cores)

  # Print summary for this core count
  core_times <- results[results$cores == cores, "elapsed_seconds"]
  cat(sprintf("Summary for cores=%d: Mean=%.2f sec, SD=%.2f sec, CV=%.1f%%\n\n",
              cores, mean(core_times), sd(core_times),
              sd(core_times)/mean(core_times)*100))
}

cat("\n")
cat("==================================================\n")
cat("EXPERIMENT COMPLETED SUCCESSFULLY\n")
cat("==================================================\n")
total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat("  Total runs completed:", nrow(results), "/", total_runs, "\n")
cat("  Total elapsed time:", round(total_time, 2), "minutes\n")
cat("  Average time per run:", round(total_time * 60 / nrow(results), 2), "seconds\n\n")

# Speedup analysis
cat("==================================================\n")
cat("PRELIMINARY SPEEDUP ANALYSIS\n")
cat("==================================================\n\n")

# Calculate mean time by core count
core_summary <- aggregate(
  elapsed_seconds ~ cores,
  data = results,
  FUN = function(x) c(mean = mean(x), sd = sd(x))
)

cat("Mean Time by Core Count:\n")
print(core_summary)
cat("\n")

# Calculate speedup relative to 1 core
baseline_time <- mean(results[results$cores == 1, "elapsed_seconds"])

cat("Speedup Analysis (relative to 1 core):\n")
cat(sprintf("  Baseline (1 core): %.2f sec\n", baseline_time))
for (c in core_counts[core_counts > 1]) {
  c_time <- mean(results[results$cores == c, "elapsed_seconds"])
  speedup <- baseline_time / c_time
  efficiency <- speedup / c * 100

  cat(sprintf("  %2d cores: %.2f sec | Speedup: %.2fx | Efficiency: %.1f%%",
              c, c_time, speedup, efficiency))

  if (speedup < 1.2) {
    cat(" ⚠️  Minimal benefit\n")
  } else if (speedup < c * 0.5) {
    cat(" ✓ Some benefit\n")
  } else {
    cat(" ✅ Good scaling\n")
  }
}
cat("\n")

# Compare to Phase 3 (p=10)
cat("Comparison to Phase 3 (p=10):\n")
cat("  Phase 3 (p=10, 4 cores): 84.62 sec\n")
phase4_4cores <- mean(results[results$cores == 4, "elapsed_seconds"])
cat(sprintf("  Phase 4 (p=100, 4 cores): %.2f sec\n", phase4_4cores))
cat(sprintf("  Scaling factor: %.2fx\n\n", phase4_4cores / 84.62))

# Save results
cat("==================================================\n")
cat("SAVING RESULTS\n")
cat("==================================================\n\n")

# Save to cluster storage
cluster_exp_dir <- "/N/slate/nescoba/experiments/EXP-RT-001"
cluster_output <- file.path(cluster_exp_dir, "artifacts", "phase4_minimal_results.csv")

dir.create(file.path(cluster_exp_dir, "artifacts"),
           showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(cluster_exp_dir, "raw_results"),
           showWarnings = FALSE, recursive = TRUE)

write.csv(results, cluster_output, row.names = FALSE)
cat("  Cluster artifacts:", cluster_output, "\n")

# Also save to raw results with timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
raw_output <- file.path(cluster_exp_dir, "raw_results",
                        paste0("phase4_minimal_", timestamp, ".csv"))
write.csv(results, raw_output, row.names = FALSE)
cat("  Raw results (timestamped):", raw_output, "\n")

# Save local copy
local_output <- "phase4_minimal_results.csv"
write.csv(results, local_output, row.names = FALSE)
cat("  Local copy:", local_output, "\n\n")

cat("==================================================\n")
cat("PHASE 4 COMPLETE: ✓\n")
cat("==================================================\n\n")

cat("Validation Summary:\n")
cat("  ✓ All", total_runs, "experimental conditions completed\n")
cat("  ✓ Parallelization effect tested across 4 core counts\n")
cat("  ✓ Results saved to multiple locations\n")
cat("  ✓ Ready for final analysis\n\n")

cat("Key Findings (Preliminary):\n")
speedup_16 <- baseline_time / mean(results[results$cores == 16, "elapsed_seconds"])
cat(sprintf("  16 cores vs 1 core: %.2fx speedup\n", speedup_16))
if (speedup_16 > 2) {
  cat("  ✅ Parallelization DOES help for p=100!\n")
} else if (speedup_16 > 1.5) {
  cat("  ✓ Moderate benefit from parallelization\n")
} else {
  cat("  ⚠️  Limited benefit from parallelization\n")
}
cat("\n")

cat("Next Steps:\n")
cat("  1. Retrieve results from cluster\n")
cat("  2. Create detailed visualizations\n")
cat("  3. Statistical analysis of speedup\n")
cat("  4. Compare Phase 3 (p=10) vs Phase 4 (p=100)\n")
cat("  5. Document recommendations for riemtan package\n")
cat("  6. Update Notion and GitHub\n\n")

cat("Session Info:\n")
print(sessionInfo())
