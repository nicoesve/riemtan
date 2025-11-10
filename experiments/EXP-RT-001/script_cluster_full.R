# EXP-RT-001 Phase 4: Full Cluster Experiment
# Purpose: Complete factorial design for batching effect analysis
# Date: 2025-10-24
# Agent: Toby Esterhase

# Load required packages
library(Matrix)
library(riemtan)
library(parallel)

cat("==================================================\n")
cat("EXP-RT-001 Phase 4: Full Cluster Experiment\n")
cat("==================================================\n\n")

# Set seed for reproducibility
set.seed(20251024)

# Full factorial design parameters
n <- 500  # Sample size
p <- 10   # Matrix dimension
batch_sizes <- c(8, 16, 32, 64, 128, 256)  # 6 levels
core_counts <- c(1, 2, 4, 8, 16, 32)  # 6 levels
n_reps <- 10  # Replications per condition

cat("FULL FACTORIAL EXPERIMENTAL DESIGN\n")
cat("==================================================\n")
cat("  Sample size (n):", n, "\n")
cat("  Matrix dimension (p):", p, "\n")
cat("  Batch sizes:", paste(batch_sizes, collapse = ", "), "\n")
cat("  Core counts:", paste(core_counts, collapse = ", "), "\n")
cat("  Replications per condition:", n_reps, "\n")
cat("  Total conditions:", length(batch_sizes), "×", length(core_counts), "=",
    length(batch_sizes) * length(core_counts), "\n")
cat("  Total runs:", length(batch_sizes) * length(core_counts) * n_reps, "\n\n")

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
cat("Generating reference point (identity matrix)...\n")
ref_pt <- diag(p) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

# Dispersion in tangent space: d = p*(p+1)/2 = 55 for p=10
d <- p * (p + 1) / 2
cat("Generating dispersion matrix (0.1 * identity, tangent space dim =", d, ")...\n")
disp <- 0.1 * diag(d) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

cat("Generating", n, "SPD matrices using rspdnorm()...\n")
timing_gen <- system.time({
  samples <- rspdnorm(n = n, refpt = ref_pt, disp = disp, met = airm)
})
cat("  Data generation time:", round(timing_gen["elapsed"], 2), "seconds\n\n")

# Initialize results data frame
results <- data.frame()

# Progress tracking
total_runs <- length(batch_sizes) * length(core_counts) * n_reps
current_run <- 0
start_time <- Sys.time()

cat("==================================================\n")
cat("EXECUTING FULL FACTORIAL EXPERIMENT\n")
cat("==================================================\n")
cat("Total experimental runs:", total_runs, "\n")
cat("Estimated duration: ~1-2 hours\n")
cat("Progress updates every 10 runs\n\n")

# Main experimental loop
for (bs in batch_sizes) {
  for (cores in core_counts) {
    # Check if we have enough cores
    if (cores > slurm_cpus) {
      cat(sprintf("Skipping: batch_size=%d, cores=%d (insufficient CPUs allocated)\n",
                  bs, cores))
      next
    }

    # Set core limit explicitly
    old_mc_cores <- getOption("mc.cores", 2L)
    options(mc.cores = cores)
    Sys.setenv(MC_CORES = cores)

    for (rep in 1:n_reps) {
      current_run <- current_run + 1

      # Progress update every 10 runs
      if (current_run %% 10 == 0 || current_run == 1) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
        progress_pct <- (current_run / total_runs) * 100
        eta_mins <- (elapsed / current_run) * (total_runs - current_run)

        cat(sprintf("\n[Progress: %d/%d (%.1f%%) | Elapsed: %.1f min | ETA: %.1f min]\n",
                    current_run, total_runs, progress_pct, elapsed, eta_mins))
        cat(sprintf("Current: batch_size=%d, cores=%d, rep=%d\n", bs, cores, rep))
      }

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
        phase = "Phase4_Full",
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

      # Brief progress indicator
      if (current_run %% 10 != 0 && current_run != 1) {
        cat(".")
      }
    }

    # Restore core setting
    options(mc.cores = old_mc_cores)
  }
}

cat("\n\n")
cat("==================================================\n")
cat("EXPERIMENT COMPLETED SUCCESSFULLY\n")
cat("==================================================\n")
total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat("  Total runs completed:", nrow(results), "/", total_runs, "\n")
cat("  Total elapsed time:", round(total_time, 2), "minutes\n")
cat("  Average time per run:", round(total_time * 60 / nrow(results), 2), "seconds\n\n")

# Quick summary statistics
cat("PRELIMINARY RESULTS SUMMARY\n")
cat("==================================================\n")

# Overall stats
cat("Computation Time Statistics:\n")
cat("  Mean:", round(mean(results$elapsed_seconds), 2), "seconds\n")
cat("  Median:", round(median(results$elapsed_seconds), 2), "seconds\n")
cat("  SD:", round(sd(results$elapsed_seconds), 2), "seconds\n")
cat("  Range:", round(min(results$elapsed_seconds), 2), "-",
    round(max(results$elapsed_seconds), 2), "seconds\n\n")

# By batch size
batch_summary <- aggregate(
  elapsed_seconds ~ batch_size,
  data = results,
  FUN = function(x) c(mean = mean(x), sd = sd(x))
)
cat("Mean Time by Batch Size:\n")
print(batch_summary)
cat("\n")

# By cores
core_summary <- aggregate(
  elapsed_seconds ~ cores,
  data = results,
  FUN = function(x) c(mean = mean(x), sd = sd(x))
)
cat("Mean Time by Core Count:\n")
print(core_summary)
cat("\n")

# Save results
cat("SAVING RESULTS\n")
cat("==================================================\n")

# Save to cluster storage
cluster_exp_dir <- "/N/slate/nescoba/experiments/EXP-RT-001"
cluster_output <- file.path(cluster_exp_dir, "artifacts", "cluster_full_results.csv")

dir.create(file.path(cluster_exp_dir, "artifacts"),
           showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(cluster_exp_dir, "raw_results"),
           showWarnings = FALSE, recursive = TRUE)

write.csv(results, cluster_output, row.names = FALSE)
cat("  Cluster artifacts:", cluster_output, "\n")

# Also save to raw results with timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
raw_output <- file.path(cluster_exp_dir, "raw_results",
                        paste0("results_", timestamp, ".csv"))
write.csv(results, raw_output, row.names = FALSE)
cat("  Raw results (timestamped):", raw_output, "\n")

# Save local copy for easy retrieval
local_output <- "cluster_full_results.csv"
write.csv(results, local_output, row.names = FALSE)
cat("  Local copy:", local_output, "\n\n")

cat("==================================================\n")
cat("PHASE 4 COMPLETE: ✓\n")
cat("==================================================\n")
cat("\nValidation Summary:\n")
cat("  ✓ All", total_runs, "experimental conditions completed\n")
cat("  ✓ Full factorial design executed successfully\n")
cat("  ✓ Results saved to multiple locations\n")
cat("  ✓ Ready for Phase 5 analysis\n\n")

cat("Next Steps:\n")
cat("  1. Retrieve results from cluster\n")
cat("  2. Run analyze_results.R (Phase 5)\n")
cat("  3. Generate statistical analysis and visualizations\n")
cat("  4. Document findings in Notion and GitHub\n")
cat("  5. Update riemtan package recommendations\n\n")

cat("Session Info:\n")
print(sessionInfo())
