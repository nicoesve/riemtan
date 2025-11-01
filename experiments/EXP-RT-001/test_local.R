# EXP-RT-001 Phase 2: Local Subset Test
# Purpose: Validate measurement approach with subset of conditions
# Date: 2025-10-24
# Agent: Toby Esterhase

# Load required packages
library(Matrix)
library(riemtan)
library(ggplot2)

cat("==========================================\n")
cat("EXP-RT-001 Phase 2: Local Subset Test\n")
cat("==========================================\n\n")

# Set seed for reproducibility
set.seed(20251024)

# Parameters
n <- 500  # Sample size
p <- 10   # Matrix dimension
batch_sizes <- c(16, 64, 256)  # Test 3 batch sizes
n_reps <- 3  # Replications per condition

cat("Experimental Design:\n")
cat("  Sample size (n):", n, "\n")
cat("  Matrix dimension (p):", p, "\n")
cat("  Batch sizes:", paste(batch_sizes, collapse = ", "), "\n")
cat("  Replications per condition:", n_reps, "\n")
cat("  Total conditions:", length(batch_sizes) * n_reps, "\n")
cat("  Platform:", .Platform$OS.type, "(sequential processing)\n\n")

# Load AIRM metric
data(airm)
cat("Metric: AIRM\n\n")

# Create reference point and dispersion matrix
cat("Generating reference point and dispersion matrix...\n")
ref_pt <- diag(p) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

# Dispersion in tangent space: d = p*(p+1)/2
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
cat("========================================\n")
cat("Running Experiments\n")
cat("========================================\n\n")

total_runs <- length(batch_sizes) * n_reps
current_run <- 0

for (bs in batch_sizes) {
  cat("\nBatch size:", bs, "\n")
  cat(strrep("-", 40), "\n")

  for (rep in 1:n_reps) {
    current_run <- current_run + 1
    cat(sprintf("  Rep %d/%d... ", rep, n_reps))

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
      phase = "Phase2_Local",
      date = Sys.Date(),
      n = n,
      p = p,
      batch_size = bs,
      cores = 1,  # Sequential on Windows
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
}

cat("\n========================================\n")
cat("Summary Statistics\n")
cat("========================================\n\n")

# Compute summary statistics by batch size
summary_stats <- aggregate(
  elapsed_seconds ~ batch_size,
  data = results,
  FUN = function(x) {
    c(
      mean = mean(x),
      sd = sd(x),
      cv = sd(x) / mean(x) * 100,
      min = min(x),
      max = max(x)
    )
  }
)

cat("Elapsed Time by Batch Size:\n")
print(summary_stats)
cat("\n")

# Save detailed results
output_file <- "artifacts/local_results.csv"
write.csv(results, output_file, row.names = FALSE)
cat("Detailed results saved to:", output_file, "\n\n")

# Create visualization
cat("Creating diagnostic plot...\n")

# Summary stats for plotting
summary_df <- do.call(data.frame, aggregate(
  elapsed_seconds ~ batch_size,
  data = results,
  FUN = function(x) c(mean = mean(x), sd = sd(x))
))
names(summary_df) <- c("batch_size", "mean", "sd")

# Bar plot with error bars
plot_file <- "artifacts/plots/local_batch_comparison.pdf"
pdf(plot_file, width = 8, height = 6)

par(mar = c(5, 5, 3, 2))
bp <- barplot(
  summary_df$mean,
  names.arg = summary_df$batch_size,
  ylim = c(0, max(summary_df$mean + summary_df$sd) * 1.2),
  xlab = "Batch Size",
  ylab = "Computation Time (seconds)",
  main = "EXP-RT-001 Phase 2: Batch Size Effect (Local, Sequential)",
  col = "steelblue",
  border = "darkblue"
)

# Add error bars (standard deviation)
arrows(
  x0 = bp, y0 = summary_df$mean - summary_df$sd,
  x1 = bp, y1 = summary_df$mean + summary_df$sd,
  angle = 90, code = 3, length = 0.1, lwd = 2
)

# Add sample size points
points(
  x = rep(bp, each = n_reps),
  y = results$elapsed_seconds,
  pch = 19, col = rgb(0, 0, 0, 0.3), cex = 1.2
)

# Add legend
legend(
  "topright",
  legend = c("Mean", "±SD", "Individual runs"),
  pch = c(15, NA, 19),
  lty = c(NA, 1, NA),
  col = c("steelblue", "black", rgb(0, 0, 0, 0.3)),
  bty = "n"
)

# Add text annotations
text(
  x = bp,
  y = summary_df$mean + summary_df$sd + max(summary_df$mean) * 0.05,
  labels = sprintf("%.2f s", summary_df$mean),
  cex = 0.9
)

dev.off()
cat("Plot saved to:", plot_file, "\n\n")

cat("========================================\n")
cat("Phase 2 Complete: ✓\n")
cat("========================================\n")
cat("\nValidation Checks:\n")
cat("  ✓ All", total_runs, "conditions completed\n")
cat("  ✓ Timing measurements recorded\n")
cat("  ✓ Results saved to CSV\n")
cat("  ✓ Diagnostic plot created\n\n")

cat("Observations:\n")
cat("  Note: Sequential processing on Windows\n")
cat("  Batch size effect:", {
  if (max(summary_df$mean) / min(summary_df$mean) > 1.1) {
    "Observable difference detected"
  } else {
    "Minimal difference (as expected for sequential)"
  }
}, "\n\n")

cat("Next step: Proceed to Phase 3 (cluster infrastructure test)\n")
cat("  - Scripts: script_cluster_test.R, run_cluster_test.slurm\n")
cat("  - This will test parallel processing effects\n")
